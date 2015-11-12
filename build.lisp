#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *build-output*)

(defclass build (repository timed-task runner)
  ((logfile :initarg :logfile :accessor logfile)
   (project :initarg :project :accessor project)
   (restore-behaviour :initarg :restore :accessor restore-behaviour)
   (stages :initarg :stages :accessor stages)
   (stage-order :initarg :stage-order :accessor stage-order)
   (prev-timestamp :initform NIL :accessor prev-timestamp)
   (current-stage :initform NIL :accessor current-stage))
  (:default-initargs
   :logfile "autobuild.log"
   :project NIL
   :restore :never
   :stages NIL
   :stage-order NIL))

(defmethod reinitialize-instance :after ((build build) &key (stages NIL s-p) &allow-other-keys)
  (when s-p (setf (stages build) stages)))

(defmethod update-instance-for-different-class :after (prev (build build) &key (stages NIL s-p) &allow-other-keys)
  (when s-p (setf (stages build) stages)))

(defmethod initialize-instance :after ((build build) &rest initargs &key stages)
  (declare (ignore stages))
  (when (and (project build) (not (location build)))
    (setf (location build) (location (project build))))
  (setf (logfile build) (merge-pathnames (logfile build) (location build)))
  (multiple-value-bind (status start end) (discover-state-from-logfile (logfile build))
    (setf (status build) status)
    (setf (start build) start)
    (setf (end build) end))
  (ecase (restore-behaviour build)
    (:never (when s-p (setf (stages build) stages)))
    (:always
     (restore build NIL))
    (:if-newer
     (let* ((file (discover-recipe build))
            (script (when file (read-script file :if-does-not-exist NIL)))
            (type (getf script :type)))
       (remf script :type)
       ;; Filter explicitly passed args so we don't overwrite them.
       (loop for (key val) on script by #'cddr
             do (when (getf initargs key)
                  (remf script val)))
       (cond (type (apply #'change-class build type script))
             (script (apply #'reinitialize-instance build script)))))))

(defmethod print-object ((build build) stream)
  (print-unreadable-object (build stream :type T)
    (format stream "~s ~s ~s" (status build) :commit (current-commit build))))

(defmethod (setf stages) (stages (build build))
  (setf (slot-value build 'stages)
        (loop for (key val) on stages by #'cddr
              collect key collect (typecase val
                                    (stage val)
                                    (T (make-instance 'stage :name key :script val))))))

(defmethod run-task ((build build))
  (perform-build build))

(defmethod task-ready-p ((build build))
  (case (status build)
    ((:running :stopping) NIL)
    ((:stopped :completed :errored :created :scheduled) T)))

(defun discover-state-from-logfile (logfile)
  (let ((status :created)
        (start NIL)
        (end NIL))
    (when (probe-file logfile)
      (with-open-file (stream logfile :direction :input)
        (loop for line = (read-line stream NIL NIL)
              while line
              do (or (cl-ppcre:register-groups-bind (name) ("^;{4,} Autobuild ([A-Z]+)" line)
                       (let ((symb (find-symbol name :keyword)))
                         (when (and symb (not (eql symb :running))) (setf status symb))))
                     (cl-ppcre:register-groups-bind (code) ("^;{2,} Started on .*? \\(([0-9]+)\\)" line)
                       (setf start (parse-integer code :junk-allowed T)))
                     (cl-ppcre:register-groups-bind (code) ("^;{2,} Ended on .*? \\(([0-9]+)\\)" line)
                       (setf end (parse-integer code :junk-allowed T))))
              finally (return :stopped))))
    (values status start end)))

(defgeneric perform-build (build))

(defun handle-build-start (build)
  (setf (start build) (get-universal-time))
  (setf (end build) T)
  (setf (status build) :running)
  (format *build-output* "~&;;;; Autobuild ~a" (status build))
  (format *build-output* "~&;;; Started on ~a (~a)~%" (format-date (start build)) (start build))
  (finish-output *build-output*))

(defun print-build-end (build)
  (format *build-output* "~&;;; Ended on ~a (~a)" (format-date (end build)) (end build))
  (format *build-output* "~&;;; Build took ~a" (format-time (- (end build) (start build))))
  (format *build-output* "~&;;;; Autobuild ~a" (status build))
  (finish-output *build-output*))

(defun handle-build-error (build err)
  (setf (end build) (get-universal-time))
  (setf (status build) :errored)
  (format *build-output* "~&;;; !! ERROR DURING BUILD~&")
  (dissect:present err *build-output*)
  (print-build-end build)
  (v:log :error :autobuild err))

(defun handle-build-complete (build)
  (setf (end build) (get-universal-time))
  (setf (status build) :completed)
  (print-build-end build)
  (v:info :autobuild "Build for ~a finished." build))

(defun handle-build-stopped (build)
  (setf (end build) (get-universal-time))
  (setf (status build) :stopped)
  (print-build-end build)
  (v:info :autobuild "Build for ~a stopped." build))

(defmethod perform-build :around ((build build))
  (when (location build)
    (v:info :autobuild "Performing build for ~a" build)
    (with-simple-restart (abort "Abort the build of ~a" build)
      (with-chdir (build)
        (uiop:delete-file-if-exists (logfile build))
        (with-open-file-no-remove (log-out (logfile build)
                                           :direction :output
                                           :if-does-not-exist :create)
          (let* ((*build-output* (make-broadcast-stream *standard-output* log-out))
                 (*standard-output* *build-output*))
            (handle-build-start build)
            (multiple-value-prog1
                (restart-case
                    (handler-bind ((error (lambda (err) (handle-build-error build err))))
                      ;; Perhaps we need to reload the configuration from file.
                      ;; Last chance, so do it now.
                      (maybe-restore build)
                      ;; Pass over into real build methods.
                      (call-next-method))
                  (simple-tasks:stop ()
                    (handle-build-stopped build)
                    (if (find-restart 'simple-tasks:stop)
                        (invoke-restart 'simple-tasks:stop)
                        (abort))))
              (handle-build-complete build))))))))

(defmethod perform-build ((build build))
  ;; First we iterate and find the stages to make sure about two things
  ;; 1) that the stages are all cached, as method stages need to be recreated
  ;; 2) that we don't run into "surprise!! no such stage!!" sometime down the build.
  (dolist (stage (stage-order build))
    (find-stage stage build))
  (dolist (stage (stage-order build))
    (perform-stage stage build)))

(defmethod schedule-task :around ((stage stage) (build build))
  (setf (current-stage build) stage)
  (unwind-protect
       (call-next-method)
    (setf (current-stage build) NIL)))

(defgeneric perform-stage (stage build)
  (:method (name (build build))
    (perform-stage (find-stage name build) build))
  (:method ((stage stage) (build build))
    (schedule-task stage build)))

(defgeneric find-stage (name build)
  (:method ((stage stage) build)
    stage)
  (:method (name (build build))
    (or (getf (stages build) name)
        ;; Find method and cache it.
        (and (find-method #'stage () `((eql ,name) ,(class-of build)) NIL)
             (setf (getf (stages build) name)
                   (make-instance 'stage :name name :script (lambda () (stage name build)))))
        (error "No stage named ~s found for build ~a." name build))))

(defgeneric stage (identifier build))

(defgeneric log-contents (build &optional file-position)
  (:method ((build build) &optional file-position)
    (when (probe-file (logfile build))
      (with-open-file (stream (logfile build) :direction :input)
        (when file-position (file-position stream file-position))
        (values (read-stream-to-string stream)
                (file-position stream))))))

(defgeneric discover-recipe (location &key default)
  (:method ((location T) &key default)
    (let ((pathname (make-pathname :name ".autobuild" :type "lisp" :defaults (location location))))
      (or (probe-file pathname) (and default pathname))))
  (:method ((build build) &key default)
    (or (call-next-method build :default NIL)
        (discover-recipe (project build))
        (when default
          (call-next-method build :default T)))))

(defgeneric recipe (build)
  (:method ((build build))
    (let ((recipe (discover-recipe build)))
      (when recipe (with-open-file (stream recipe :direction :input)
                     (read-stream-to-string stream))))))

(defgeneric maybe-restore (build)
  (:method ((build build))
    (case (restore-behaviour build)
      (:never)
      (:always
       (restore build NIL))
      (:if-newer
       (let ((file (discover-recipe build)))
         (when (and file
                    (or (not (prev-timestamp build))
                        (< (prev-timestamp build)
                           (file-write-date file))))
           (restore build file)))))))

(defmethod restore ((build build) (source null))
  (let ((file (discover-recipe build)))
    (when file
      (restore build file))))

(defmethod restore ((build build) (file pathname))
  (restore build (read-script file :if-does-not-exist NIL)))

(defmethod restore ((build build) (script string))
  (let ((file (discover-recipe build :default T)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (write-string script stream)))
  (restore build (read-script script)))

(defmethod restore ((build build) (data list))
  (let ((type (getf data :type)))
    (when data
      (remf data :type)
      (prog1
          (if (or (not type) (eql (type-of build) type))
              (apply #'reinitialize-instance build data)
              (apply #'change-class build type data))
        (setf (prev-timestamp build) (get-universal-time))))))

(defmethod destroy :before ((build build))
  (when (eql :running (status build))
    (interrupt-task build NIL)))

(defmethod destroy :after ((build build))
  ;; Signify that we don't really exist anymore.
  (setf (location build) NIL)
  (setf (builds (project build))
        (delete build (builds (project build)))))

(defgeneric coerce-build (thing &rest args)
  (:method ((script list) &rest args)
    (let ((script (copy-list script))
          (type (or (getf script :type) 'build)))
      (remf script :type)
      (apply #'make-instance type (append args script))))
  (:method ((name symbol) &rest args)
    (apply #'make-instance name args))
  (:method ((build build) &rest args)
    (declare (ignore args))
    build))
