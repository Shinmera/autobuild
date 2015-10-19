#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *build-output*)

(defclass build (repository task)
  ((logfile :initarg :logfile :accessor logfile)
   (project :initarg :project :accessor project)
   (restore-behaviour :initarg :restore :accessor restore-behaviour)
   (start :initform NIL :accessor start)
   (end :initform NIL :accessor end)
   (prev-timestamp :initform NIL :accessor prev-timestamp)
   (pre-build-func :initarg :pre-build :accessor pre-build-func :script T)
   (build-func :initarg :build :accessor build-func :script T)
   (post-build-func :initarg :post-build :accessor post-build-func :script T)
   (clean-func :initarg :clean :accessor clean-func :script T))
  (:default-initargs
   :logfile "autobuild.log"
   :project NIL
   :restore :never)
  (:metaclass autobuild-script:script-class))

(defmethod initialize-instance :after ((build build) &rest initargs)
  (when (and (project build) (not (location build)))
    (setf (location build) (location (project build))))
  (setf (logfile build) (merge-pathnames (logfile build) (location build)))
  (setf (status build) (discover-status-from-logfile (logfile build)))
  (ecase (restore-behaviour build)
    (:never)
    (:always
     (restore build NIL))
    (:if-newer
     (let* ((file (discover-recipe build))
            (script (when file (autobuild-script:read-script file :if-does-not-exist NIL)))
            (type (getf script :type)))
       (remf script :type)
       ;; Filter explicitly passed args so we don't overwrite them.
       (loop for (key val) on script by #'cddr
             do (when (getf initargs key)
                  (remf script val)))
       (when script
         (if type
             (apply #'change-class build type script)
             (apply #'reinitialize-instance build script)))))))

(defmethod print-object ((build build) stream)
  (print-unreadable-object (build stream :type T)
    (format stream "~s ~s ~s" (status build) :commit (current-commit build))))

(defmethod run-task ((build build))
  (unwind-protect
       (perform-build build)
    ;; Reset runner to allow rescheduling
    (setf (runner build) NIL)))

(defmethod task-ready-p ((build build))
  (case (status build)
    ((:running :stopping) NIL)
    ((:stopped :completed :errored :created :scheduled) T)))

(defun discover-status-from-logfile (logfile)
  ;; FIXME: discover start/end
  (if (not (probe-file logfile))
      :created
      (with-open-file (stream logfile :direction :input)
        (loop for line = (read-line stream NIL NIL)
              while line
              do (when (search ";;;; Autobuild" line)
                   (cond ((search "ERRORED" line)
                          (return :errored))
                         ((search "COMPLETED" line)
                          (return :completed))))
              finally (return :stopped)))))

(defgeneric perform-build (build))

(defun handle-build-start (build)
  (setf (start build) (get-universal-time))
  (setf (end build) NIL)
  (setf (status build) :running)
  (format *build-output* "~&;;;; Autobuild ~a" (status build))
  (format *build-output* "~&;; Started on ~a (~a)~%" (format-date (start build)) (start build))
  (finish-output *build-output*))

(defun print-build-end (build)
  (format *build-output* "~&;; Ended on ~a (~a)" (format-date (end build)) (end build))
  (format *build-output* "~&;; Build took ~a" (format-time (- (end build) (start build))))
  (format *build-output* "~&;;;; Autobuild ~a" (status build))
  (finish-output *build-output*))

(defun handle-build-error (build err)
  (setf (end build) (get-universal-time))
  (setf (status build) :errored)
  (format *build-output* "~&;; !! ERROR DURING BUILD~&")
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
                      ;; Clean out possible changes from a previous build or something.
                      (clean build)
                      ;; Perhaps we need to reload the configuration from file.
                      ;; Last chance, so do it now.
                      (maybe-restore build)
                      ;; Pass over into real build methods.
                      (call-next-method))
                  (simple-tasks:stop ()
                    (handle-build-stopped build)
                    (invoke-restart 'simple-tasks:stop)))
              (handle-build-complete build))))))))

(defmethod perform-build :before ((build build))
  (when (pre-build-func build)
    (funcall (pre-build-func build))))

(defmethod perform-build ((build build))
  (funcall (build-func build)))

(defmethod perform-build :after ((build build))
  (when (post-build-func build)
    (funcall (post-build-func build))))

(defgeneric duration (build)
  (:method ((build build))
    (when (start build)
      (- (or (end build) (get-universal-time)) (start build)))))

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
  (restore build (autobuild-script:read-script file :if-does-not-exist NIL)))

(defmethod restore ((build build) (script string))
  (let ((file (discover-recipe build :default T)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (write-string script stream)))
  (restore build (autobuild-script:read-script script)))

(defmethod restore ((build build) (data list))
  (let ((type (getf data :type)))
    (when data
      (remf data :type)
      (prog1
          (if (or (not type) (eql (type-of build) type))
              (apply #'reinitialize-instance build data)
              (apply #'change-class build type data))
        (setf (prev-timestamp build) (get-universal-time))))))

(defmethod clean ((build build) &key)
  (when (clean-func build)
    (funcall (clean-func build))))

(defmethod destroy :before ((build build))
  (when (eql :running (status build))
    (interrupt-task build NIL)))

(defmethod destroy :after ((build build))
  ;; Signify that we don't really exist anymore.
  (setf (location build) NIL)
  (setf (builds (project build))
        (delete build (builds (project build)))))

(defclass invalid-build (build)
  ()
  (:metaclass autobuild-script:script-class))

(defmethod perform-build ((build invalid-build))
  (error "INVALID BUILD! Please specify a proper build type for your project."))

(defclass make-build (build)
  ((target :initarg :target :initform NIL :accessor target))
  (:metaclass autobuild-script:script-class))

(defmethod perform-build ((build make-build))
  (run "make" (when (target build) (list (target build)))
       :output *build-output* :error *build-output* :on-non-zero-exit :error))

(defmethod clean ((build make-build) &key)
  (if (clean-func build)
      (funcall (clean-func build))
      (run "make" (list "clean")
           :output *build-output* :error *build-output* :on-non-zero-exit :error)))

(defclass asdf-build (build)
  ((system :initarg :system :initform NIL :accessor system)
   (operation :initarg :operation :initform :load-op :accessor operation))
  (:metaclass autobuild-script:script-class))

(defun initialize-asdf-build (build)
  (unless (system build)
    (if (project build)
        (setf (system build) (name (project build)))
        (setf (system build) (parse-directory-name (location build))))))

(defmethod initialize-instance :after ((build asdf-build) &key)
  (initialize-asdf-build build))

(defmethod reinitialize-instance :after ((build asdf-build) &key)
  (initialize-asdf-build build))

(defmethod perform-build ((build asdf-build))
  (run "sbcl" (list "--disable-debugger"
                    "--eval"
                    (format NIL "(push ~s asdf:*central-registry*)" (location build))
                    "--eval"
                    (format NIL "(asdf:operate ~s (asdf:find-system ~s T) :verbose T :force T)"
                            (operation build) (system build))
                    "--eval"
                    (format NIL "(sb-ext:exit)"))
       :output *build-output* :error *build-output* :on-non-zero-exit :error))

(defclass function-build (build)
  ((func :initarg :func :accessor func))
  (:default-initargs
   :func (error "FUNC required."))
  (:metaclass autobuild-script:script-class))

(defmethod perform-build ((build function-build))
  (funcall (func build) build))

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
