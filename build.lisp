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
   (start :initform NIL :accessor start)
   (end :initform NIL :accessor end)
   (pre-build-func :initarg :pre-build :accessor pre-build-func)
   (build-func :initarg :build :accessor build-func)
   (post-build-func :initarg :post-build :accessor post-build-func))
  (:default-initargs
   :logfile "autobuild.log"
   :project NIL
   :pre-build NIL
   :build NIL
   :post-build NIL))

(defmethod initialize-instance :after ((build build) &key)
  (when (and (project build) (not (location build)))
    (setf (location build) (location (project build))))
  (setf (logfile build) (merge-pathnames (logfile build) (location build)))
  (setf (status build) (discover-status-from-logfile (logfile build)))
  (setf (pre-build-func build) (coerce-function (pre-build-func build)))
  (setf (build-func build) (coerce-function (build-func build)))
  (setf (post-build-func build) (coerce-function (post-build-func build))))

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
    ((:stopped :completed :errored :created) T)))

(defun discover-status-from-logfile (logfile)
  (if (not (probe-file logfile))
      :created
      (with-open-file (stream logfile :direction :input)
        (loop for line = (read-line stream NIL NIL)
              while line
              do (when (search ";;;; Autobuild" line)
                   (cond ((search "RUNNING" line))
                         ((search "ERRORED" line)
                          (return :errored))
                         ((search "COMPLETED" line)
                          (return :completed))))
              finally (return :stopped)))))

(defgeneric perform-build (build))

(defmethod perform-build :around ((build build))
  (when (location build)
    (v:info :autobuild "Performing build for ~a" build)
    (with-simple-restart (abort "Abort the build of ~a" build)
      (with-chdir (build)
        (uiop:delete-file-if-exists (logfile build))
        (with-open-file-no-remove (log-out (logfile build)
                                           :direction :output
                                           :if-does-not-exist :create)
          (let ((*build-output* (make-broadcast-stream *standard-output* log-out))
                (start-time (get-universal-time)))
            (setf (start build) start-time)
            (format log-out ";;;; Autobuild RUNNING~&")
            (format log-out ";; Started on ~a~&~%" (format-date start-time))
            (finish-output log-out)
            (multiple-value-prog1
                (handler-bind ((error (lambda (err)
                                        (format log-out "~&~%~%;; !! ERROR DURING BUILD~&")
                                        (dissect:present err log-out)
                                        (format log-out "~&~%~%")
                                        (format log-out ";;;; Autobuild ERRORED~&")
                                        (finish-output log-out)                                    
                                        (setf (start build) NIL)
                                        (v:log :error :autobuild err))))
                  (call-next-method))
              (let ((end-time (get-universal-time)))
                (format log-out "~&~%")
                (format log-out ";; Ended on ~a~&" (format-date))
                (format log-out ";; Build took ~a~&" (format-time (- end-time start-time)))
                (format log-out ";;;; Autobuild COMPLETED~&")
                (finish-output log-out)
                (setf (end build) end-time)
                (v:info :autobuild "Build for ~a finished." build)))))))))

(defmethod perform-build :before ((build build))
  (funcall (pre-build-func build)))

(defmethod perform-build ((build build))
  (funcall (build-func build)))

(defmethod perform-build :after ((build build))
  (funcall (post-build-func build)))

(defgeneric duration (build)
  (:method ((build build))
    (when (start build)
      (- (or (end build) (get-universal-time)) (start build)))))

(defgeneric log-contents (build)
  (:method ((build build))
    (when (probe-file (logfile build))
      (alexandria:read-file-into-string (logfile build)))))

(defmethod destroy :before ((build build))
  (when (eql :running (status build))
    (interrupt-task build NIL)))

(defmethod destroy :after ((build build))
  ;; Signify that we don't really exist anymore.
  (setf (location build) NIL)
  (setf (builds (project build))
        (delete build (builds (project build)))))

(defclass invalid-build (build)
  ())

(defmethod perform-build ((build invalid-build))
  (error "INVALID BUILD! Please specify a proper build type for your project."))

(defclass make-build (build)
  ())

(defmethod perform-build ((build make-build))
  (run "make" ()
       :output *build-output* :error *build-output* :on-non-zero-exit :error))

(defclass asdf-build (build)
  ((system :initarg :system :accessor system))
  (:default-initargs
   :system NIL))

(defmethod initialize-instance :after ((build asdf-build) &key)
  (unless (system build)
    (if (project build)
        (setf (system build) (name (project build)))
        (setf (system build) (parse-directory-name (location build))))))

(defmethod perform-build ((build asdf-build))
  (run "sbcl" (list "--disable-debugger"
                    "--eval"
                    (format NIL "(push ~s asdf:*central-registry*)" (location build))
                    "--eval"
                    (format NIL "(asdf:load-system ~s :verbose T :force T)" (system build))
                    "--eval"
                    (format NIL "(sb-ext:exit)"))
       :output *build-output* :error *build-output* :on-non-zero-exit :error))

(defclass function-build (build)
  ((func :initarg :func :accessor func))
  (:default-initargs
   :func (error "FUNC required.")))

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
