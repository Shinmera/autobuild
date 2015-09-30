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
   (end :initform NIL :accessor end))
  (:default-initargs
   :logfile "autobuild.log"
   :project NIL))

(defmethod initialize-instance :after ((build build) &key)
  (when (and (project build) (not (location build)))
    (setf (location build) (location (project build))))
  (setf (logfile build) (merge-pathnames (logfile build) (location build))))

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

(defgeneric perform-build (build))

(defmethod perform-build :around ((build build))
  (v:info :autobuild "Performing build for ~a" build)
  (with-simple-restart (abort "Abort the build of ~a" build)
    (with-chdir (build)
      (uiop:delete-file-if-exists (logfile build))
      (with-open-file (log-out (logfile build)
                               :direction :output
                               :if-does-not-exist :create)
        (let ((*build-output* (make-broadcast-stream *standard-output* log-out))
              (start-time (get-universal-time)))
          (setf (start build) start-time)
          (format log-out ";;;; Autobuild~%")
          (format log-out ";; Started on ~a~%~%" (format-date start-time))
          (finish-output log-out)
          (multiple-value-prog1
              (handler-bind ((error (lambda (err)
                                      (format log-out "~&~%~%;; !! ERROR DURING BUILD~%")
                                      (dissect:present err log-out)
                                      (format log-out "~&~%~%")
                                      (finish-output log-out)                                      
                                      (setf (start build) NIL)
                                      (v:log :err :autobuild err))))
                (call-next-method))
            (let ((end-time (get-universal-time)))
              (format log-out "~&~%")
              (format log-out ";; Ended on ~a~%" (format-date))
              (format log-out ";; Build took ~a" (format-time (- end-time start-time)))
              (finish-output log-out)
              (setf (end build) end-time)
              (v:info :autobuild "Build for ~a finished." build))))))))

(defgeneric duration (build)
  (:method ((build build))
    (when (start build)
      (- (or (end build) (get-universal-time)) (start build)))))

(defgeneric log-contents (build)
  (:method ((build build))
    (when (probe-file (logfile build))
      (alexandria:read-file-into-string (logfile build)))))

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
