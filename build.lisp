#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *build-output*)

(defclass build (repository)
  ((logfile :initarg :logfile :accessor logfile)
   (status :initform :none :accessor status))
  (:default-initargs
   :logfile "autobuild.log"))

(defgeneric perform-build (project))

(defmethod perform-build :around ((build build))
  (with-simple-restart (abort "Abort the build of ~a" build)
    (with-chdir (build)
      (with-open-file (log-out (merge-pathnames (logfile build) (location build))
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
        (let ((*build-output* (make-broadcast-stream *standard-output* log-out)))
          (setf (status build) :running)
          (format log-out ";;;; Autobuild~%")
          (format log-out ";; Started on ~a~%~%" (format-date))
          (finish-output log-out)
          (let ((start-time (get-internal-real-time)))
            (handler-bind ((error (lambda (err)
                                    (format log-out "~&~%~%;; !! ERROR DURING BUILD:~%")
                                    (dissect:present err log-out)
                                    (format log-out "~&~%~%")
                                    (finish-output log-out)
                                    (setf (status build) :errored))))
              (call-next-method))
            (format log-out "~&~%")
            (format log-out ";; Ended on ~a~%" (format-date))
            (format log-out ";; Build took ~a" (format-time (/ (- (get-internal-real-time)
                                                                  start-time)
                                                               INTERNAL-TIME-UNITS-PER-SECOND)))
            (finish-output log-out)
            (setf (status build) :completed)))))))

(defclass make-build (build)
  ())

(defmethod perform-build ((build make-build))
  (run "make" () :output *build-output* :error *build-output*))
