#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defclass build ()
  ((status :initform :created :accessor status)
   (plan :initarg :plan :accessor plan)
   (current-stage :initform NIL :accessor current-stage)
   (metrics :initform () :accessor metrics)
   (thread :initform NIL :accessor thread))
  (:default-initargs
   :plan ()))

(defmethod execute :before ((build build))
  (setf (status build) :started))

(defmethod execute :after ((build build))
  (setf (status build) :completed))

(defmethod execute :around ((build build))
  (let ((finished NIL))
    (unwind-protect
         (multiple-value-prog1
             (restart-case (call-next-method)
               (cancel-build ()
                 :report "Cancel the build."
                 (setf (status build) :cancelled)))
           (setf finished T))
      (unless finished
        (setf (status build) :failed)))))

(defmethod execute ((build build))
  (let ((simple-inferiors:*cwd* simple-inferiors:*cwd*))
    (dolist (stage (plan build))
      (run-stage stage build))))

(defmethod run-stage :before ((stage stage) (build build))
  (setf (current-stage build) stage))

(defmethod run-stage :after ((stage stage) (build build))
  (setf (current-stage build) NIL))

(defmethod run-stage ((stage stage) (build build))
  (let ((start-time (get-internal-real-time)))
    (execute stage)
    (setf (getf (gethash stage (metrics *build*)) :time)
          (/ (- (get-internal-real-time) start-time)
             INTERNAL-TIME-UNITS-PER-SECOND))))

(defmethod start ((build build))
  (unless (eql (status build) :created)
    (error "The build is already ~a." (status build)))
  (setf (thread build)
        (bt:make-thread (lambda ()
                          (unwind-protect (execute build)
                            (setf (thread build) NIL))))))

(defmethod cancel ((build build))
  (unless (eql (status build) :started)
    (error "The build is not currently running."))
  (if (eql (bt:current-thread) (thread build))
      (invoke-restart 'cancel-build)
      (bt:interrupt-thread (thread build) (lambda () (invoke-restart 'cancel-build)))))
