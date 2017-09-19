#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defvar *build*)

(defclass build ()
  ((status :initform :created :accessor status)
   (recipe :initarg :recipe :accessor recipe)
   (current-stage :initform NIL :accessor current-stage)
   (metrics :initform () :accessor metrics)
   (thread :initform NIL :accessor thread)))

(defmethod execute :before ((build build))
  (setf (status build) :running))

(defmethod execute :after ((build build))
  (setf (status build) :completed))

(defmethod execute :around ((build build))
  (let ((finished NIL)
        (*build* build))
    (unwind-protect
         (multiple-value-prog1
             (restart-case (call-next-method)
               (abort-build ()
                 :report "Abort the build."
                 (setf (status build) :aborted)))
           (setf finished T))
      (unless finished
        (setf (Status build) :failed)))))

(defmethod execute ((build build))
  (execute (recipe build)))

(defmethod execute :before ((stage stage))
  (setf (current-stage *build*) stage))

(defmethod execute :after ((stage stage))
  (setf (current-stage *build*) NIL))

(defmethod execute ((stage stage))
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

(defmethod abort ((build build))
  (unless (eql (status build) :running)
    (error "The build is not currently running."))
  (bt:interrupt-thread (thread build) (lambda () (invoke-restart 'abort-build))))
