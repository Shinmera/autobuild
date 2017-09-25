#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.manager)

(defvar *builds* ())
(defvar *builds-lock* (bt:make-lock "builds-list lock"))

(defun list-builds ()
  (copy-list *builds*))

(defclass build (autobuild-build:build)
  ())

(defmethod initialize-instance :around ((build build) &rest initargs)
  (let ((initargs (copy-list initargs)))
    (setf (getf initargs :recipe) (etypecase (getf initargs :recipe)
                                    (recipe (getf initargs :recipe))
                                    (T (find-recipe (getf initargs :recipe) :error T))))
    (apply #'call-next-method build initargs)))

(defmethod initialize-instance :after ((build build) &key recipe)
  (setf (gethash recipe *builds*) build))

(defmethod (setf status) :before (status (build build))
  (v:info :autobuild.manager "~a changing status to ~a." build status))

(defmethod (setf status) :after (status (build build))
  (when (find status #(:completed :cancelled :failed))
    (setf (build (autobuild-build:recipe build)) NIL)
    (bt:with-lock-held (*builds-lock*)
      (setf *builds* (remove build *builds*))))
  (deeds:issue
   (make-instance
    (ecase status
      (:started 'build-started)
      (:completed 'build-completed)
      (:cancelled 'build-cancelled)
      (:failed 'build-failed))
    :build build)
   deeds:*standard-event-loop*))

(defmethod execute :around ((build build))
  (flet ((w (line)
           (deeds:do-issue build-output :output line)))
    (let ((*standard-output* (make-instance 'line-stream :on-line #'w)))
      (call-next-method))))

(defmethod run-stage :befor (stage (build build))
  (v:info :autobuild.manager "~a entering stage ~a."
          build (autobuild-build:name stage)))
