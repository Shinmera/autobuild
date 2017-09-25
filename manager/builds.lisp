#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.manager)

(defvar *builds* ())
(defvar *builds-lock* (bt:make-lock :name "builds-list lock"))

(defun list-builds ()
  (copy-list *builds*))

(defclass build (autobuild-build:build)
  ())

(defmethod initialize-instance :after ((build build) &key recipe)
  (setf (gethash recipe *builds*) build))

(defmethod (setf status) :after (status (build build))
  (when (find status #(:completed :cancelled :failed))
    (setf (build (recipe build)) NIL)
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
