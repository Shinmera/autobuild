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

(defclass recipe (autobuild-build:recipe)
  ((build :initform NIL :accessor build)))

(defclass build (autobuild-build:build)
  ((recipe :initarg :recipe :accessor recipe))
  (:default-initargs
   :recipe (error "RECIPE required.")))

(defmethod initialize-instance :after ((build build) &key recipe)
  (setf (plan build) (autobuild-build:compute-plan recipe))
  (setf (gethash recipe *builds*) build))

(deeds:define-event build-event ()
  ((build :initarg :build :reader build))
  (:default-initargs
   :build (error "BUILD required.")))

(deeds:define-event build-output (build-event)
  ((output :initarg :output :reader output))
  (:default-initargs
   :output (error "OUTPUT required.")))

(defmethod execute :around ((build build))
  (flet ((w (line)
           (deeds:do-issue build-output :output line)))
    (let ((*standard-output* (make-instance 'line-stream :on-line #'w)))
      (call-next-method))))

(deeds:define-event build-started (build-event)
  ())

(deeds:define-event build-completed (build-event)
  ())

(deeds:define-event build-cancelled (build-event)
  ())

(deeds:define-event build-failed (build-event)
  ())

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

(deeds:define-event recipe-event ()
  ((recipe :initarg :recipe :reader recipe))
  (:default-initargs
   :recipe (error "RECIPE required.")))

(deeds:define-command build-recipe (recipe)
  :superclasses (recipe-event)
  :class 'deeds:locally-blocking-handler
  (cond ((build recipe)
         (v:warn "Cannot launch a build for a recipe: already building." recipe))
        (T
         (let ((build (make-instance 'build :recipe recipe)))
           (setf (build recipe) build)
           (bt:with-lock-held (*builds-lock*)
             (push build *builds*))
           (autobuild-build:start build)))))

(deeds:define-command cancel-recipe (recipe)
  :superclasses (recipe-event)
  :class 'deeds:locally-blocking-handler
  (if (build recipe)
      (autobuild-build:cancel (build recipe))
      (v:warn "Cannot cancel the recipe ~a: no build." recipe)))
