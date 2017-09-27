#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(deeds:define-event build-event ()
  ((build :initarg :build :reader build))
  (:default-initargs
   :build (error "BUILD required.")))

(deeds:define-event build-output (build-event)
  ((output :initarg :output :reader output))
  (:default-initargs
   :output (error "OUTPUT required.")))

(deeds:define-event build-started (build-event)
  ())

(deeds:define-event build-completed (build-event)
  ())

(deeds:define-event build-cancelled (build-event)
  ())

(deeds:define-event build-failed (build-event)
  ())

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
         (autobuild-build:start (make-instance 'build :recipe recipe)))))

(deeds:define-command cancel-recipe (recipe)
  :superclasses (recipe-event)
  :class 'deeds:locally-blocking-handler
  (if (build recipe)
      (autobuild-build:cancel (build recipe))
      (v:warn "Cannot cancel the recipe ~a: no build." recipe)))
