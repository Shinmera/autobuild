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

(deeds:define-command build-recipe (ev recipe commit location)
  :superclasses (recipe-event)
  :class 'deeds:locally-blocking-handler
  (start (make-instance 'build :recipe (ensure-recipe recipe)
                               :commit commit
                               :location location)))

(deeds:define-command cancel-build (ev build)
  :superclasses (build-event)
  :class 'deeds:locally-blocking-handler
  (cancel build))

(deeds:define-command destroy-build (ev build)
  :superclasses (build-event)
  :class 'deeds:locally-blocking-handler
  (destroy build))
