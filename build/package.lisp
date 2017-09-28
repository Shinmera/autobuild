#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:autobuild-build
  (:nicknames #:org.shirakumo.autobuild.build)
  (:use #:cl)
  ;; build.lisp
  (:export
   #:*build*
   #:list-builds
   #:build
   #:status
   #:recipe
   #:commit
   #:arguments
   #:location
   #:current-stage
   #:metrics
   #:thread
   #:execute
   #:run-stage
   #:start
   #:cancel
   #:destroy
   #:argument)
  ;; events.lisp
  (:export
   #:build-event
   #:build
   #:build-output
   #:output
   #:build-started
   #:build-completed
   #:build-cancelled
   #:build-failed
   #:recipe-event
   #:recipe
   #:build-recipe
   #:cancel-build
   #:destroy-build)
  ;; line-stream.lisp
  (:export
   #:line-stream
   #:on-line)
  ;; recipe.lisp
  (:export
   #:ensure-recipe
   #:find-recipe
   #:remove-recipe
   #:list-recipes
   #:recipe
   #:repository
   #:stages)
  ;; stage.lisp
  (:export
   #:stage
   #:name
   #:dependencies
   #:execute
   #:compute-plan
   #:announce-stage
   #:function-stage
   #:func))
