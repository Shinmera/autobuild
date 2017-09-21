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
   #:build
   #:status
   #:plan
   #:current-stage
   #:metrics
   #:thread
   #:execute
   #:run-stage
   #:start
   #:cancel)
  ;; filesystem.lisp
  (:export
   #:*root*
   #:*repositories*
   #:recipe-location
   #:commit-location
   #:ensure-repository)
  ;; recipe.lisp
  (:export
   #:stage
   #:name
   #:dependencies
   #:execute
   #:compute-plan
   #:function-stage
   #:func
   #:finish-stage
   #:recipe))
