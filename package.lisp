#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:autobuild
  (:nicknames #:org.shirakumo.autobuild)
  (:use #:cl #:legit #:simple-tasks)
  ;; autobuild.lisp
  (:export
   #:*projects*
   #:project
   #:remove-project
   #:make-build-project
   #:scan-for-projects
   #:watcher
   #:project
   #:builder
   #:output
   #:*builder*
   #:*watcher*
   #:watch-task
   #:timeout
   #:initialize-autobuild)
  ;; build.lisp
  (:export
   #:*build-output*
   #:build
   #:logfile
   #:status
   #:project
   #:stages
   #:stage-order
   #:current-stage
   #:perform-build
   #:perform-stage
   #:find-stage
   #:stage
   #:log-contents
   #:discover-recipe
   #:recipe
   #:coerce-build)
  ;; project.lisp
  (:export
   #:*base-project-dir*
   #:project
   #:builds
   #:name
   #:watch
   #:scan-for-builds
   #:build-dir
   #:ensure-build
   #:build
   #:status
   #:clean)
  ;; stage.lisp
  (:export
   #:timed-task
   #:start
   #:end
   #:duration
   #:stage
   #:name
   #:script)
  ;; standard-builds.lisp
  (:export
   #:invalid-build
   #:make-build
   #:target
   #:asdf-build
   #:system
   #:operation)
  ;; Reexport repository API
  (:export
   #:location
   #:clone
   #:pull
   #:checkout
   #:reset
   #:commits
   #:current-commit
   #:current-branch
   #:current-message
   #:remote-url)
  ;; toolkit.lisp
  (:export
   #:destroy
   #:restore))

(defpackage #:autobuild-script
  (:nicknames #:org.shirakumo.autobuild.script)
  (:use #:cl #:autobuild #:legit)
  (:export
   #:$
   #:->
   #:>>
   #:s/r
   #:read-script
   #:write-script
   #:coerce-script
   #:script-class))

(defpackage #:autobuild-script-user
  (:nicknames #:org.shirakumo.autobuild.script.user #:as-user)
  (:use #:cl #:legit #:autobuild #:autobuild-script))
