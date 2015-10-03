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
   #:start
   #:end
   #:perform-build
   #:duration
   #:invalid-build
   #:make-build
   #:asdf-build
   #:log-contents)
  ;; project.lisp
  (:export
   #:*base-project-dir*
   #:project
   #:build-type
   #:builds
   #:name
   #:branch
   #:remote
   #:watch
   #:scan-for-builds
   #:build-dir
   #:coerce-build
   #:ensure-build
   #:ensure-current-build
   #:build
   #:status
   #:clean
   ;; Reexport repository API
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
   #:destroy))

(defpackage #:autobuild-script
  (:nicknames #:org.shirakumo.autobuild.script)
  (:use #:cl #:autobuild #:legit)
  (:export
   #:$
   #:->
   #:>>
   #:s/r
   #:read-script
   #:read-script-file))

(defpackage #:autobuild-script-user
  (:nicknames #:org.shirakumo.autobuild.script.user #:as-user)
  (:use #:cl #:legit #:autobuild #:autobuild-script))
