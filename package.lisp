#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:autobuild
  (:nicknames #:org.shirakumo.autobuild)
  (:use #:cl #:legit)
  ;; autobuild.lisp
  (:export
   #:project
   #:remove-project
   #:make-build-project
   #:watcher
   #:project
   #:watcher-stream
   #:thread
   #:watcher
   #:remove-watcher
   #:start-watcher
   #:stop-watcher)
  ;; build.lisp
  (:export
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
   #:scan-for-builds
   #:build-dir
   #:coerce-build
   #:build
   #:status
   #:watch-project)
  ;; toolkit.lisp
  (:export))
