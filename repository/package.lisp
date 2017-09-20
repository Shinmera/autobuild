#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:autobuild-repository
  (:nicknames #:org.shirakumo.autobuild.repository)
  (:use #:cl)
  ;; interface.lisp
  (:export
   #:*repository-types*
   #:remote-ish
   #:permalink
   #:located
   #:location
   #:repository
   #:create
   #:update
   #:list-commits
   #:find-commit
   #:commit
   #:id
   #:author
   #:message
   #:timestamp
   #:checkout
   #:checkout
   #:clean
   #:destroy))

(defpackage #:autobuild-git-repository
  (:nicknames #:org.shirakumo.autobuild.repository.git)
  (:use #:cl #:org.shirakumo.autobuild.repository)
  (:shadow #:repository #:commit #:checkout)
  (:export))

(defpackage #:autobuild-archive-repository
  (:nicknames #:org.shirakumo.autobuild.repository.archive)
  (:use #:cl #:org.shirakumo.autobuild.repository)
  (:shadow #:repository #:commit #:checkout)
  (:export))
