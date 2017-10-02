#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:autobuild-script
  (:nicknames #:org.shirakumo.autobuild.script)
  (:use #:cl)
  ;; functions.lisp
  (:export
   #:--
   #:>>
   #:./
   #:sed
   #:cp
   #:rm)
  ;; script.lisp
  (:export
   #:parse-body-for-class
   #:load-script))

(defpackage #:autobuild-script-user
  (:nicknames #:org.shirakumo.autobuild.script.user)
  (:use #:cl #:autobuild-build #:autobuild-script))
