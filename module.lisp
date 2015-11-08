#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:autobuild-server
  (:nicknames #:org.shirakumo.autobuild.server)
  (:shadowing-import-from #:autobuild #:remote #:name #:target)
  (:use #:cl #:radiance #:autobuild)
  (:export)
  (:domain "autobuild"))
(in-package #:autobuild-server)
