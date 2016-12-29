#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:autobuild-server
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "0.1.0"
  :license "Artistic"
  :description "A webserver interface to autobuild."
  :homepage "https://github.com/Shinmera/autobuild"
  :components ((:file "module")
               (:file "front"))
  :depends-on ((:interface :user)
               (:interface :auth)
               :i-json
               :r-clip
               :autobuild
               :system-load
               :alexandria))
