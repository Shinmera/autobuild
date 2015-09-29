#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem autobuild
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Primitive automated build system"
  :homepage "https://github.com/Shinmera/autobuild"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "redirect-stream")
               (:file "build")
               (:file "project")
               (:file "autobuild"))
  :depends-on (:alexandria
               :dissect
               :verbose
               :legit
               :bordeaux-threads
               :trivial-gray-streams))
