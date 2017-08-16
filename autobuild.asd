#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
               (:file "script")
               (:file "stage")
               (:file "synchronized-repository")
               (:file "build")
               (:file "standard-builds")
               (:file "project")
               (:file "autobuild"))
  :depends-on (:alexandria
               :bordeaux-threads
               :dissect
               :verbose
               :legit
               :bordeaux-threads
               :simple-inferiors
               :simple-tasks
               :trivial-gray-streams
               :redirect-stream
               :cl-ppcre
               :closer-mop))
