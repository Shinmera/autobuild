#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem autobuild-repository
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shinmera/autobuild"
  :serial T
  :components ((:file "package")
               (:file "interface")
               (:file "git")
               (:file "archive")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :legit))