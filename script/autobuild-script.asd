#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem autobuild-script
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://Shinmera.github.io/autobuild/"
  :bug-tracker "https://github.com/Shinmera/autobuild/issues"
  :source-control (:git "https://github.com/Shinmera/autobuild.git")
  :serial T
  :components ((:file "package")
               (:file "functions")
               (:file "script")
               (:file "documentation"))
  :depends-on (:autobuild-build
               :documentation-utils
               :simple-inferiors
               :cl-ppcre
               :uiop
               :form-fiddle))