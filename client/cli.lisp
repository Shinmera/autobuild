#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.client)

(defvar *root* (merge-pathnames "autobuild/" (user-homedir-pathname)))

(defparameter *command-line-spec*
  '((("root-path" #\r) :type string :optional T :documentation "")
    (("recipe" #\R) :type string :optional T :documentation "")
    (("build-path" #\p) :type string :optional T :documentation "")
    (("commit" #\c) :type string :optional T :documentation "")))

(defun ensure-recipe (recipe-ish)
  (typecase recipe-ish
    (pathname (autobuild-script:load-recipe recipe-ish))
    (T (autobuild-build:ensure-recipe recipe-ish))))

(defun cli (program &key root-path recipe build-path (commit :latest))
  (when root-path
    (setf *root* (uiop:parse-native-namestring root-path)))
  (cond ((string-equal program "client")
         (start-client))
        ((string-equal program "build")
         (let ((recipe (ensure-recipe recipe))
               (location (or build-path (find-build-path recipe commit))))
           (autobuild-build:execute
            (make-instance 'autobuild-build:build :recipe recipe :commit commit :location location))))
        ((string-equal program "list"))))

(defun main (args)
  (command-line-arguments:handle-command-line
   *command-line-spec* 'cli :positional-arity 1 :name "autobuild"))
