#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.client)

(defvar *root* (pathname-utils:subdirectory (user-homedir-pathname) "autobuild"))

(defparameter *command-line-spec*
  '((("root-path" #\r) :type string :optional T :documentation "")
    (("recipe" #\R) :type string :optional T :documentation "")
    (("build-path" #\p) :type string :optional T :documentation "")
    (("commit" #\c) :type string :optional T :documentation "")))

(defmethod find-build-path ((recipe autobuild-build:recipe) (commit string))
  (pathname-utils:subdirectory (find-recipe-path recipe) "builds" commit))

(defmethod find-build-path (recipe (commit autobuild-repository:commit))
  (find-build-path recipe (autobuild-repository:id commit)))

(defmethod find-recipe-path ((recipe autobuild-build:recipe))
  (pathname-utils:subdirectory *root* (autobuild-build:name recipe)))

(defmethod autobuild-build:repository :around ((recipe autobuild-build:recipe))
  (let ((repo (call-next-method)))
    (etypecase repo
      (autobuild-repository:repository
       repo)
      (cons
       (destructuring-bind (type remote &optional branch) repo
         (let ((location (find-recipe-path recipe)))
           (setf (autobuild-build:repository recipe)
                 (autobuild-repository:create type location remote :branch branch))))))))

(defmethod autobuild-build:ensure-recipe ((path pathname))
  (autobuild-script:load-recipe path))

(defun cli (program &key root-path recipe build-path (commit :latest))
  (when root-path
    (setf *root* (uiop:parse-native-namestring root-path)))
  (cond ((string-equal program "client")
         (start-client))
        ((string-equal program "build")
         (let* ((recipe (autobuild-build:ensure-recipe recipe))
                (commit (autobuild-repository:find-commit commit (autobuild-build:repository recipe) T))
                (location (or build-path (find-build-path recipe commit))))
           (autobuild-build:execute
            (make-instance 'autobuild-build:build :recipe recipe :commit commit :location location))))
        ((string-equal program "list"))))

(defun main (args)
  (command-line-arguments:handle-command-line
   *command-line-spec* 'cli :positional-arity 1 :name "autobuild"))
