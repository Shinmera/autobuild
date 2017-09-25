#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.manager)

(defvar *recipes* (make-hash-table :test 'eql))

(defclass recipe (autobuild-build:recipe)
  ((build :initform NIL :accessor build)))

(defmethod initialize-instance :before ((recipe recipe) &key stages)
  ;; Ensure all stages are recipe instances.
  (loop for cons on stages
        do (setf (car cons) (etypecase (cdr cons)
                              (autobuild-build:stage (cdr cons))
                              (T (find-recipe (cdr cons) :error T))))))

(defun find-recipe (name &key error)
  (or (gethash name *recipes*)
      (when error (error "No recipe with name ~s known." name))))

(defun (setf find-recipe) (recipe name &key force)
  (check-type recipe recipe)
  (when (and (find-recipe name :error NIL) (not force))
    (error "A recipe with the name ~s already exists." name))
  (setf (gethash name *recipes*) recipe))

(defun remove-recipe (name)
  (let ((recipe (find-recipe name :error T)))
    (when (build recipe)
      (warn "~s has a build running. Cancelling the build." recipe)
      (autobuild-build:cancel (build recipe)))
    (remhash name *recipes*))
  name)

(defun list-recipes ()
  (loop for v being the hash-values of *recipes*
        collect v))
