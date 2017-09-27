#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defvar *recipes* (make-hash-table :test 'eql))

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
    (dolist (build *builds*)
      (when (eql recipe (recipe build))
        (warn "~s has a build running. Cancelling ~a." recipe build)
        (autobuild-build:cancel (build recipe))))
    (remhash name *recipes*))
  name)

(defun list-recipes ()
  (loop for v being the hash-values of *recipes*
        collect v))

(defclass recipe (stage)
  ((repository :initarg :repository :accessor repository)
   (dependencies :initarg :stages :accessor stages))
  (:default-initargs
   :repository (error "REPOSITORY required.")))

(defmethod initialize-instance :before ((recipe recipe) &key stages)
  ;; Ensure all stages are recipe instances.
  (loop for cons on stages
        do (setf (car cons) (etypecase (cdr cons)
                              (autobuild-build:stage (cdr cons))
                              (T (find-recipe (cdr cons) :error T))))))

(defmethod initialize-instance :after ((recipe recipe) &key)
  (check-type (repository recipe) autobuild-repository:repository))

(defmethod execute ((recipe recipe)))
