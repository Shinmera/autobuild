#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defvar *recipes* (make-hash-table :test 'equal))

(defun ensure-recipe (recipe-ish)
  (typecase recipe-ish
    (recipe recipe-ish)
    (T (find-recipe recipe-ish :error T))))

(defun find-recipe (name &key error)
  (or (gethash name *recipes*)
      (when error (error "No recipe with name ~s known." name))))

(defun (setf find-recipe) (recipe name &key force)
  (check-type recipe recipe)
  (when (and (find-recipe name :error NIL) (not force))
    (cerror "Remove the previous recipe." "A recipe with the name ~s already exists." name)
    (remove-recipe name))
  (setf (gethash name *recipes*) recipe))

(defun remove-recipe (name)
  (let ((recipe (find-recipe name :error T)))
    (dolist (build *builds*)
      (when (and (eql :running (status build))
                 (eql recipe (recipe build)))
        (warn "~s has a build running. Cancelling ~a." recipe build)
        (autobuild-build:cancel build)))
    (remhash name *recipes*))
  name)

(defun list-recipes ()
  (loop for v being the hash-values of *recipes*
        collect v))

(defclass recipe (stage)
  ((repository :initarg :repository :accessor repository)
   (stage :initarg :stages :accessor stages))
  (:default-initargs
   :repository (error "REPOSITORY required.")))

(defmethod initialize-instance :before ((recipe recipe) &key stages dependencies repository)
  (dolist (stage stages) (check-type stage stage))
  (map-into dependencies #'ensure-recipe dependencies)
  (check-type repository autobuild-repository:repository))

(defmethod compute-plan ((recipe recipe))
  (let ((deps (call-next-method))
        (stages (compute-plan (make-instance 'stage :dependencies (stages recipe)))))
    (nconc deps (nbutlast stages))))

(defmethod execute ((recipe recipe)))
