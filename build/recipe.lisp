#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defgeneric ensure-repository (type remote branch))
(defgeneric commit-location (commit recipe))

(defclass stage ()
  ((name :initarg :name :accessor name)
   (dependencies :initarg :dependencies :accessor dependencies)))

(defgeneric execute (stage))
(defgeneric compute-plan (stage))

(defmethod compute-plan ((stage stage))
  (let ((sorted ())
        (visited (make-hash-table :test 'eq)))
    (labels ((visit (stage)
               (ecase (gethash stage visited)
                 (:temporary
                  (error "The stage dependency graph contains cycles."))
                 (:permanently)
                 ((NIL)
                  (setf (gethash stage visited) :temporary)
                  (mapcar #'visit (dependencies stage))
                  (setf (gethash stage visited) :permanently)
                  (push stage sorted)))))
      (visit stage))
    sorted))

(defclass recipe (stage)
  ((commit :initarg :commit :accessor commit)
   (stages :initarg :stages :accessor stages)))

(defmethod execute ((recipe recipe))
  (destructuring-bind (type remote branch commit-id)
      (commit recipe)
    (let* ((repo (ensure-repository type remote branch))
           (commit (find-commit commit-id repo))
           (checkout (checkout commit (commit-location commit recipe))))
      (simple-inferiors:with-chdir ((location checkout))
        (dolist (stage (stages recipe))
          (execute stage))))))
