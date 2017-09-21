#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.repository.local)

(defmethod create ((type (eql :local)) location remote &key branch)
  (let ((repository (make-instance 'repository :remote remote
                                               :location location)))
    (unless (uiop:directory-exists-p location)
      (update repository))
    repository))

(defclass repository (autobuild-repository:repository
                      autobuild-repository:checkout)
  ())

(defmethod update ((repository repository))
  (when (string/= remote location)
    ;; FIXME: Copy tree
    ))

(defmethod list-commits ((repository repository))
  (list repository))

(defmethod find-commit (commit-id (repository repository) &optional error)
  (or (when (string= commit-id "") repository)
      (when error (error "No commit named ~s in ~a." commit-id repository))))

(defmethod autobuild-repository:repository ((repository repository))
  repository)

(defmethod id ((repository repository))
  "")

(defmethod author ((repository repository))
  NIL)

(defmethod message ((repository repository))
  NIL)

(defmethod timestamp ((repository repository))
  NIL)

(defmethod checkout ((repository repository) location)
  (make-instance 'repository :remote (remote repository)
                             :location location))

(defmethod clean ((repository repository))
  (error "Don't know how to clean a local repository."))

(push :local *repository-types*)
