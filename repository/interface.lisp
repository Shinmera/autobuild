#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.repository)

(defvar *repository-types* ())

(defclass remote-ish () ())
(defgeneric permalink (remote-ish))

(defclass located () ())
(defgeneric location (located))
(defgeneric delete (located))

(defmethod print-object ((located located) stream)
  (print-unreadable-object (located stream :type T)
    (format stream "~a" (location located))))

(defmethod delete ((located located))
  (uiop:delete-directory-tree
   (location located)
   :validate (constantly T)))

(defmethod delete :around ((located located))
  (call-next-method)
  located)

(defgeneric create (type location remote &key branch))
(defclass repository (remote-ish located) ())
(defgeneric update (repository))

(defmethod update :around ((repository repository))
  (call-next-method)
  repository)

(defgeneric list-commits (repository))
(defgeneric find-commit (commit-id repository &optional error))

(defmethod find-commit ((commit-id (eql :latest)) (repository repository) &optional error)
  (or (first (list-commits repository))
      (error "There is no latest commit on ~a." repository)))

(defclass commit (remote-ish) ())
(defgeneric repository (commit))
(defgeneric id (commit))
(defgeneric author (commit))
(defgeneric message (commit))
(defgeneric timestamp (commit))

(defmethod print-object ((commit commit) stream)
  (print-unreadable-object (commit stream :type T)
    (format stream "~a" (id commit))))

(defgeneric checkout (commit location))
(defclass checkout (commit located) ())
(defgeneric clean (checkout))

(defmethod clean :around ((checkout checkout))
  (call-next-method)
  checkout)
