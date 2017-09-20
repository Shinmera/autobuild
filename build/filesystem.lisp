#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defvar *root* (merge-pathnames #p"autobuild/" (user-homedir-pathname)))
(defvar *repositories* (make-hash-table))

(defmethod recipe-location ((recipe recipe))
  (merge-pathnames (format NIL "~a/" (name recipe))
                   *root*))

(defmethod commit-location ((commit autobuild-repository:commit) (recipe recipe))
  (merge-pathnames (format NIL "~a/" (autobuild-repository:id commit))
                   (recipe-location recipe)))

(defmethod ensure-repository ((recipe recipe))
  (or (gethash (name recipe) *repositories*)
      (setf (gethash (name recipe) *repositories*)
            (destructuring-bind (&key type remote branch &allow-other-keys)
                (commit recipe)
              (autobuild-repository:create type remote
                                           :branch branch
                                           :location (recipe-location recipe))))))
