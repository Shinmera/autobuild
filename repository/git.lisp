#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.repository.git)

(defmethod create ((type (eql :git)) location remote &key branch)
  (change-class (if (uiop:directory-exists-p location)
                    (make-instance 'legit:repository :location location)
                    (legit:clone remote location :branch branch :bare T))
                'repository))

(defclass repository (autobuild-repository:repository legit:repository)
  ((commit-table :initform (make-hash-table :test 'equal) :accessor commit-table)))

(defmethod permalink ((repo repository))
  (cdr (first (legit:remotes repo))))

(defmethod location ((repository repository))
  (legit:location repository))

(defmethod update ((repo repository))
  (legit:fetch repo :remote (cdr (first (legit:remotes repo)))))

(defmethod list-commits ((repo repository))
  (loop for hash in (legit:commits repo)
        collect (or (gethash hash (commit-table repo))
                    (setf (gethash hash (commit-table repo))
                          (make-instance 'commit :repo repo :hash hash)))))

(defmethod find-commit ((hash string) (repo repository) &optional error)
  (or (find hash (list-commits repo) :key #'hash :test #'string=)
      (when error (error "No commit with hash ~s found in ~a."
                         hash repo))))

(defclass commit (autobuild-repository:commit)
  ((repo :initarg :repo :accessor repo)
   (hash :initarg :hash :accessor hash)))

(defmethod permalink ((commit commit))
  "uuuh.")

(defmethod autobuild-repository:repository ((commit commit))
  (repo commit))

(defmethod id ((commit commit))
  (hash commit))

(defmethod author ((commit commit))
  (legit:commit-author (repo commit) (hash commit)))

(defmethod message ((commit commit))
  (legit:commit-message (repo commit) (hash commit)))

(defmethod timestamp ((commit commit))
  (legit:commit-age (repo commit) (hash commit)))

(defmethod autobuild-repository:checkout ((commit commit) location)
  (change-class (legit:clone (repo commit) location :branch (hash commit))
                'checkout :repo (repo commit)
                          :hash (hash commit)))

(defclass checkout (commit legit:repository autobuild-repository:checkout)
  ())

(defmethod location ((checkout checkout))
  (legit:location checkout))

(defmethod clean ((checkout checkout))
  (legit:reset checkout :hard T :to "HEAD")
  (legit:clean checkout :force T :directories T :ignored T))

(pushnew :git *repository-types*)
