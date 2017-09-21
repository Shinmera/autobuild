#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.repository.archive)

(defmethod create ((type (eql :archive)) location remote &key branch)
  (let ((repository (make-instance 'repository :location location
                                               :remote remote)))
    (unless (uiop:directory-exists-p location)
      (update repository))
    repository))

(defclass repository (autobuild-repository:repository)
  ((commits :initform () :accessor commits)
   (location :initarg :location :accessor location)
   (remote :initarg :remote :accessor remote)))

(defun pathname-filename (pathname)
  (format NIL "~@[~a~]~@[.~a~]"
          (pathname-name pathname)
          (pathname-type pathname)))

(defun update-locally (path target)
  (dolist (file (uiop:directory-files path))
    (let ((other (make-pathname :name (pathname-name file)
                                :type (pathname-type file)
                                :defaults target)))
      (unless (uiop:file-exists-p other)
        (uiop:copy-file file other)))))

(defun update-remotely (url target)
  ;; FIXME: actually implement the download and search logic.
  )

(defmethod update ((repo repository))
  (etypecase (remote repo)
    (pathname
     (update-locally (remote repo) (location repo)))
    (string
     (update-remotely (remote repo) (location repo))))
  
  (dolist (file (uiop:directory-files (location repo)))
    (let ((file (pathname-filename file)))
      (unless (find file (commits repo) :key #'file :test #'string=)
        (push (make-instance 'commit :repo repo
                                     :file file)
              (commits repo))))))

(defmethod list-commits ((repo repository))
  (commits repo))

(defmethod find-commit (commit-ish (repo repository) &optional error)
  (or (find commit-ish (list-commits repo) :key #'file :test #'string=)
      (when error (error "No commit with name ~s found on ~a."
                         commit-ish repo))))

(defclass commit (autobuild-repository:commit)
  ((repo :initarg :repo :accessor repo)
   (file :initarg :file :accessor file)))

(defmethod repository ((commit commit))
  (repo commit))

(defmethod id ((commit commit))
  (file commit))

(defmethod author ((commit commit))
  NIL)

(defmethod message ((commit commit))
  NIL)

(defmethod timestamp ((commit commit))
  NIL)

(defmethod checkout ((commit commit) location)
  (prepare (make-instance 'checkout :repo (repo commit)
                                    :file (file commit)
                                    :location location)))

(defclass checkout (autobuild-repository:checkout)
  ((location :initarg :location :accessor location)))

(defmethod prepare ((checkout checkout))
  (extract (merge-pathnames (file commit) (location (repo commit)))
           (location checkout))
  checkout)

(defmethod clean ((checkout checkout))
  (delete checkout)
  (prepare checkout))

(pushnew :archive *repository-types*)
