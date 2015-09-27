#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *base-project-folder* (relative-dir (user-homedir-pathname) ".cache" "shirakumo" "autobuild"))

(defclass project (repository)
  ((build-type :initarg :build-type :accessor build-type)
   (builds :initform () :accessor builds)
   (name :initform :name :accessor name)
   (branch :initform :branch :accessor branch)
   (remote :initform :remote :accessor remote))
  (:default-initargs
   :build-type 'build
   :name NIL
   :branch NIL
   :remote NIL))

(defmethod initialize-instance :after ((project project) &key)
  (cond ((and (not (name project))
              (not (remote project))
              (not (location project)))
         (error "At least one of NAME, REMOTE, or LOCATION must be given."))
        ((location project)
         (init project :if-does-not-exist :create :branch (branch project))
         (unless (remote project)
           (setf (remote project) (remote-url project)))
         (unless (name project)
           (setf (name project) (car (last (pathname-directory (location project)))))))
        ((and (remote project) (name project))
         (setf (location project) (relative-dir *base-project-folder* (name project)))
         (init project :if-does-not-exist :clone :remote (remote project) :branch (branch project)))
        ((remote project)
         (setf (name project) (parse-remote-name (remote project)))
         (setf (location project) (relative-dir *base-project-folder* (name project)))
         (init project :if-does-not-exist :clone :remote (remote project)  :branch (branch project)))
        ((name project)
         (setf (location project) (relative-dir *base-project-folder* (name project)))
         (init project :if-does-not-exist :create :branch (branch project))))
  (unless (branch project)
    (setf (branch project) (current-branch project)))
  (setf (builds project) (scan-for-builds project)))

(defun parse-remote-name (name)
  )

(defgeneric scan-for-builds (project)
  (:method ((project project))
    (mapcar (lambda (dir) (make-instance 'build :location dir))
            (uiop:subdirectories (relative-dir (location project) ".autobuild")))))

(defgeneric build-dir (project)
  (:method ((project project))
    (relative-dir (location project) ".autobuild" (current-commit project))))

(defmethod perform-build ((project project))
  (let ((dir (build-dir project)))
    (clone project dir)
    (let ((build (make-instance 'build :location dir)))
      (push build (builds project))
      (perform-build build))))
