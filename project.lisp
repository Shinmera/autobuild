#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *base-project-dir* (relative-dir (user-homedir-pathname) ".cache" "shirakumo" "autobuild"))

(defclass project (repository)
  ((builds :initform () :accessor builds)
   (name :initarg :name :accessor name)
   (watch :initarg :watch :accessor watch))
  (:default-initargs
   :name NIL
   :branch NIL
   :watch NIL))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type T)
    (format stream "~s ~s ~s ~s"
            :name (name project) :branch (current-branch project))))

(defmethod initialize-instance :after ((project project) &key branch remote)
  (with-slots (name location) project
    (cond ((and (not name)
                (not remote)
                (not location))
           (error "At least one of NAME, REMOTE, or LOCATION must be given."))
          (location
           (init project :if-does-not-exist :create :branch branch)
           (unless name
             (setf name (parse-directory-name location))))
          ((and remote name)
           (setf location (relative-dir *base-project-dir* name))
           (init project :if-does-not-exist :clone :remote remote :branch branch))
          (remote
           (setf name (parse-remote-name remote))
           (setf location (relative-dir *base-project-dir* name))
           (init project :if-does-not-exist :clone :remote remote  :branch branch))
          (name
           (setf location (relative-dir *base-project-dir* name))
           (init project :if-does-not-exist :create :branch branch)))))

(defun parse-directory-name (pathname)
  (car (last (pathname-directory pathname))))

(defun parse-remote-name (name)
  (let ((colon-pos (position #\: name :from-end T))
        (slash-pos (position #\/ name :from-end T)))
    (cond ((and (not colon-pos) (not slash-pos))
           name)
          ((not colon-pos)
           (subseq name (1+ slash-pos)))
          ((not slash-pos)
           (subseq name (1+ colon-pos)))
          ((< colon-pos slash-pos)
           (subseq name (1+ slash-pos)))
          ((< slash-pos colon-pos)
           (subseq name (1+ colon-pos))))))

(defmethod (setf builds) (builds (project project))
  ;; Ensure things are sorted by date.
  (setf (slot-value project 'builds)
        (sort builds #'> :key #'current-age)))

(defgeneric project-config-file (project)
  (:method ((project project))
    (make-pathname :name ".autobuild" :type "lisp" :defaults (location project))))

(defgeneric scan-for-builds (project)
  (:method ((project project))
    (mapcar (lambda (dir) (coerce-build 'build :location dir :project project :restore :if-newer))
            (uiop:subdirectories (relative-dir (location project) ".autobuild")))))

(defgeneric build-dir (project &optional commit)
  (:method ((project project) &optional (commit (current-commit project)))
    (let ((name (git-value project `(rev-parse ,commit) (git-rev-parse commit))))
      (relative-dir (location project) ".autobuild" name))))

(defgeneric ensure-build (project commit &rest args)
  (:method ((project project) commit &rest args)
    ;; FIXME: Parallelism issue
    (or (build commit project)
        (let ((dir (build-dir project commit))
              (args (copy-list args)))
          (unless (getf args :type) (setf (getf args :type) 'build))
          (v:debug :autobuild.project "Creating build for ~a ~s in ~s" project commit dir)
          (clone project dir)
          (let ((build (coerce-build args :location dir :project project)))
            (checkout build commit)
            (reset build :hard T)
            (push build (builds project))
            build)))))

(defmethod perform-build ((project project))
  (perform-build (ensure-build project (current-commit project))))

(defgeneric build (id project)
  (:method (id (project project))
    (find id (builds project) :key #'current-commit
                              :test (lambda (id hash) (search id hash :test #'char-equal))))
  (:method (id (name T))
    (build id (or (project name) (return-from build NIL)))))

(defmethod clean ((project project) &key)
  (dolist (build (copy-list (nthcdr 5 (builds project))))
    (destroy build)))
