#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *base-project-dir* (relative-dir (user-homedir-pathname) ".cache" "shirakumo" "autobuild"))

(defclass project (repository)
  ((build-type :initarg :build-type :accessor build-type)
   (builds :initform () :accessor builds)
   (name :initarg :name :accessor name)
   (branch :initarg :branch :accessor branch)
   (remote :initarg :remote :accessor remote)
   (watch :initarg :watch :accessor watch))
  (:default-initargs
   :build-type 'invalid-build
   :name NIL
   :branch NIL
   :remote NIL
   :watch NIL))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type T)
    (format stream "~s ~s ~s ~s ~s ~s"
            :name (name project) :branch (branch project)
            :build-type (build-type project))))

(defmethod initialize-instance :after ((project project) &key)
  (with-slots (name branch remote location) project
    (cond ((and (not name)
                (not remote)
                (not location))
           (error "At least one of NAME, REMOTE, or LOCATION must be given."))
          (location
           (init project :if-does-not-exist :create :branch branch)
           (unless remote
             (setf remote (remote-url project)))
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
           (init project :if-does-not-exist :create :branch branch)))
    (unless branch
      (setf branch (current-branch project)))))

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

(defmethod (setf name) :after (type (project project)) (offload project))
(defmethod (setf build-type) :after (type (project project)) (offload project))
(defmethod (setf watch) :after (type (project project)) (offload project))

(defgeneric project-config-file (project)
  (:method ((project project))
    (make-pathname :name ".autobuild" :type "lisp" :defaults (location project))))

(defgeneric offload (project)
  (:method :around ((project project))
    (call-next-method)
    project)
  (:method ((project project))
    (with-open-file (stream (project-config-file project)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*package* (find-package :org.shirakumo.autobuild)))
        (write `(:name ,(name project)
                 :watch ,(watch project)
                 :build ,(build-type project))
               :stream stream
               :readably T
               :pretty T
               :circle T)))))

(defgeneric restore (project)
  (:method :around ((project project))
    (call-next-method)
    project)
  (:method ((project project))
    (let ((script (autobuild-script:read-script-file (project-config-file project))))
      (macrolet ((fsetf (field accessor)
                   `(when (getf script ,field) (setf (,accessor project) (getf script ,field)))))
        (fsetf :name name)
        (fsetf :build build-type)
        (fsetf :watch watch)))))

(defgeneric scan-for-builds (project)
  (:method ((project project))
    (mapcar (lambda (dir) (coerce-build (build-type project) :location dir :project project))
            (uiop:subdirectories (relative-dir (location project) ".autobuild")))))

(defgeneric build-dir (project &optional commit)
  (:method ((project project) &optional (commit (current-commit project)))
    (relative-dir (location project) ".autobuild" commit)))

(defgeneric ensure-build (project commit)
  (:method ((project project) commit)
    (or (build commit project)
        (let ((dir (build-dir project commit)))
          (v:debug :autobuild.project "Creating build for ~a ~s" project commit)
          (clone project dir)
          (let ((build (coerce-build (build-type project) :location dir :project project)))
            (checkout build commit)
            (reset build :hard T)
            (push build (builds project))
            build)))))

(defgeneric ensure-current-build (project)
  (:method ((project project))
    (ensure-build project (current-commit project))))

(defmethod perform-build ((project project))
  (perform-build (ensure-current-build project)))

(defgeneric build (id project)
  (:method (id (project project))
    (find id (builds project) :key #'current-commit :test #'equalp))
  (:method (id (name T))
    (build id (project name))))

(defgeneric clean (project)
  (:method ((project project))
    (dolist (build (copy-list (nthcdr 5 (builds project))))
      (destroy build))))
