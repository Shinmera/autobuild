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
   (remote :initarg :remote :accessor remote))
  (:default-initargs
   :build-type 'invalid-build
   :name NIL
   :branch NIL
   :remote NIL))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type T)
    (format stream "~s ~s ~s ~s" :name (name project) :branch (branch project))))

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
           (setf (name project) (parse-directory-name (location project)))))
        ((and (remote project) (name project))
         (setf (location project) (relative-dir *base-project-dir* (name project)))
         (init project :if-does-not-exist :clone :remote (remote project) :branch (branch project)))
        ((remote project)
         (setf (name project) (parse-remote-name (remote project)))
         (setf (location project) (relative-dir *base-project-dir* (name project)))
         (init project :if-does-not-exist :clone :remote (remote project)  :branch (branch project)))
        ((name project)
         (setf (location project) (relative-dir *base-project-dir* (name project)))
         (init project :if-does-not-exist :create :branch (branch project))))
  (unless (branch project)
    (setf (branch project) (current-branch project)))
  (setf (builds project) (scan-for-builds project)))

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

(defgeneric scan-for-builds (project)
  (:method ((project project))
    (mapcar (lambda (dir) (make-instance 'build :location dir))
            (uiop:subdirectories (relative-dir (location project) ".autobuild")))))

(defgeneric build-dir (project)
  (:method ((project project))
    (relative-dir (location project) ".autobuild" (current-commit project))))

(defgeneric coerce-build (thing &rest args)
  (:method ((name symbol) &rest args)
    (apply #'make-instance name args))
  (:method ((build build) &rest args)
    (declare (ignore args))
    build))

(defmethod perform-build ((project project))
  (let ((dir (build-dir project)))
    (clone project dir)
    (let ((build (coerce-build (build-type project) :location dir :project project)))
      (push (cons (current-commit build) build) (builds project))
      (perform-build build)
      build)))

(defgeneric build (id project)
  (:method (id (project project))
    (cdr (assoc id (builds project) :test #'equalp))))

(defgeneric status (project)
  (:method ((project project))
    `(:commit ,(current-commit project)
      :branch ,(current-branch project)
      :location ,(location project))))

(defgeneric watch-project (project function &key interval change-fun &allow-other-keys)
  (:method :around (project function &key interval change-fun)
    (declare (ignore project function interval change-fun))
    (with-simple-restart (abort "Stop watching ~a" project)
      (call-next-method)))
  (:method ((project project) function &key (interval 10) (change-fun #'pull))
    (loop for prev = NIL then status
          for status = (status project)
          do (when (and prev (loop for (key val) on status by #'cddr
                                   thereis (not (equal val (getf prev key)))))
               (with-simple-restart (continue "Continue watching, skipping the current call.")
                 (funcall function status)))
             (sleep interval)
             (funcall change-fun project))))
