#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defvar *projects* ())

(defgeneric project (id)
  (:method ((project project))
    project)
  (:method (id)
    (find id *projects* :key #'name :test #'equalp)))

(defgeneric (setf project) (project place)
  (:method ((project project) (place (eql NIL)))
    (push project *projects*)))

(defun remove-project (id)
  (setf *projects*
        (etypecase id
          (project (remove id *projects*))
          (T (remove id *projects* :key #'name :test #'equalp)))))

(defun make-build-project (build-type name remote &key branch)
  (when (project name)
    (cerror "Name ~s is already taken by ~a." name (project name)))
  (let ((project (make-instance 'project :name name :remote remote :branch branch :build-type build-type)))
    (setf (project NIL) project)))

(defvar *watchers* (make-hash-table :test 'eq))

(defclass watcher ()
  ((project :initarg :project :reader project)
   (output :initform (make-redirect-output-stream) :reader watcher-output)
   (thread :initform NIL :accessor thread))
  (:default-initargs
   :project (error "PROJECT required.")))

(defmethod output ((watcher watcher))
  (output-stream (watcher-output watcher)))

(defmethod (setf output) (stream (watcher watcher))
  (setf (output-stream (watcher-output watcher)) stream))

(defmethod initialize-instance :after ((watcher watcher) &key)
  (setf (watcher (project watcher)) watcher)
  (setf (thread watcher)
        (bt:make-thread
         (lambda ()
           (watch-project
            (project watcher)
            (lambda (status)
              (declare (ignore status))
              (perform-build (project watcher)))))
         :name (format NIL "~a watcher" (name (project watcher)))
         :initial-bindings `((*standard-output* . ,(watcher-output watcher))
                             (*error-output* . ,(watcher-output watcher))))))

(defun watcher (project)
  (gethash (project project) *watchers*))

(defun (setf watcher) (watcher project)
  (setf (gethash (project project) *watchers*) watcher))

(defun remove-watcher (project)
  (remhash (project project) *watchers*))

(defun start-watcher (project)
  (when (watcher project)
    (cerror "Project ~a already has a watcher ~a." project (watcher project)))
  (make-instance 'watcher :project project))

(defun stop-watcher (project)
  (unless (watcher project)
    (error "Project ~a does not have a watcher." project))
  (let ((watcher (watcher project)))
    (bt:interrupt-thread watcher (lambda () (invoke-restart 'abort)))
    (remove-watcher project)))
