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

(defun scan-for-projects (&optional (dir *base-project-dir*))
  (mapcar (lambda (dir)
            (let ((project (restore (make-instance 'project :location dir))))
              (setf (builds project) (scan-for-builds project))
              project))
          (uiop:subdirectories dir)))

(defclass builder (queued-runner)
  ((output-stream :initform (redirect-stream:make-redirect-stream) :accessor output-stream)))

(defmethod output ((builder builder))
  (redirect-stream:stream (output-stream builder)))

(defmethod (setf output) (stream (builder builder))
  (setf (redirect-stream:stream (output-stream builder)) stream))

(defmethod start-runner :around ((builder builder))
  (let ((*standard-output* (output-stream builder))
        (*error-output* (output-stream builder)))
    (call-next-method)))

(defvar *builder* (make-instance 'builder))
(defvar *builder-thread* (make-runner-thread *builder*))

(eval-when (:load-toplevel :execute)
  (setf *projects* (scan-for-projects)))
