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

(defun make-build-project (name remote &key branch)
  (when (project name)
    (cerror "Name ~s is already taken by ~a." name (project name)))
  (let ((project (make-instance 'project :name name :remote remote :branch branch)))
    (setf (project NIL) project)))

(defmethod destroy :after ((project project))
  (setf *projects* (delete project *projects*)))

(defun scan-for-projects (&optional (dir *base-project-dir*))
  (mapcar (lambda (dir)
            (let ((project (make-instance 'project :location dir)))
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
    (handler-bind ((error (lambda (err)
                            (declare (ignore err))
                            (when (find-restart 'skip)
                              (invoke-restart 'skip)))))
      (call-next-method))))

(defvar *builder* (make-instance 'builder))
(defvar *builder-thread* NIL)
(defvar *watcher* (make-instance 'builder))
(defvar *watcher-thread* NIL)

(defclass watch-task (task)
  ((timeout :initarg :timeout :accessor timeout))
  (:default-initargs
   :timeout 30))

(defmethod run-task ((task watch-task))
  (dolist (project *projects*)
    (when (watch project)
      (v:info :watcher "Watching project ~a for changes." project)
      (let ((old-commit (current-commit project))
            (new-commit (progn (pull project) (current-commit project))))
        (when (string/= old-commit new-commit)
          (v:info :watcher "~a has changed. Performing build." project)
          (let ((build (ensure-build project new-commit :restore :if-newer)))
            (simple-tasks:schedule-task build *builder*))))))
  ;; Done, reschedule self in a moment.
  (bt:make-thread
   (lambda ()
     (sleep (timeout task))
     (schedule-task (make-instance 'watch-task) *watcher*))))

(defun initialize-autobuild ()
  (setf *projects* (scan-for-projects))
  (unless (eql (status *builder*) :running)
    (setf *builder-thread* (make-runner-thread *builder*)))
  (unless (eql (status *watcher*) :running)
    (setf *watcher-thread* (make-runner-thread *watcher*))
    (schedule-task (make-instance 'watch-task) *watcher*)))
