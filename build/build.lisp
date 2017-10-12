#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defvar *build*)
(defvar *builds* ())
(defvar *builds-lock* (bt:make-lock "builds-list lock"))

(defun list-builds ()
  (copy-list *builds*))

(defmethod find-build (recipe-ish commit)
  (find-build (find-recipe recipe-ish :error T) commit))

(defmethod find-build ((recipe recipe) commit)
  (dolist (build *builds*)
    (when (and (eq recipe (recipe build))
               (equal commit (commit build)))
      (return build))))

(defmethod find-build ((recipe recipe) (latest (eql :latest)))
  (let ((latest NIL))
    (flet ((tt (build)
             (autobuild-repository:timestamp
              (autobuild-repository:find-commit
               (commit build) (repository recipe)))))
      (dolist (build *builds* latest)
        (when (and (eq recipe (recipe build))
                   (or (not latest))
                   (< (tt latest) (tt build)))
          (setf latest build))))))

(defclass build ()
  ((status :initform :created :accessor status)
   (recipe :initarg :recipe :accessor recipe)
   (commit :accessor commit)
   (arguments :initarg :arguments :accessor arguments)
   (location :initarg :location :accessor location)
   (current-stage :initform NIL :accessor current-stage)
   (metrics :initform (make-hash-table) :accessor metrics)
   (thread :initform NIL :accessor thread))
  (:default-initargs
   :recipe (error "RECIPE required.")
   :commit :latest
   :arguments ()
   :location (error "LOCATION required.")))

(defmethod initialize-instance :around ((build build) &rest initargs)
  (let ((initargs (copy-list initargs)))
    (setf (getf initargs :recipe) (etypecase (getf initargs :recipe)
                                    (recipe (getf initargs :recipe))
                                    (T (find-recipe (getf initargs :recipe) :error T))))
    (apply #'call-next-method build initargs)))

(defmethod initialize-instance :after ((build build) &key commit)
  (check-type (recipe build) recipe)
  (check-type (location build) pathname)
  (setf (commit build) commit)
  (bt:with-lock-held (*builds-lock*)
    (push build *builds*)))

(defmethod (setf commit) ((commit string) (build build))
  (setf (commit build) (autobuild-repository:find-commit commit (repository build))))

(defmethod (setf status) :before (status (build build))
  (v:info :autobuild.manager "~a changing status to ~a." build status))

(defmethod (setf status) :after (status (build build))
  (deeds:issue
   (make-instance
    (ecase status
      (:started 'build-started)
      (:completed 'build-completed)
      (:cancelled 'build-cancelled)
      (:failed 'build-failed))
    :build build)
   deeds:*standard-event-loop*))

(defmethod print-object ((build build) stream)
  (print-unreadable-object (build stream :type T)
    (format stream "~a/~a ~a"
            (name (recipe build)) (autobuild-repository:id (commit build)) (status build))))

(defmethod repository ((build build))
  (repository (recipe build)))

(defmethod execute :before ((build build))
  (setf (status build) :started))

(defmethod execute :after ((build build))
  (setf (status build) :completed))

(defmethod execute :around ((build build))
  (flet ((w (line)
           (v:info :autobuild.manager "~a: ~a" build line)
           (deeds:do-issue build-output :output line :build build)))
    (let ((finished NIL)
          (*build* build)
          (*standard-output* (make-instance 'line-stream :on-line #'w)))
      (unwind-protect
           (multiple-value-prog1
               (restart-case (call-next-method)
                 (cancel-build ()
                   :report "Cancel the build."
                   (setf (status build) :cancelled)))
             (setf finished T))
        (unless finished
          (setf (status build) :failed))))))

(defmethod execute ((build build))
  (autobuild-repository:checkout
   (commit build)
   (location build))
  (simple-inferiors:with-chdir ((location build))
    (dolist (stage (compute-plan (recipe build)))
      (run-stage stage build))))

(defmethod run-stage :before ((stage stage) (build build))
  (setf (current-stage build) stage)
  (v:info :autobuild.manager "~a entering stage ~a."
          build (name stage)))

(defmethod run-stage :after ((stage stage) (build build))
  (setf (current-stage build) NIL))

(defmethod run-stage ((stage stage) (build build))
  (let ((start-time (get-internal-real-time)))
    (execute stage)
    (setf (getf (gethash stage (metrics build)) :time)
          (/ (- (get-internal-real-time) start-time)
             INTERNAL-TIME-UNITS-PER-SECOND))))

(defmethod start ((build build))
  (unless (eql (status build) :created)
    (error "The build is already ~a." (status build)))
  (setf (thread build)
        (bt:make-thread (lambda ()
                          (unwind-protect (execute build)
                            (setf (thread build) NIL)))
                        :initial-bindings `((*standard-output* . ,*standard-output*)
                                            (*error-output* . ,*error-output*)
                                            (*trace-output* . ,*trace-output*)
                                            (*standard-input* . ,*standard-input*)
                                            (*terminal-io* . ,*terminal-io*)
                                            (*query-io* . ,*query-io*)
                                            (*debug-io* . ,*debug-io*))))
  build)

(defmethod cancel ((build build))
  (unless (eql (status build) :started)
    (error "The build is not currently running."))
  (if (eql (bt:current-thread) (thread build))
      (invoke-restart 'cancel-build)
      (bt:interrupt-thread (thread build) (lambda () (invoke-restart 'cancel-build))))
  build)

(defmethod destroy ((build build))
  (bt:with-lock-held (*builds-lock*)
    (setf *builds* (remove build *builds*)))
  (uiop:delete-directory-tree (location build) :validate (constantly T)))

(defun argument (name &optional (build *build*))
  (getf (intern (string-upcase name) :keyword)
        (arguments build)))
