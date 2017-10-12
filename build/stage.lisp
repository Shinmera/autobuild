#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defclass stage ()
  ((name :initarg :name :accessor name)
   (dependencies :initarg :dependencies :accessor dependencies))
  (:default-initargs
   :name (gensym (string :stage-))
   :dependencies ()))

(defmethod print-object ((stage stage) stream)
  (print-unreadable-object (stage stream :type T)
    (format stream "~a" (name stage))))

(defmethod execute :around ((stage stage))
  (with-simple-restart (skip "Skip executing stage ~a." stage)
    (call-next-method)
    stage))

(defmethod compute-plan ((stage stage))
  (let ((sorted ())
        (visited (make-hash-table :test 'eq)))
    (labels ((visit (stage)
               (ecase (gethash stage visited)
                 (:temporary
                  (error "The stage dependency graph contains cycles."))
                 (:permanently)
                 ((NIL)
                  (setf (gethash stage visited) :temporary)
                  (mapcar #'visit (dependencies stage))
                  (setf (gethash stage visited) :permanently)
                  (push stage sorted)))))
      (visit stage))
    (nreverse sorted)))

(defclass announce-stage (stage)
  ())

(defmethod execute ((stage announce-stage))
  (v:info :announce "Running stage ~s." (name stage)))

(defclass function-stage (stage)
  ((func :initarg :function :initform (lambda ()) :accessor func)))

(defmethod execute ((stage function-stage))
  (funcall (func stage)))

(defclass eval-stage (stage)
  ((form :initarg :form :initform () :accessor form)))

(defmethod execute ((stage eval-stage))
  (funcall (compile NIL `(lambda () ,(form stage)))))

(defclass lisp-stage (stage)
  ((inferior-lisp :initarg :inferior-lisp :initform (first (uiop:raw-command-line-arguments)) :accessor inferior-lisp)
   (lisp-type :initarg :lisp-type :initform :SBCL :accessor lisp-type)
   (form :initarg :form :initform () :accessor form)))

(defmethod execute ((stage lisp-stage))
  (let* ((form-string (let ((*package* (find-package "CL-USER")))
                        (typecase (form stage)
                          (string (form stage))
                          (T (prin1-to-string (form stage))))))
         (args (ecase (lisp-type stage)
                 (:sbcl (list "--noinform" "--non-interactive" "--eval" form-string)))))
    (simple-inferiors:run (inferior-lisp stage) args :output T :error T :on-non-zero-exit :error :copier :line)))

(defclass asdf-system-stage (lisp-stage)
  ((operation :initarg :operation :initform :load-op :accessor operation)
   (system :initarg :system :initform (error "SYSTEM required.") :accessor system)))

(defmethod execute :before ((stage asdf-system-stage))
  (setf (form stage) `(progn (asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :inherit-configuration))
                             (asdf:operate ,(operation stage) ,(system stage) :verbose T :force T))))
