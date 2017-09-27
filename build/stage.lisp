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
  ((func :initarg :function :accessor func))
  (:default-initargs
   :func (lambda ())))

(defmethod execute ((stage function-stage))
  (funcall (func stage)))
