#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defclass timed-task (task)
  ((start :initform NIL :accessor start)
   (end :initform NIL :accessor end)))

(defmethod run-task :around ((timed-task timed-task))
  (setf (start timed-task) (get-universal-time)
        (end timed-task) T)
  (unwind-protect
       (call-next-method)
    (setf (end timed-task) (get-universal-time))))

(defgeneric duration (timed-task)
  (:method ((timed-task timed-task))
    (when (and (start timed-task) (end timed-task))
      (- (if (eql (end timed-task) T)
             (get-universal-time)
             (end timed-task))
         (start timed-task)))))

(defclass stage (timed-task)
  ((name :initarg :name :accessor name)
   (script :initarg :script :accessor script :script T))
  (:default-initargs
   :name (error "NAME required.")
   :script NIL)
  (:metaclass autobuild-script:script-class))

(defun handle-stage-start (stage)
  (v:info :autobuild "~a entering stage ~a" (runner stage) stage)
  (format T "~&;; Begin stage ~a" (name stage))
  (format T "~&; Started on ~a (~a)~%" (format-date (start stage)) (start stage))
  (finish-output T))

(defun handle-stage-end (stage)
  (v:info :autobuild "~a leaving stage ~a" (runner stage) stage)
  (format T "~&; Ended on ~a (~a)" (format-date (end stage)) (end stage))
  (format T "~&; Stage took ~a~%" (format-time (duration stage)))
  (format T "~&;; End stage ~a" (name stage))
  (finish-output T))

(defmethod run-task :before ((stage stage))
  (handle-stage-start stage))

(defmethod run-task :around ((stage stage))
  (unwind-protect
       (call-next-method)
    (handle-stage-end stage)))

(defmethod run-task ((stage stage))
  (when (script stage)
    (funcall (script stage))))

(defmethod task-ready-p ((stage stage))
  (case (status stage)
    ((:running :stopping) NIL)
    ((:stopped :completed :errored :created :scheduled) T)))
