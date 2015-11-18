#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defun $ (cmd)
  (legit:run "bash" (list "-c" cmd)
             :output *build-output* :error *build-output*
             :on-non-zero-exit :error))

(defun -> (file contents)
  (with-open-file (stream (merge-pathnames file *cwd*)
                          :direction :output
                          :if-exists :rename-and-delete
                          :if-does-not-exist :create)
    (write-string contents stream)
    (terpri stream)))

(defun >> (file contents)
  (with-open-file (stream (merge-pathnames file *cwd*)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-string contents stream)
    (terpri stream)))

(defun s/r (file search replace &key (all T) case-insensitive single-line multi-line)
  (let ((regex (cl-ppcre:create-scanner search :single-line-mode single-line
                                               :multi-line-mode multi-line
                                               :case-insensitive-mode case-insensitive))
        (text (alexandria:read-file-into-string file)))
    (alexandria:write-string-into-file
     (if all
         (cl-ppcre:regex-replace-all regex text replace :preserve-case T)
         (cl-ppcre:regex-replace regex text replace :preserve-case T))
     file :if-exists :rename-and-delete)))

(defvar *script-read-table* (copy-readtable))

(defun skip-whitespace (stream)
  (loop for char = (read-char stream)
        while (find char '(#\Space #\Tab #\Newline #\Linefeed))
        finally (unread-char char stream)))

(defun read-line* (stream)
  (skip-whitespace stream)
  (with-output-to-string (output)
    (loop with skipping = NIL
          for prev = #\  then char
          for char = (read-char stream NIL NIL)
          while char
          do (cond ((char= prev #\\)
                    (setf skipping NIL)
                    (case char
                      (#\a (write-char #\Bel output))
                      (#\b (write-char #\Backspace output))
                      (#\f (write-char #\Page output))
                      (#\n (write-char #\Linefeed output))
                      (#\r (write-char #\Return output))
                      (#\t (write-char #\Tab output))
                      (#\v (write-char #\Vt output))
                      (#\Return (setf skipping T))
                      (#\Newline (setf skipping T))
                      (T (write-char prev output)
                       (write-char char output))))
                   ((char= char #\\))
                   ((and skipping (char= char #\ )))
                   ((and skipping (char= prev #\Return) (char= char #\Newline)))
                   ((or (char= char #\Linefeed)
                        (char= char #\Return))
                    (return))
                   (T
                    (setf skipping NIL)
                    (write-char char output))))))

(defun read-filename (stream)
  (skip-whitespace stream)
  (with-output-to-string (output)
    (loop for prev = #\  then char
          for char = (read-char stream NIL NIL)
          while char
          do (case char
               ((#\Space #\Tab #\Linefeed #\Return)
                (if (char= prev #\\)
                    (write-char char output)
                    (return)))
               (#\\)
               (T (write-char char output))))))

(progn
  (defun read-$ (stream char)
    (declare (ignore char))
    `($ ,(read-line* stream)))
  (set-macro-character #\$ #'read-$ T *script-read-table*))

(progn
  (defun read-> (stream char)
    (declare (ignore char))
    (if (char= (peek-char NIL stream) #\>)
        (progn (read-char stream)
               `(>> ,(read-filename stream) ,(read-line* stream)))
        `(-> ,(read-filename stream) ,(read-line* stream))))
  (set-macro-character #\> #'read-> T *script-read-table*))

(defgeneric read-script (source &key &allow-other-keys)
  (:method ((stream stream) &key)
    (let ((*readtable* *script-read-table*)
          (*package* (find-package '#:org.shirakumo.autobuild.script.user)))
      (read stream)))
  (:method ((source string) &key)
    (let ((*readtable* *script-read-table*)
          (*package* (find-package '#:org.shirakumo.autobuild.script.user)))
      (read-from-string source)))
  (:method ((file pathname) &key if-does-not-exist)
    (with-open-file (stream file :direction :input
                                 :if-does-not-exist if-does-not-exist)
      (when stream (read-script stream)))))

(defgeneric write-script (script target &key &allow-other-keys)
  (:method (script (stream stream) &key)
    (let ((*package* (find-package :org.shirakumo.autobuild.script.user)))
      (write script
             :stream stream
             :readably T
             :pretty T
             :circle T
             :case :downcase)))
  (:method (script (file pathname) &key (if-exists :supsersede))
    (with-open-file (stream file
                            :direction :output
                            :if-exists if-exists
                            :if-does-not-exist :create)
      (write-script script stream))))

(defgeneric coerce-script (func)
  (:method ((func null))
    (lambda (build) (declare (ignore build))))
  (:method ((func symbol))
    (function func))
  (:method ((func function))
    func)
  (:method ((func list))
    (compile NIL `(lambda (build)
                    (flet ((perform-stage (stage &optional (build build))
                             (perform-stage stage build)))
                      (declare (ignorable #'perform-stage))
                      ,func))))
  (:method ((file pathname))
    (coerce-script (read-script file)))
  (:method ((stream stream))
    (coerce-script (read-script stream)))
  (:method ((source string))
    (coerce-script (read-script source))))

(defclass script-class (standard-class)
  ()
  (:documentation "Metaclass for classes with script slots."))

(defmethod c2mop:validate-superclass ((class script-class) (superclass t)) NIL)
(defmethod c2mop:validate-superclass ((class standard-class) (superclass script-class)) NIL)
(defmethod c2mop:validate-superclass ((class script-class) (superclass standard-class)) T)
(defmethod c2mop:validate-superclass ((class script-class) (superclass script-class)) T)

(defclass script-slot ()
  ((script-p :initarg :script :initform NIL :reader script-p))
  (:documentation "Superclass for slots that act as a script."))

(defmethod print-object ((slot script-slot) stream)
  (print-unreadable-object (slot stream :type T :identity T)
    (format stream "~s ~s ~s" (c2mop:slot-definition-name slot) :script (script-p slot))))

(defclass script-direct-slot-definition (script-slot c2mop:standard-direct-slot-definition)
  ()
  (:default-initargs :initform NIL :initfunction (constantly NIL)))

(defclass script-effective-slot-definition (script-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:compute-effective-slot-definition ((class script-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'script-direct-slot-definition)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (setf (slot-value effective-slot 'script-p)
                     (script-p direct-slot))
               (return)))
    effective-slot))

(defmethod c2mop:direct-slot-definition-class ((class script-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'script-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class script-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'script-effective-slot-definition))

(defmethod (setf c2mop:slot-value-using-class) (new-value (class script-class) object (slotd script-slot))
  (if (script-p slotd)
      (call-next-method (coerce-script new-value) class object slotd)
      (call-next-method)))
