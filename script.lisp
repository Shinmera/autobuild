#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.script)

(defun $ (cmd)
  (legit:run "bash" (list "-c" cmd)
             :output *build-output* :error *build-output*
             :on-non-zero-exit :error))

(defun -> (file contents)
  (with-open-file (stream (merge-pathnames file (uiop:getcwd))
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun >> (file contents)
  (with-open-file (stream (merge-pathnames file (uiop:getcwd))
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (fresh-line stream)
    (write-string contents stream)))

(defun s/r (file search replace &key (all T) case-insensitive single-line multi-line)
  (with-open-file (stream (merge-pathnames file (uiop:getcwd))
                          :direction :io
                          :if-exists :supersede
                          :if-does-not-exist :error)
    (let ((regex (cl-ppcre:create-scanner search :single-line-mode single-line
                                                 :multi-line-mode multi-line
                                                 :case-insensitive-mode case-insensitive))
          (text (with-output-to-string (datum)
                  (let ((buffer (make-array 4096 :element-type 'character)))
                    (loop for byte-count = (read-sequence buffer stream)
                          do (write-sequence buffer datum :end byte-count)
                          while (= byte-count 4096))))))
      (write-string
       (if all
           (cl-ppcre:regex-replace-all regex text replace :preserve-case T)
           (cl-ppcre:regex-replace regex text replace :preserve-case T))
       stream))))

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
                      (#\Newline (setf skipping T))
                      (T (write-char prev output)
                       (write-char char output))))
                   ((char= char #\\))
                   ((char= char #\Newline)
                    (return))
                   ((and skipping (char= char #\ )))
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

(defun read-script (stream)
  (let ((*readtable* *script-read-table*)
        (*package* (find-package '#:org.shirakumo.autobuild.script.user)))
    (read stream)))

(defun read-script-file (file &key if-does-not-exist)
  (with-open-file (stream file :direction :input
                               :if-does-not-exist if-does-not-exist)
    (when stream (read-script stream))))
