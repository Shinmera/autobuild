#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defun format-date (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time universal-time)
    (format NIL "~d.~d.~d ~2,'0d:~2,'0d:~2,'0d"
            yy mm dd h m s)))

(defun format-time (seconds)
  (let ((s (mod seconds 60))
        (m (floor (mod (/ seconds 60) 60)))
        (h (floor (/ seconds (* 60 60)))))
    (format NIL "~:[~d hour~:p~;~*~]~:[ ~d minute~:p~;~*~]~:[ ~d second~:p~;~*~]"
            (= 0 h) h (= 0 m) m (= 0 s) s)))

(defmacro with-open-file-no-remove ((stream pathname &rest options &key &allow-other-keys) &body body)
  `(let ((,stream (open ,pathname ,@options)))
     (unwind-protect
          ,@body
       (finish-output ,stream)
       (close ,stream))))

(define-condition destroy-warning (warning)
  ((thing :initarg :thing :reader thing))
  (:report (lambda (c s) (format s "Destroying ~a." (thing c)))))

(defgeneric destroy (thing)
  (:method :before (thing)
    (warn 'destroy-warning :thing thing))
  (:method ((repository repository))
    (when (location repository)
      (uiop:delete-directory-tree
       (location repository)
       :validate (lambda (pathname)
                   (uiop:subpathp pathname (location repository)))))))

(define-condition restore-warning (warning)
  ((thing :initarg :thing :reader thing)
   (source :initarg :source :reader source))
  (:report (lambda (c s) (format s "Restoring ~a~@[ from ~a~]." (thing c) (source c)))))

(defgeneric restore (thing source)
  (:method :before (thing source)
    (warn 'restore-warning :thing thing :source source)))

(defun read-stream-to-string (stream)
  (with-output-to-string (output)
    (let ((buffer (make-array 4096 :element-type 'character)))
      (loop for count = (read-sequence buffer stream)
            do (write-sequence buffer output :end count)
            while (= count (length buffer))))))
