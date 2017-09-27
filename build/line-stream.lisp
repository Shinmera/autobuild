#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.build)

(defclass line-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((on-line :initarg :on-line :accessor on-line)
   (buffer :initform (make-string-output-stream) :accessor buffer)
   (column :initform 0 :accessor column))
  (:default-initargs
   :on-line #'print))

(defun %write-char (stream character)
  (write-char character (buffer stream))
  (case character
    (#\Linefeed
     (setf (column stream) 0)
     (funcall (on-line stream) (get-output-stream-string (buffer stream))))
    (T
     (incf (column stream)))))

(defmethod trivial-gray-streams:stream-write-char ((stream line-stream) character)
  (%write-char stream character))

(defmethod trivial-gray-streams:stream-line-column ((stream line-stream))
  (column stream))

(defmethod trivial-gray-streams:stream-start-line-p ((stream line-stream))
  (= 0 (column stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream line-stream) string &optional start end)
  (loop for i from (or start 0) below (or end (length string))
        do (%write-char stream (aref string i))))

(defmethod trivial-gray-streams:stream-terpri ((stream line-stream))
  (%write-char stream #\Linefeed))

(defmethod trivial-gray-streams:stream-fresh-line ((stream line-stream))
  (unless (= 0 (column stream))
    (%write-char stream #\Linefeed)))

(defmethod trivial-gray-streams:stream-finish-output ((stream line-stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream line-stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream line-stream))
  (setf (column stream) 0)
  (get-output-stream-string (buffer stream)))

(defmethod trivial-gray-streams:stream-advance-to-column ((stream line-stream) column)
  (when (< column (column stream))
    (format (buffer stream) "~v@{ ~}" (- (column stream) column) NIL)
    (setf (column stream) column)))

