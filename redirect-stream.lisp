#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defclass redirect-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output-stream :initarg :output-stream :accessor output-stream))
  (:default-initargs
   :output-stream (error "OUTPUT-STREAM required.")))

(defmacro define-redirect-method (name args)
  `(defmethod ,name ((stream redirect-output-stream) ,@args)
     (,name (output-stream stream) ,@args)))

(defmethod trivial-gray-streams:stream-write-char ((stream redirect-output-stream) char)
  (trivial-gray-streams:stream-write-char (output-stream stream) char))

(defmethod trivial-gray-streams:stream-write-sequence ((stream redirect-output-stream) sequence start end &key &allow-other-keys)
  (trivial-gray-streams:stream-write-sequence (output-stream stream) sequence start end))

(defmethod trivial-gray-streams:stream-write-string ((stream redirect-output-stream) string &optional start end)
  (trivial-gray-streams:stream-write-string (output-stream stream) string start end))

(defmethod trivial-gray-streams:stream-terpri ((stream redirect-output-stream))
  (trivial-gray-streams:stream-terpri (output-stream stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream redirect-output-stream))
  (trivial-gray-streams:stream-force-output (output-stream stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream redirect-output-stream))
  (trivial-gray-streams:stream-finish-output (output-stream stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream redirect-output-stream))
  (trivial-gray-streams:stream-clear-output (output-stream stream)))

(defmethod trivial-gray-streams:stream-fresh-line ((stream redirect-output-stream))
  (trivial-gray-streams:stream-fresh-line (output-stream stream)))

(defun make-redirect-output-stream (&optional stream)
  (make-instance 'redirect-output-stream :otuput-stream (or stream (make-broadcast-stream))))
