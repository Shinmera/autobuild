#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defclass synchronized-repository (repository)
  ((lock :initform (bt:make-lock) :reader repository-lock)))

(defmacro define-locked-method (name args)
  `(defmethod ,name :around ,args
     (bt:with-lock-held ((repository-lock ,(caar args)))
       (call-next-method))))

(define-locked-method clone ((from synchronized-repository) to &key))
(define-locked-method pull ((repository synchronized-repository) &key))
(define-locked-method checkout ((repository synchronized-repository) thing &key))
(define-locked-method reset ((repository synchronized-repository) &key))
(define-locked-method clean ((repository synchronized-repository) &key))
