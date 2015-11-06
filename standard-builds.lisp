#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)

(defclass invalid-build (build)
  ())

(defmethod perform-build ((build invalid-build))
  (error "INVALID BUILD! Please specify a proper build type for your project."))


(defclass make-build (build)
  ((target :initarg :target :initform NIL :accessor target))
  (:default-initargs :stage-order '(:clean :build)))

(defmethod stage ((stage (eql :clean)) (build make-build))
  (run "make" (list "clean")
       :output *build-output* :error *build-output* :on-non-zero-exit :error))

(defmethod stage ((stage (eql :build)) (build make-build))
  (run "make" (when (target build) (list (target build)))
       :output *build-output* :error *build-output* :on-non-zero-exit :error))


(defclass asdf-build (build)
  ((system :initarg :system :initform NIL :accessor system)
   (operation :initarg :operation :initform :load-op :accessor operation))
  (:default-initargs :stage-order '(:build)))

(defun initialize-asdf-build (build)
  (unless (system build)
    (if (project build)
        (setf (system build) (name (project build)))
        (setf (system build) (parse-directory-name (location build))))))

(defmethod initialize-instance :after ((build asdf-build) &key)
  (initialize-asdf-build build))

(defmethod reinitialize-instance :after ((build asdf-build) &key)
  (initialize-asdf-build build))

(defmethod stage ((stage (eql :build)) (build asdf-build))
  (run "sbcl" (list "--disable-debugger"
                    "--eval"
                    (format NIL "(push ~s asdf:*central-registry*)" (location build))
                    "--eval"
                    (format NIL "(asdf:operate ~s (asdf:find-system ~s T) :verbose T :force T)"
                            (operation build) (system build))
                    "--eval"
                    (format NIL "(sb-ext:exit)"))
       :output *build-output* :error *build-output* :on-non-zero-exit :error))
