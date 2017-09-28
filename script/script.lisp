#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.script)

(defvar *script-readtable* (copy-readtable))

(set-macro-character #\$ (lambda (s c)
                           (declare (ignore c))
                           `(autobuild-build:argument ',(read s)))
                     T *script-readtable*)

(defmethod parse-body-for-class ((class symbol) body)
  (parse-body-for-class (find-class class) body))

(defmethod parse-body-for-class ((class (eql (find-class 'autobuild-build:function-stage))) body)
  (list :func (compile NIL `(lambda () ,@body))))

(defun parse-stage (stagedef)
  (destructuring-bind (name &rest stagedef) stagedef
    (form-fiddle:with-body-options (body other
                                         depends-on
                                         (class 'autobuild-build:function-stage))
                                   stagedef
      (apply #'allocate-instance class
             :name name
             :dependencies depends-on
             (append (parse-body-for-class class body)) other))))

(defun parse-recipe (recipedef)
  (form-fiddle:with-body-options (body other
                                       (name (error "NAME required."))
                                       (repository (error "REPOSITORY required."))
                                       depends-on
                                       (class 'autobuild-build:recipe))
                                 recipedef
    (let ((stages (mapcar #'parse-stage body)))
      (dolist (stage stages)
        (let ((deps (loop for dep in (autobuild-build:dependencies stage)
                          collect (or (find dep stages :key #'autobuild-build:name
                                                       :test #'equal)
                                      (error "~a cannot depend on ~a: no such stage."
                                             stage dep)))))
          (initialize-instance stage :dependencies deps)))
      (apply #'make-instance class
             :name name
             :repository repository
             :dependencies depends-on
             :stages stages
             other))))

(defun read-script-file (file)
  (with-open-file (stream file :direction :input
                               :if-does-not-exist :error)
    (let ((*package* (find-package '#:org.shirakumo.autobuild.script.user))
          (*readtable* *script-readtable*))
      (loop with eof = (gensym)
            for token = (read stream NIL eof)
            until (eq token eof)
            collect token))))

(defun load-script (file &key (register T))
  (let ((recipe (parse-recipe (read-script-file file))))
    (when register
      (setf (autobuild-build:find-recipe (autobuild-build:name recipe) :force T) recipe))
    recipe))
