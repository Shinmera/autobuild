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

(defmethod parse-body-for-class ((class (eql (find-class 'autobuild-build:eval-stage))) body)
  (list :form `(progn ,@body)))

(defmethod parse-body-for-class ((class standard-class) body)
  body)

(defun parse-stage (stagedef)
  (destructuring-bind (name &rest stagedef) stagedef
    (form-fiddle:with-body-options (body other
                                         depends-on
                                         (class 'autobuild-build:eval-stage))
                                   stagedef
      (list* (allocate-instance (find-class class))
             :name name
             :dependencies depends-on
             (append (parse-body-for-class class body)
                     other)))))

(defun resolve-stages (stages)
  (loop for (stage . initargs) in stages
        for dependencies = (getf initargs dependencies)
        do (setf (getf initargs :dependencies)
                 (loop for dep in dependencies
                       for stage = (find dep stages :test #'equal
                                                    :key (lambda (a) (getf a :name)))
                       collect (first stage)))
        collect (apply #'initialize-instance stage initargs)))

(defun parse-recipe (recipedef)
  (form-fiddle:with-body-options (body other
                                       (name (error "NAME required."))
                                       (repository (error "REPOSITORY required."))
                                       depends-on
                                       (class 'autobuild-build:recipe))
                                 recipedef
    (apply #'make-instance class
           :name name
           :repository repository
           :dependencies depends-on
           :stages (resolve-stages (mapcar #'parse-stage body))
           other)))

(defun read-recipe-file (file)
  (with-open-file (stream file :direction :input
                               :if-does-not-exist :error)
    (let ((*package* (find-package '#:org.shirakumo.autobuild.script.user))
          (*readtable* *script-readtable*))
      (loop with eof = (gensym)
            for token = (read stream NIL eof)
            until (eq token eof)
            collect token))))

(defun load-recipe (file &key (register T))
  (let ((recipe (parse-recipe (read-recipe-file file))))
    (when register
      (setf (autobuild-build:find-recipe (autobuild-build:name recipe) :force T) recipe))
    recipe))
