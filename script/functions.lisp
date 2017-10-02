#|
 This file is a part of Autobuild
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild.script)

(defun rpath (pathname)
  (merge-pathnames program simple-inferiors:*cwd*))

(defun resolve-arg (arg &optional stream)
  (typecase arg
    (pathname (format stream "~a" (uiop:native-namestring arg)))
    (string (format stream "~a" arg))
    (T (format stream "~s" arg))))

(defun -- (&rest contents)
  (fresh-line)
  (dolist (content contents)
    (resolve-arg content T)))

(defun >> (file &rest contents)
  (with-open-file (stream (rpath file)
                          :direction :output
                          :if-exists :append
                          :if-does-not-not-exist :create)
    (fresh-line stream)
    (dolist (content contents)
      (resolve-arg content stream))))

(defun ./ (program &rest args)
  (simple-inferiors:run (rpath program) (mapcar #'resolve-arg args)
                        :input T :error T
                        :on-non-zero-exit :error
                        :copier :line))

(defun sed (search replace file &optional (target file))
  (let* ((contents (uiop:read-file-string (rpath file)))
         (replaced (cl-ppcre:regex-replace-all search contents replace)))
    (with-open-file (target (rpath target)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string replaced target))))

(defun cp (&rest files)
  (let ((files (butlast files))
        (target (rpath (car (last files)))))
    (if (uiop:directory-pathname-p target)
        (dolist (file files)
          (uiop:copy-file (rpath file) (make-pathname :name (pathname-name file)
                                                      :type (pathname-type file)
                                                      :defaults target)))
        (dolist (file files)
          (uiop:copy-file (rpath file) target)))))

(defun rm (&rest files)
  (loop for file in files
        for path = (rpath file)
        do (if (uiop:probe-file* path)
               (if (uiop:directory-pathname-p path)
                   (uiop:delete-directory-tree path :validate (constantly T))
                   (delete-file path))
               (v:warn :autobuild.build "Cannot remove ~a: path does not exist" path))))
