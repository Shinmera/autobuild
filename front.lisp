#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:autobuild-server)

(defvar *projects* ())

(defgeneric add-project (project)
  (:method ((project autobuild:project))
    (setf *projects* (sort (cons project *projects*) #'string< :key #'autobuild:title))))

(defgeneric remove-project (project)
  (:method ((project autobuild:project))
    (setf *projects* (remove project *projects*))))

(define-page builds "/^$" (:lquery (template "builds.ctml"))
  (r-clip:process T :projects *projects*))

(define-page build "/project/([^/]+)/build/([a-z0-9]{40})" (:uri-groups (project hash) :lquery (template "build.ctml"))
  (let ((project (project project)))
    (r-clip:process T :project project :build (build project hash))))
