#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:autobuild-server)

(define-api autobuild/project/build/start (project build) ()
  (let ((build (build build project)))
    (when (simple-tasks:task-ready-p build)
      (simple-tasks:schedule-task build *builder*)))
  (redirect (referer)))

(define-api autobuild/project/build/stop (project build) ()
  (let ((build (build build project)))
    (when (eql :running (status build))
      (simple-tasks:interrupt-task build NIL)))
  (redirect (referer)))

(define-api autobuild/project/build/log (project build) ()
  (api-output
   (log-contents (build build project))))

(define-api autobuild/project/pull (project) ()
  (let ((project (project project)))
    (pull project)
    (ensure-current-build project))
  (redirect (referer)))

(define-api autobuild/project/toggle-watch (project) ()
  (let ((project (project project)))
    (setf (watch project) (not (watch project))))
  (redirect (referer)))

(define-api autobuild/system/load () ()
  (api-output
   `(:cpu-usage ,(system-load:cpu-usage)
     :mem-usage ,(system-load:mem-usage)
     :mem-total ,(system-load:mem-total)
     :mem-free ,(system-load:mem-free))))

(defmethod clip:clip ((project project) field)
  (ecase field
    (name (name project))
    (remote (remote-url project))
    (branch (branch project))
    (watch (watch project))
    (builds (builds project))))

(defmethod clip:clip ((build build) field)
  (ecase field
    (status
     (status build))
    (status-icon
     (case (status build)
       (:created "fa-circle-o")
       (:running "fa-cog")
       ((:stopping :stopped) "fa-times")
       (:completed "fa-check-circle")
       (:errored "fa-exclamation-triangle")))
    (remote
     ;; Implement this ourselves at some point instead of relying on GitHub
     (format NIL "~a/commit/~a" (remote-url (project build)) (current-commit build)))
    (project
     (project build))
    (commit
     (current-commit build))
    (short-commit
     (current-commit build :short T))
    (message
     (current-message build))
    (short-message
     (current-message build))
    (duration
     (if (duration build)
         (autobuild::format-time (duration build))
         "-"))
    (start
     (if (start build)
         (autobuild::format-date (start build))
         "-"))
    (log-contents
     (log-contents build))))

(define-page builds #@"/^$" (:lquery (template "projects.ctml"))
  (let ((*package* (find-package :org.shirakumo.autobuild.server)))
    (clip:process
     lquery:*lquery-master-document*
     :projects *projects*)))

(define-page build #@"/project/([^/]+)/build/([a-z0-9]{40})" (:uri-groups (project hash) :lquery (template "build.ctml"))
  (let ((*package* (find-package :org.shirakumo.autobuild.server)))
    (clip:with-clipboard-bound ((build hash (project project)))
      (clip:process-node lquery:*lquery-master-document*))))

(define-page web-fonts (#@"/static/autobuild-server/wf/(.+)" 1001) (:uri-groups (path))
  (setf (header "Cache-Control") "public, max-age=31536000")
  (setf (header "Access-Control-Allow-Origin") (string-right-trim "/" (uri-to-url #@"autobuild/" :representation :external)))
  (serve-file (static-file (format NIL "wf/~a" path))))
