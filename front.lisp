#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:autobuild-server)

(define-api autobuild/project/build/add (project commit) ()
  (let ((project (project project)))
    (ensure-build project commit :restore :if-newer))
  (redirect (referer)))

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

(define-api autobuild/project/build/delete (project build) ()
  (let ((build (build build project)))
    (destroy build))
  (redirect #@"/"))

(define-api autobuild/project/build/log (project build &optional file-position) ()
  (multiple-value-bind (text position) (log-contents (build build project) (parse-integer file-position :junk-allowed T))
    (api-output
     (alexandria:plist-hash-table
      `(:text ,text :position ,position)))))

(define-api autobuild/project/build (project build[]) ()
  (api-output
   (alexandria:alist-hash-table
    (loop for commit in build[]
          for build = (build commit project)
          collect (cons (current-commit build)
                        (alexandria:plist-hash-table
                         `(:status ,(status build)
                           :message ,(current-message build)
                           :start ,(start build)
                           :end ,(end build)
                           :duration ,(duration build))))))))

(define-api autobuild/project/pull (project) ()
  (let ((project (project project)))
    (pull project)
    (ensure-build project (current-commit project)
                  :restore :if-newer))
  (redirect (referer)))

(define-api autobuild/project/delete (project) ()
  (let ((project (project project)))
    (destroy project))
  (redirect (referer)))

(define-api autobuild/project/populate (project) ()
  (let ((project (project project)))
    (dolist (commit (commits project))
      (ensure-build project commit :restore :if-newer)))
  (redirect (referer)))

(define-api autobuild/project/clean (project) ()
  (let ((project (project project)))
    (clean project))
  (redirect (referer)))

(define-api autobuild/project/toggle-watch (project) ()
  (let ((project (project project)))
    (setf (watch project) (not (watch project))))
  (redirect (referer)))

(define-api autobuild/project/add (remote &optional name branch) ()
  (make-build-project
   (or* name (autobuild::parse-remote-name remote))
   remote :branch (or* branch "master"))
  (redirect (referer)))

(define-api autobuild/system/load (&optional sample) ()
  (api-output
   (alexandria:plist-hash-table
    `(:cpu-usage ,(cdr (assoc :cpu (system-load:cpu-usages :sample (parse-integer (or* sample "1")))))
      :ram-usage ,(system-load:ram-usage)
      :mem-usage ,(system-load:mem-usage)
      :mem-total ,(system-load:mem-total)
      :mem-free ,(system-load:mem-free)))))

(defmethod clip:clip ((project project) field)
  (ecase field
    (name (name project))
    (remote (remote-url project))
    (branch (current-branch project))
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
     (let* ((message (current-message build))
            (cutoff (min 80 (or (position #\Newline message) 80))))
       (cond ((< cutoff (length message))
              (format NIL "~a..." (subseq message 0 cutoff)))
             (T message))))
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

(define-trigger radiance:startup ()
  (initialize-autobuild))
