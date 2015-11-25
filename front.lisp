#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:autobuild-server)

(defmacro define-ab-api (name args opts &body body)
  `(define-api ,name ,args ,opts
     (cond ((permitted)
            (or (progn ,@body)
                (if (string= (post/get "browser") "true")
                    (redirect (referer))
                    (api-output NIL))))
           (T
            (if (string= (post/get "browser") "true")
                (redirect (format NIL "~a?error=Not permitted." (cut-get-part (referer))))
                (api-output T :status 403 :message "Not permitted."))))))

(define-ab-api autobuild/project/build/add (project commit) ()
  (when (permitted)
    (let ((project (project project)))
      (when project
        (ensure-build project commit :restore :if-newer))))
  NIL)

(define-ab-api autobuild/project/build/start (project build) ()
  (let ((build (build build project)))
    (when (and build (simple-tasks:task-ready-p build))
      (simple-tasks:schedule-task build *builder*)))
  NIL)

(define-ab-api autobuild/project/build/stop (project build) ()
  (let ((build (build build project)))
    (when build
      (simple-tasks:interrupt-task build NIL)))
  NIL)

(define-ab-api autobuild/project/build/rejuvenate (project build) ()
  (let ((build (build build project)))
    (when build
      (rejuvenate build)))
  (if (string= (post/get "browser") "true")
      (redirect #@"/")
      (api-output NIL)))

(define-ab-api autobuild/project/build/delete (project build) ()
  (let ((build (build build project)))
    (when build
      (destroy build)))
  (if (string= (post/get "browser") "true")
      (redirect #@"/")
      (api-output NIL)))

(define-ab-api autobuild/project/build/log (project build &optional file-position) ()
  (let ((build (build build project))
        (pos (parse-integer (or* file-position "0") :junk-allowed T)))
    (when build
      (multiple-value-bind (text position) (log-contents build pos)
        (api-output
         (alexandria:plist-hash-table
          `(:text ,text :position ,position)))))))

(define-ab-api autobuild/project/build/update-recipe (project build recipe) ()
  (let ((build (build build project)))
    (when (and build recipe)
      (restore build recipe)))
  NIL)

(define-ab-api autobuild/project/build (project build[]) ()
  (api-output
   (alexandria:alist-hash-table
    (loop for commit in build[]
          for build = (build commit project)
          when build
          collect (cons (current-commit build)
                        (alexandria:plist-hash-table
                         `(:status ,(status build)
                           :message ,(current-message build)
                           :start ,(start build)
                           :end ,(end build)
                           :duration ,(duration build)
                           :stages ,(loop with table = (make-hash-table)
                                          for (name stage) on (stages build) by #'cddr
                                          do (setf (gethash (string-downcase name) table)
                                                   (alexandria:plist-hash-table
                                                    `(:status ,(status stage)
                                                      :start ,(start stage)
                                                      :end ,(end stage)
                                                      :duration ,(duration stage))))
                                          finally (return table)))))))))

(define-ab-api autobuild/project/pull (project) ()
  (let ((project (project project)))
    (when project
      (pull project)
      (ensure-build project (current-commit project)
                    :restore :if-newer)))
  NIL)

(define-ab-api autobuild/project/delete (project) ()
  (let ((project (project project)))
    (when project
      (destroy project)))
  NIL)

(define-ab-api autobuild/project/populate (project) ()
  (let ((project (project project)))
    (when project
      (loop for commit in (commits project)
            for i from 0 below 20
            do (ensure-build project commit :restore :if-newer))))
  NIL)

(define-ab-api autobuild/project/clean (project) ()
  (let ((project (project project)))
    (when project
      (clean project)))
  NIL)

(define-ab-api autobuild/project/toggle-watch (project) ()
  (let ((project (project project)))
    (when project
      (setf (watch project) (not (watch project)))))
  NIL)

(define-ab-api autobuild/project/add (remote &optional name branch) ()
  (make-build-project
   (or* name (autobuild::parse-remote-name remote))
   remote :branch (or* branch "master"))
  NIL)

(define-ab-api autobuild/system/load (&optional sample) ()
  (api-output
   (alexandria:plist-hash-table
    `(:cpu-usage ,(cdr (assoc :cpu (system-load:cpu-usages :sample (parse-integer (or* sample "1")))))
      :ram-usage ,(system-load:ram-usage)
      :mem-usage ,(system-load:mem-usage)
      :mem-total ,(system-load:mem-total)
      :mem-free ,(system-load:mem-free)))))

(defun permitted ()
  (or (radiance:config-tree :standalone)
      (let ((user (auth:current)))
        (and user (user:check user (perm :autobuild :admin))))))

(defmethod clip:clip ((timed-task timed-task) field)
  (case field
    (duration
     (duration timed-task))
    (duration-formatted
     (if (duration timed-task)
         (autobuild::format-time (duration timed-task))
         "-"))
    (start
     (start timed-task))
    (start-formatted
     (if (start timed-task)
         (autobuild::format-date (start timed-task))
         "-"))
    (end
     (end timed-task))
    (end-formatted
     (case (end timed-task)
       ((NIL) "-")
       (T (autobuild::format-date (end timed-task)))))
    (T (call-next-method))))

(defmethod clip:clip ((repository legit:repository) field)
  (case field
    (remote (remote-url repository))
    (branch (current-branch repository))
    (commit
     (current-commit repository))
    (short-commit
     (current-commit repository :short T))
    (message
     (current-message repository))
    (short-message
     (let* ((message (current-message repository))
            (cutoff (min 80 (or (position #\Newline message) 80))))
       (cond ((< cutoff (length message))
              (format NIL "~a..." (subseq message 0 cutoff)))
             (T message))))
    (committed
     (legit:current-age repository))
    (committed-formatted
     (autobuild::format-date (legit:current-age repository)))
    (T (call-next-method))))

(defmethod clip:clip ((project project) field)
  (case field
    (name (name project))
    (watch (watch project))
    (builds (builds project))
    (T (call-next-method))))

(defmethod clip:clip ((build build) field)
  (case field
    (status
     (status build))
    (status-icon
     (case (status build)
       (:created "fa-circle-o")
       (:scheduled "fa-ellipsis-h")
       (:running "fa-cog")
       (:stopping "fa-ellipsis-h")
       (:stopped "fa-dot-circle-o")
       (:completed "fa-check-circle")
       (:errored "fa-exclamation-triangle")))
    (remote
     ;; Implement this ourselves at some point instead of relying on GitHub
     (format NIL "~a/commit/~a" (remote-url (project build)) (current-commit build)))
    (project
     (project build))
    (location
     (uiop:native-namestring
      (uiop:enough-pathname (location build) *base-project-dir*)))    
    (log-data
     (multiple-value-list (log-contents build)))
    (recipe
     (recipe build))
    (recipe-file
     (uiop:native-namestring
      (uiop:enough-pathname (discover-recipe build :default T) *base-project-dir*)))
    (stages
     (loop for (key val) on (stages build) by #'cddr collect val))
    (T (call-next-method))))

(defmethod clip:clip ((stage stage) field)
  (case field
    (name
     (name stage))
    (T (call-next-method))))

(define-page builds #@"/^$" (:lquery (template "projects.ctml"))
  (let ((*package* (find-package :org.shirakumo.autobuild.server)))
    (clip:process lquery:*lquery-master-document* :projects *projects*)))

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

(radiance:remove-uri-dispatcher 'radiance:welcome)
(user:add-default-permission (perm :autobuild :admin))
