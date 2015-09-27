#|
 This file is a part of autobuild
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.autobuild)





(defmethod poll ((project project))
  (let ((commit (legit:current-commit project)))
    (flet ((poll ()
             (format T "~&; Polling ~a~&" project)
             (legit:pull project)
             (let ((commit (legit:current-commit project)))
               (format T "~&; Commit  ~a~&" commit)
               commit)))
      (loop for new-commit = (poll)
            do (when (string/= new-commit commit)
                 (setf commit new-commit)
                 (format T "~&; Launching build...~&")
                 (build project))
               (sleep 60)))))



;; A build should be started in a separate directory
;; named with the commit ID. Cloned from the original directory.
;; Add a server to show status of current builds and so on.
