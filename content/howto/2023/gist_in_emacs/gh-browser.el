(require 'gh)
(require 'gh-repos)
(require 'gh-cache)

(defun starred-gh-repos-get-api ()
  (gh-repos-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun showstarred ()
  (interactive)
  (let* ((bufname "*starredrepos*")
         (api (starred-gh-repos-get-api))
         (response (gh-repos-starred-list api)))
    (switch-to-buffer bufname)
    (message response)))

(get-buffer "*starred*")

(showstarred)

(defmethod ghjn-repos-starred-list ((api gh-repos-api) &optional username)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api repo-cls)) "GET"
   (format "/users/%s/starred" (or username (gh-api-get-username api)))))

(defun gist-get-api (&optional sync)
  (let ((gh-profile-current-profile
         (or gh-profile-current-profile (gh-profile-completing-read))))
    (make-instance 'gh-gist-api :sync sync :cache t :num-retries 1)))
