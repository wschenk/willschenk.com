;; set the directory
(setq blog-mode-base-dir "/home/wschenk/willschenk.com/content/articles")
(require 'transient)

(defun blog-mode-file-peek (pattern file)
  (let ((result (car (process-lines "awk" "-F: " (concat pattern " {print $2}") file))))
    (if result
        (replace-regexp-in-string "\"" "" result)
      "")))

(defun blog-mode-parse-org (file)
  (let ((title (blog-mode-file-peek "/\\+title/" file))
        (date (blog-mode-file-peek "/\\+date/" file))
        (draft (blog-mode-file-peek "/\\+draft/" file))
        (tags (blog-mode-file-peek "/\\+tags/" file)))

    (list file (vector title draft date tags))))

(defun blog-mode-parse-md (file)
  (let ((title (blog-mode-file-peek "/^title/" file))
        (date (blog-mode-file-peek "/^date/" file))
        (draft (blog-mode-file-peek "/^draft/" file))
        (tags (blog-mode-file-peek "/^tags/" file)))
    (list file (vector title draft date tags))))

(defun blog-mode-parse-directory (directory)
  (let ((md (concat directory "/index.md"))
        (org (concat directory "/index.org")))
    (if (file-exists-p md)
      (blog-mode-parse-md md)
      (if (file-exists-p org)
        (blog-mode-parse-org org)
        nil))))

(defun blog-mode-parse (file)
  (if (file-directory-p file)
      (blog-mode-parse-directory file)
    (let ((ex (file-name-extension file)))
      (if (string= ex "md")
          (blog-mode-parse-md file)
        (if (string= ex "org")
            (blog-mode-parse-org file)
          (message (concat "Unknown extension " ex)))))))

(defun blog-mode-refresh-data ()
  (setq blog-mode-entries nil)
  (dolist (file (process-lines "find" blog-mode-base-dir  "-maxdepth" "2" "-print"))
    (let ((entry (blog-mode-parse file)))
      (if entry
          (push (blog-mode-parse file) blog-mode-entries)))))

(define-derived-mode blog-mode tabulated-list-mode "blog-mode" "Major mode Blog Mode, to edit hugo blogs"
  (setq tabulated-list-format [("Title" 60 t)
                               ("Draft" 5 nil)
                               ("Date"  11 t)
                               ("Tags" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (use-local-map blog-mode-map)
  (tabulated-list-init-header))

(defun blog-list ()
  (interactive)
  (pop-to-buffer "*Blog Mode*" nil)
  (blog-mode)
  (blog-mode-refresh-data)
  (setq tabulated-list-entries (-non-nil blog-mode-entries))
  (tabulated-list-print t))

(defvar blog-mode-map nil "keymap for blog-mode")

(setq blog-mode-map (make-sparse-keymap))

(define-key blog-mode-map (kbd "?") 'blog-mode-help)
(define-key blog-mode-map (kbd "o") 'blog-mode-open)
(define-key blog-mode-map (kbd "<return>") 'blog-mode-open)
(define-key blog-mode-map (kbd "d") 'blog-mode-drafts)
(define-key blog-mode-map (kbd "a") 'blog-mode-all)
(define-key blog-mode-map (kbd "p") 'blog-mode-published)
(define-key blog-mode-map (kbd "r") 'blog-mode-refresh-all)
(define-key blog-mode-map (kbd "c") 'blog-mode-make-draft)
(define-key blog-mode-map (kbd "s") 'blog-mode-start-hugo)
(define-key blog-mode-map (kbd "RET") 'blog-mode-open)

(transient-define-prefix blog-mode-help ()
  "Help transient for blog mode."
  ["Blog mode help"
   ("o" "Open" blog-mode-open)
   ("d" "Drafts" blog-mode-drafts)
   ("a" "All" blog-mode-all)
   ("p" "Published" blog-mode-published)
   ("r" "Refresh" blog-mode-refresh-all)
   ("c" "Create post" blog-mode-make-draft)
   ("s" "Start hugo" blog-mode-start-hugo)
   ])

(defun blog-mode-open ()
  (interactive)
  (find-file (tabulated-list-get-id)))

(defun blog-mode-refresh-all ()
  (interactive)
  (progn
    (blog-mode-refresh-data)
    (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t)))

(defun blog-mode-all () 
  (interactive)
  (progn
    (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t)))

(defun blog-mode-drafts () 
  (interactive)
  (progn
    (setq tabulated-list-entries 
          (-filter (lambda (x)
                     (string= "true"
                              (aref (car (cdr x)) 1))) (-non-nil blog-mode-entries)))
    (tabulated-list-print t)))

(defun blog-mode-published () 
  (interactive)
  (progn
    (setq tabulated-list-entries 
          (-filter (lambda (x)
                     (string= ""
                              (aref (car (cdr x)) 1))) blog-mode-entries)))
    (tabulated-list-print t))

(defun string-title-to-filename (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (setq str (replace-regexp-in-string " " "_" str))
    (downcase str)))

(defun blog-mode-make-draft ()
  "Little function to create a org file inside of the blog"
  (interactive)
  (let* (
         (mini (yes-or-no-p "Mini post? "))
         (title (read-from-minibuffer "Title: "))
         (year (format-time-string "%Y"))
         (filename (string-title-to-filename title))
         (rootpath (concat blog-mode-base-dir "/" year "/" filename))
         (path (if mini (concat rootpath ".org") (concat rootpath "/index.org")))
         )
    (set-buffer (find-file path))
    (insert "#+title: " title "\n")
    (insert "#+date: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+draft: true\n")
    (unless mini
      (insert "\n* References\n# Local Variables:\n# eval: (add-hook 'after-save-hook (lambda ()(org-babel-tangle)) nil t)\n# End:\n"))
    )
  )

(defun blog-mode-start-hugo ()
  "Starts up a hugo watch process"
  (interactive)
  (let* (
         (default-directory "/home/wschenk/willschenk.com")
         (height (/ (frame-total-lines) 3))
         (name "*shell hugo process"))
    (delete-other-windows)
    (split-window-vertically (- height))
    (other-window 1)
    (switch-to-buffer name)
    (unless (get-buffer-process name)
      (async-shell-command "cd /home/wschenk/willschenk.com;./dev.sh" name))
    (async-shell-command "sleep 5;xdg-open http://localhost:1313" (get-buffer "*hugo web opener*"))))

(global-set-key (kbd "C-c d") 'blog-list)
