(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; Backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; https://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil)

;; https://www.emacswiki.org/emacs/TabStopList
(setq tab-stop-list (number-sequence 2 120 2))

(load-file "~/.emacs.d/functions.el")
(load-file "~/.emacs.d/shortcuts.el")
(load-file "~/.emacs.d/blog.el")

(package-initialize)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("stable melpa" . "http://stable.melpa.org/packages/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Startup Screen
(use-package all-the-icons
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  )

;; Org mode
(use-package org
  :ensure t
  :bind (
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (require 'org-tempo)
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (setq org-log-done t) ; Set time for when things were completed
  (setq org-hide-emphasis-markers t) ; Not show typographical commands
  (setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
  )

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("i" "Idea" entry (file+headline "~/org/notes.org" "Ideas")
         "* TODO %?\n  %i\n  %a")
        ("w" "Work Task" entry (file+datetree "~/org/work.org")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; elfeed

(defun elfeed-eww-browse ()
  "Wrapper to open eww and mark elfeed as read"
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww-browse-url link))))

(use-package elfeed
  :ensure t
  :bind (
	 ("C-x w" . elfeed))
  :config
  (define-key elfeed-show-mode-map (kbd "B") 'elfeed-eww-browse)
  )

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package modus-vivendi-theme
  :ensure t
  :config
  (load-theme 'modus-vivendi 'no-confirm))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :ensure t
  :hook (org-load . org-pdftools-setup-link))

(use-package helm-ls-git
  :ensure t
  :bind (
         ( "C-x C-d" . helm-browse-project ))
  :config
  (require 'helm-config))

(use-package helm-ag
  :ensure t
  :bind
         ( "C-x f" . helm-ag-project-root))
  
(use-package company
  :ensure t
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "PATH"))

(use-package docker
  :ensure t
  :bind ("C-c C-d" . docker))

(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "PATH"))
