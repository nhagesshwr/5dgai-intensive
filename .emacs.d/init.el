;; 5D-GAI Intensive Emacs Configuration
;; Support for Org-mode, literate programming, and notebook-style workflows

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use-package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Org-mode configuration
(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-hide-emphasis-markers t)
  (setq org-src-preserve-indentation t)
  (setq org-startup-folded 'content)
  
  ;; Org babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t)
     (plantuml . t)
     (dot . t)
     (jupyter . t)
     (restclient . t))))

;; Org export backends
(use-package ox-hugo
  :after ox)

(use-package ox-reveal
  :after ox)

;; Notebook-style packages
(use-package jupyter)
(use-package ein)

;; Org-roam - knowledge management
(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "notes" (file-name-directory (or load-file-name buffer-file-name))))
  :config
  (org-roam-db-autosync-mode))

;; Structured editing and navigation
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; Flycheck for syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; Company for code completion
(use-package company
  :hook (after-init . global-company-mode))

;; Python support
(use-package elpy
  :init
  (elpy-enable))

;; REST client for API testing
(use-package restclient)

;; PlantUML for diagrams
(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path "/usr/bin/plantuml"))

;; Yaml mode
(use-package yaml-mode)

;; Magit for Git
(use-package magit)

;; Projectile for project management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Custom functions for org-based workflows
(defun org-tangle-files-in-dir (dir &optional exclude)
  "Tangle all org files in DIR, excluding any files matching EXCLUDE regex."
  (interactive "DDirectory: ")
  (let ((files (directory-files dir t "\\.org$")))
    (dolist (file files)
      (when (and (not (string-match-p (or exclude "") file))
                 (file-regular-p file))
        (org-babel-tangle-file file)))))

;; Prettier org-mode headings
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Auto-tangling on save
(defun org-babel-auto-tangle ()
  "Automatically tangle org files when saving."
  (when (eq major-mode 'org-mode)
    (let ((org-confirm-babel-evaluate nil))
      (when (member "tangle" (org-get-tags))
        (org-babel-tangle)))))

(add-hook 'after-save-hook 'org-babel-auto-tangle)

;; Keybindings
(global-set-key (kbd "C-c o t") 'org-babel-tangle)
(global-set-key (kbd "C-c o d") 'org-babel-detangle)
(global-set-key (kbd "C-c o e") 'org-export-dispatch)
(global-set-key (kbd "C-c o j") 'org-babel-execute-src-block)

;; Project-specific setup
(setq org-publish-project-alist
      '(("5dgai-intensive"
         :base-directory "."
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
         :recursive t
         :with-toc t
         :section-numbers nil)))

(provide 'init)