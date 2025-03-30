;; 5D-GAI Intensive Emacs Configuration - Simplified Version
;; Support for Org-mode and literate programming basics

;; Minimal Org-mode configuration for tangling
(require 'org)
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-src-preserve-indentation t)
(setq org-startup-folded 'content)

;; Org babel languages - basic set
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)))

;; Custom functions for org-based workflows
(defun org-tangle-files-in-dir (dir &optional exclude)
  "Tangle all org files in DIR, excluding any files matching EXCLUDE regex."
  (interactive "DDirectory: ")
  (let ((files (directory-files dir t "\\.org$")))
    (dolist (file files)
      (when (and (not (string-match-p (or exclude "") file))
                 (file-regular-p file))
        (org-babel-tangle-file file)))))

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