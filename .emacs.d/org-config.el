;; Org-mode configuration for 5D-GAI Intensive

(use-package org
  :ensure nil  ;; Use built-in org
  :config
  ;; Basic org settings
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)
  (setq org-startup-folded 'content)

  ;; Fix shell execution in org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sh . t)
     (python . t)
     (restclient . t)))

  ;; Make sure sh and shell both work
  (unless (fboundp 'org-babel-execute:shell)
    (defalias 'org-babel-execute:shell 'org-babel-execute:sh))

  ;; Add support for Hy language
  (add-to-list 'org-src-lang-modes '("hy" . hy))

  ;; Define a function to run Hy code through IPython
  (defun org-babel-execute:hy (body params)
    "Execute a block of Hy code with IPython."
    (let ((org-babel-python-command "poetry run ipython -c 'import hy; hy.eval(\"\"\""))
      (org-babel-execute:python (concat body "\"\"\"')") params)))

  ;; Auto-tangling on save
  (defun org-babel-auto-tangle ()
    "Automatically tangle org files when saving."
    (when (eq major-mode 'org-mode)
      (let ((org-confirm-babel-evaluate nil))
        (when (member "tangle" (org-get-tags))
          (org-babel-tangle)))))

  (add-hook 'after-save-hook 'org-babel-auto-tangle)

  ;; Custom functions for org-based workflows
  (defun org-tangle-files-in-dir (dir &optional exclude)
    "Tangle all org files in DIR, excluding any files matching EXCLUDE regex."
    (interactive "DDirectory: ")
    (let ((files (directory-files dir t "\\.org$")))
      (dolist (file files)
        (when (and (not (string-match-p (or exclude "") file))
                   (file-regular-p file))
          (org-babel-tangle-file file))))))

(provide 'org-config)
