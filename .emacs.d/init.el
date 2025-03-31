;; 5D-GAI Intensive Emacs Configuration
;; Main initialization file

;; Fix path issues for TRAMP
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Set up package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure we have use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Configure org-babel before loading components
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (sh . t)
   (python . t)))

;; Make sure shell and sh both work
(unless (fboundp 'org-babel-execute:shell)
  (defalias 'org-babel-execute:shell 'org-babel-execute:sh))

;; Load environment variables first
(load-file (expand-file-name "env-loader.el" user-emacs-directory))

;; Load configuration components with error handling
(dolist (component '("package-setup.el" "org-config.el" "restclient-config.el" 
                    "gptel-config.el" "python-config.el" "keybindings.el"))
  (let ((file (expand-file-name component user-emacs-directory)))
    (if (file-exists-p file)
        (condition-case err
            (load-file file)
          (error (message "Error loading %s: %s" component (error-message-string err))))
      (message "Warning: %s not found" file))))

;; Project specific settings
(setq org-publish-project-alist
      '(("5dgai-intensive"
         :base-directory "."
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
         :recursive t
         :with-toc t
         :section-numbers nil)))

;; Custom-set-variables will be added by Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
