;; Package setup for 5D-GAI Intensive

;; Install essential packages if not already installed
(dolist (pkg '(use-package restclient ob-restclient hy-mode gptel))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Ensure required packages are installed with use-package
(use-package hy-mode)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc"))

(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package restclient)

(use-package ob-restclient)

(use-package gptel)

;; Company for completion
(use-package company
  :config
  (global-company-mode))

(provide 'package-setup)
