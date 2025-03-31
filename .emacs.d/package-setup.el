;; Package setup for 5D-GAI Intensive

;; Ensure required packages are installed
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

;; Magit for git integration
(use-package magit)

;; Company for completion
(use-package company
  :config
  (global-company-mode))

(provide 'package-setup)
