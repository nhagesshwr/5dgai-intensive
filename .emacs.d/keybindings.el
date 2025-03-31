;; Keybindings for 5D-GAI Intensive

;; Org-mode keybindings
(global-set-key (kbd "C-c o t") 'org-babel-tangle)
(global-set-key (kbd "C-c o d") 'org-babel-detangle)
(global-set-key (kbd "C-c o e") 'org-export-dispatch)
(global-set-key (kbd "C-c o j") 'org-babel-execute-src-block)

;; GPTel keybindings
(global-set-key (kbd "C-c g") 'gptel)
(global-set-key (kbd "C-c C-g") 'gptel-send)

;; Restclient keybindings
(with-eval-after-load 'restclient-mode
  (define-key restclient-mode-map (kbd "C-c C-r") 'restclient-http-send-current))

;; Project navigation
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p g") 'project-find-regexp)

(provide 'keybindings)
