;; ZMQ Configuration for 5D-GAI Intensive

;; Try to load zmq package if available
(when (require 'zmq nil t)
  (message "ZMQ package loaded successfully")
  
  ;; Configure zmq for Jupyter integration
  (when (require 'jupyter nil t)
    (message "Jupyter package loaded successfully")
    
    ;; Add jupyter to org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages '((jupyter . t))))
    
    ;; Configure default kernels
    (setq jupyter-default-kernel "python3")
    
    ;; Add keyboard shortcuts
    (global-set-key (kbd "C-c j") 'jupyter-run-repl)
    (global-set-key (kbd "C-c J") 'jupyter-connect-repl)))

;; Fall back if zmq not available
(unless (featurep 'zmq)
  (message "ZMQ not available - using standard REPL communication"))

(provide 'zmq-config)
