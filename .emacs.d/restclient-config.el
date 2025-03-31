;; Restclient configuration for 5D-GAI Intensive

(use-package restclient
  :config
  ;; Helper function to insert API key from environment
  (defun restclient-insert-api-key ()
    "Insert the API key from environment variables"
    (interactive)
    (let ((api-key (getenv "AI_STUDIO_API_KEY")))
      (if api-key
          (insert api-key)
        (message "API key not found in environment"))))

  ;; Key binding for inserting API key
  (define-key restclient-mode-map (kbd "C-c C-k") 'restclient-insert-api-key)

  ;; Security functions to filter API keys from output
  (defun filter-api-keys-from-output (output)
    "Filter API keys from restclient output."
    (let ((filtered-output output))
      ;; Filter API key from URL in request line
      (setq filtered-output 
            (replace-regexp-in-string 
             "\\(key=\\)[^&\n ]*" 
             "\\1REDACTED" 
             filtered-output))
      
      ;; Filter API key from response headers and other potential places
      (setq filtered-output 
            (replace-regexp-in-string 
             "\\(https://[^?]*\\?key=\\)[^&\n ]*" 
             "\\1REDACTED" 
             filtered-output))
      
      filtered-output))

  ;; Add advice to restclient to filter API keys from output
  (advice-add 'restclient-http-do 
              :around 
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  (let ((response-buffer (get-buffer "*HTTP Response*")))
                    (when response-buffer
                      (with-current-buffer response-buffer
                        (let ((filtered-content (filter-api-keys-from-output (buffer-string))))
                          (erase-buffer)
                          (insert filtered-content)))))
                  result))))

(use-package ob-restclient
  :config
  ;; Add a hook to org-babel-restclient to filter API keys after execution
  (defun filter-restclient-results ()
    "Filter API keys from restclient results."
    (let ((results-markers (org-babel-where-is-src-block-result)))
      (when results-markers
        (save-excursion
          (goto-char (marker-position results-markers))
          (let* ((element (org-element-at-point))
                 (content (buffer-substring-no-properties 
                           (org-element-property :contents-begin element)
                           (org-element-property :contents-end element)))
                 (filtered-content (filter-api-keys-from-output content)))
            (delete-region (org-element-property :contents-begin element)
                           (org-element-property :contents-end element))
            (goto-char (org-element-property :contents-begin element))
            (insert filtered-content))))))

  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when (string= "restclient" (org-babel-get-src-block-info t))
                (filter-restclient-results)))))

(provide 'restclient-config)
