;; Environment variables loader for 5D-GAI Intensive

;; Define utility functions for file path handling
(defun project-root-dir ()
  "Get the project root directory."
  (let ((current-dir (if load-file-name
                         (file-name-directory load-file-name)
                       default-directory)))
    ;; Navigate up to find project root (where .emacs.d is located)
    (expand-file-name ".." current-dir)))

;; Load .env file and set environment variables
(defun load-dotenv-file (&optional env-file)
  "Load environment variables from .env file or specified ENV-FILE."
  (interactive)
  (let* ((project-dir (project-root-dir))
         (dotenv-file (or env-file 
                          (expand-file-name ".env" project-dir))))
    (if (file-exists-p dotenv-file)
        (progn
          (with-temp-buffer
            (insert-file-contents dotenv-file)
            (goto-char (point-min))
            (let ((count 0))
              (while (re-search-forward "^\\([A-Za-z0-9_]+\\)=\\(.*\\)" nil t)
                (let ((key (match-string 1))
                      (value (match-string 2)))
                  ;; Remove quotes if present
                  (when (string-match "^\"\\(.*\\)\"$" value)
                    (setq value (match-string 1 value)))
                  (setenv key value)
                  (setq count (1+ count))))
              (message "Loaded %d environment variables from %s" count dotenv-file)))
          t)  ; Return true if file was loaded
      (message "Environment file %s not found" dotenv-file)
      nil)))  ; Return nil if file not found

;; Try to load .env file at startup
(unless (load-dotenv-file)
  (load-dotenv-file (expand-file-name ".envrc" (project-root-dir))))

;; Load from .envrc.el if it exists
(let ((envrc-el (expand-file-name ".envrc.el" (project-root-dir))))
  (when (file-exists-p envrc-el)
    (load-file envrc-el)))

(provide 'env-loader)
