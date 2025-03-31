;; Python configuration for 5D-GAI Intensive

;; IPython integration
(setq python-shell-interpreter "poetry"
      python-shell-interpreter-args "run ipython -i --simple-prompt")

;; Define a function to run Hy code through IPython
(defun org-babel-execute:hy (body params)
  "Execute a block of Hy code with IPython."
  (let ((org-babel-python-command "poetry run ipython -c 'import hy; hy.eval(\"\"\""))
    (org-babel-execute:python (concat body "\"\"\"')") params)))

;; Support for poetry environments
(defun set-poetry-environment ()
  "Set the python interpreter to use the poetry environment."
  (interactive)
  (let ((python-path (string-trim
                      (shell-command-to-string "poetry env info -p 2>/dev/null || echo ''"))))
    (when (and python-path (not (string= python-path "")))
      (setq python-shell-interpreter (concat python-path "/bin/python"))
      (message "Using Python at %s" python-shell-interpreter))))

;; Try to set poetry environment when opening python files
(add-hook 'python-mode-hook 'set-poetry-environment)

(provide 'python-config)
