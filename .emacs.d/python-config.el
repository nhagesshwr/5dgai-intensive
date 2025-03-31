;; Python configuration for 5D-GAI Intensive

(use-package python
  :config
  ;; IPython integration
  (setq python-shell-interpreter "poetry"
        python-shell-interpreter-args "run ipython -i --simple-prompt")

  ;; Define a function to run Hy code through IPython
  (defun org-babel-execute:hy (body params)
    "Execute a block of Hy code with IPython."
    (let ((org-babel-python-command "poetry run ipython -c 'import hy; hy.eval(\"\"\""))
      (org-babel-execute:python (concat body "\"\"\"')") params))))

(provide 'python-config)
