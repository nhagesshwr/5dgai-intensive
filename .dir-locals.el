;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((org-confirm-babel-evaluate . nil)
              (org-src-preserve-indentation . t)
              (org-babel-python-command . "python3")
              (org-babel-jupyter-async-inline-images . t)
              (org-export-with-toc . t)
              (org-export-with-section-numbers . nil)
              (eval . (progn
                        (org-babel-do-load-languages
                         'org-babel-load-languages
                         '((python . t)
                           (shell . t)
                           (plantuml . t)
                           (dot . t)
                           (jupyter . t)
                           (emacs-lisp . t)))))
              (org-babel-default-header-args . ((:mkdirp . "yes")
                                                (:exports . "both")
                                                (:eval . "never-export")
                                                (:results . "output replace")))))

 (python-mode . ((eval . (progn
                           (setq python-environment-directory "./venv")
                           (pyvenv-activate "./venv")))
                 (fill-column . 100)
                 (indent-tabs-mode . nil)
                 (python-indent-offset . 4)))
 
 (js-mode . ((js-indent-level . 2)
             (fill-column . 80)
             (indent-tabs-mode . nil)))
 
 (makefile-mode . ((eval . (setq-local whitespace-style '(face
                                                          tabs
                                                          spaces
                                                          trailing
                                                          space-before-tab
                                                          newline
                                                          indentation
                                                          empty
                                                          space-after-tab
                                                          space-mark
                                                          tab-mark))))))