;; GPTel configuration for 5D-GAI Intensive

(use-package gptel
  :config
  ;; Basic configuration
  (setq gptel-default-mode 'org-mode)  ; Set default response mode to org-mode
  (setq gptel-model-history nil)       ; Don't save model history between sessions

  ;; Configure available models
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(llama3.2:latest))

  ;; Set Gemini as the default model
  (setq
   gptel-model 'gemini-pro
   gptel-backend (gptel-make-gemini "Gemini"
                  :key (getenv "GEMINI_API_KEY")
                  :stream t))

  ;; Add GitHub/Azure models
  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key (getenv "GITHUB_TOKEN")
    :models '(gpt-4o))

  ;; Integrate with org-babel
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages '((gptel . t)))))

  ;; Helper functions
  (defun gptel-send-region-or-block ()
    "Send the current region or org block to gptel"
    (interactive)
    (if (use-region-p)
        (gptel-send (region-beginning) (region-end))
      (when (eq major-mode 'org-mode)
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (when (eq type 'src-block)
            (let ((begin (org-element-property :begin element))
                  (end (org-element-property :end element)))
              (gptel-send begin end)))))))

  ;; Set prompt templates
  (setq gptel-prompt-templates
        '(("5DGAI-Agent" . "You are an AI assistant helping with the 5D Gen AI Intensive Course.
You'll be helping with coding, API interactions, and explanations of AI concepts.
The user is working in Emacs with org-mode and Babel for literate programming.

%s")
          ("Code-Helper" . "You are a coding assistant. Help debug, explain, or generate code.
Focus on clear, efficient solutions with good documentation.

%s")
          ("API-Explorer" . "You are an API exploration assistant. Help the user understand and use 
the Gemini API and other AI APIs through restclient examples.

%s"))))

(provide 'gptel-config)
