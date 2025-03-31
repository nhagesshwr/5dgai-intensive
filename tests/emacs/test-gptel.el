;; Simple gptel test with Ollama
(require 'gptel)

;; Set up Ollama backend
(setq gptel-backend
      (gptel-make-ollama "Ollama"
                         :host "localhost:11434"
                         :stream nil
                         :models '(llama3.2:latest)))

;; Define a synchronous version of gptel-request for testing
(defun gptel-request-sync (prompt)
  "Send PROMPT to LLM and return the response synchronously."
  (let ((response-received nil)
        (response-text nil))
    ;; Set up a callback to capture the response
    (gptel-request prompt
                  :callback (lambda (response _info)
                              (setq response-text response
                                    response-received t)))
    
    ;; Wait for response (with a timeout)
    (let ((timeout 30)
          (wait-time 0.1)
          (total-wait 0))
      (while (and (not response-received)
                  (< total-wait timeout))
        (sleep-for wait-time)
        (setq total-wait (+ total-wait wait-time))))
    
    ;; Return the result
    (if response-received
        response-text
      "Timeout waiting for response")))

;; Make a test request
(message "Starting gptel test with Ollama...")
(message "Response: %s" 
         (gptel-request-sync "As a scheme interpreter, evaluate: (map inc '(1 2 (+ 1 1 2)))"))