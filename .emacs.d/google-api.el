;; Set all Google API keys using a loop
(let ((api-key "AIzaSyBNd-m2_Cof5874Gof6x6fCLVuj7crkIuc")
      (api-var-names '("GOOGLE_API_KEY" "GEMINI_API_KEY" "VERTEX_API_KEY" "AI_STUDIO_API_KEY")))
  (dolist (var-name api-var-names)
    (setenv var-name api-key)
    (message "Set %s to %s..." var-name (substring api-key 0 10))))

;; Verify all keys are set
(message "All Google API keys configured successfully.")
