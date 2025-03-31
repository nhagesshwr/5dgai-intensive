#!/usr/bin/env hy

"\"
Google AI Studio - List Available Models

This script demonstrates using the Google generative AI Python package with Hy
to list available models in Google AI Studio.
\""

(import os)
(import google.generativeai :as genai)
(import dotenv)

(defn list-available-models []
  "\"
  List all available models from Google AI Studio.
  
  Returns:
      List of model information
  \""
  ;; Load environment variables
  (dotenv.load_dotenv)
  
  ;; Configure the Google GenAI API
  (setv api-key (os.getenv "AI_STUDIO_API_KEY"))
  (when (not api-key)
    (raise (ValueError "AI_STUDIO_API_KEY environment variable not set")))
  
  (genai.configure :api_key api-key)
  
  ;; List models
  (try
    (print "Available models from Google AI Studio:")
    (print "-------------------------------------")
    
    ;; Get the list of models
    (setv models (genai.list_models))
    
    ;; Iterate through available models and print details
    (for [model models]
      (print f"Name: {(. model name)}"))
    
    (except [e Exception]
      (print f"Error listing models: {e}"))))

;; Run the script
(when (= __name__ "__main__")
  (list-available-models))