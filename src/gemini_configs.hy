#!/usr/bin/env hy

"\"
Google AI Studio - Gemini Configuration Examples in Hy

This script demonstrates different configuration options for the Gemini models,
including token limits, temperature settings, and sampling parameters,
implemented in the Hy language.
\""

(import os)
(import google.generativeai :as genai)
(import [google.generativeai [types]])
(import [dotenv [load_dotenv]])

(defn run-config-examples []
  "\"Run various Gemini configuration examples.\""
  ;; Load environment variables
  (load_dotenv)
  
  ;; Configure the Google GenAI API
  (setv api-key (os.getenv "AI_STUDIO_API_KEY"))
  (when (not api-key)
    (raise (ValueError "AI_STUDIO_API_KEY environment variable not set")))
  
  (genai.configure :api_key api-key)
  
  ;; Example 1: Token Limit Configuration
  (print (+ "=" (* 50 "=")))
  (print "EXAMPLE 1: Token Limit Configuration")
  (print (+ "=" (* 50 "=")))
  
  ;; Configure with limited output tokens
  (setv short-config (types.GenerateContentConfig :max_output_tokens 200))
  
  ;; Generate content with the short configuration
  (setv response (genai.generate_content
                   :model "gemini-2.0-flash"
                   :generation_config short-config
                   :contents "Write a 1000 word essay on the importance of olives in modern society."))
  
  (print "Response with max_output_tokens=200:")
  (print (+ "-" (* 40 "-")))
  (print (. response text))
  (print "\n")
  
  ;; Example 2: Temperature Configuration
  (print (+ "=" (* 50 "=")))
  (print "EXAMPLE 2: Temperature Configuration")
  (print (+ "=" (* 50 "=")))
  
  ;; Configure with high temperature for more randomness
  (setv high-temp-config (types.GenerateContentConfig :temperature 2.0))
  
  (print "Responses with temperature=2.0:")
  (print (+ "-" (* 40 "-")))
  
  ;; Generate multiple responses to show randomness
  (for [i (range 5)]
    (setv response (genai.generate_content
                     :model "gemini-2.0-flash"
                     :generation_config high-temp-config
                     :contents "Pick a random colour... (respond in a single word)"))
    
    (when (. response text)
      (print f"{(+ i 1)}. {(. response text)}")
      (print (+ "-" (* 25 "-")))))
  
  (print "\n")
  
  ;; Example 3: Top-P Configuration
  (print (+ "=" (* 50 "=")))
  (print "EXAMPLE 3: Top-P Configuration")
  (print (+ "=" (* 50 "=")))
  
  ;; Configure with default settings for gemini-2.0-flash
  (setv model-config (types.GenerateContentConfig
                       :temperature 1.0
                       :top_p 0.95))
  
  (setv story-prompt "You are a creative writer. Write a short story about a cat who goes on an adventure.")
  (setv response (genai.generate_content
                   :model "gemini-2.0-flash"
                   :generation_config model-config
                   :contents story-prompt))
  
  (print "Response with temperature=1.0, top_p=0.95:")
  (print (+ "-" (* 40 "-")))
  (print (. response text)))

(when (= __name__ "__main__")
  (try
    (run-config-examples)
    (except [e Exception]
      (print f"Error running examples: {e}"))))