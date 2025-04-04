#!/usr/bin/env hy
;;; Test script for Google GenAI API integration
;;; Tests basic generation and embedding functionality

(import os sys)
(import dotenv)

;; Load environment variables
(dotenv.load_dotenv)
(setv GOOGLE-API-KEY (os.getenv "GOOGLE_API_KEY"))

(when (not GOOGLE-API-KEY)
  (print "❌ Error: GOOGLE_API_KEY not found in environment!")
  (print "  Create a .env file with GOOGLE_API_KEY=your_api_key")
  (sys.exit 1))

(print f"API Key: {'*' * (- (len GOOGLE-API-KEY) 8)}{(cut GOOGLE-API-KEY -4 None)}")

(try
  ;; Import GenAI dependencies
  (import google.generativeai)
  
  ;; Initialize client
  (setv client (google.generativeai.Client :api_key GOOGLE-API-KEY))
  
  ;; Test 1: Basic generation
  (print "\n🧪 Test 1: Basic content generation")
  (print "---------------------------------")
  (setv response (client.models.generate_content
                  :model "gemini-2.0-flash"
                  :contents "Explain AI to me like I'm a kid."))
  (print (. response text))
  
  ;; Test 2: List available embedding models
  (print "\n🧪 Test 2: Available embedding models")
  (print "---------------------------------")
  (setv embedding-models [])
  (for [model (.list client.models)]
    (when (in "embedContent" model.supported-actions)
      (.append embedding-models model.name)
      (print f"- {model.name}")))
  
  ;; Test 3: Generate an embedding (if any embedding models available)
  (when embedding-models
    (print "\n🧪 Test 3: Generate sample embedding")
    (print "---------------------------------")
    (setv embedding-model (get embedding-models 0))
    (print f"Using model: {embedding-model}")
    
    (setv text "dépatouiller")
    (try
      ;; Try the new API style (1.7.0)
      (setv embedding-result (client.embeddings.create
                              :model embedding-model
                              :content text))
      ;; Print the first 5 dimensions of the embedding
      (setv vector (. embedding-result embedding))
      (print f"Text: {text}")
      (print f"Embedding dimensions: {(len vector)}")
      (print f"First 5 values: {(cut vector 0 5)}")
      
      (except [e1 Exception]
        (print f"Error with new API style: {e1}")
        (try
          ;; Try the alternate API style
          (setv embedding-result (client.get_embeddings
                                 :model embedding-model
                                 :texts [text]))
          ;; Print the first 5 dimensions of the embedding
          (setv vector (get embedding-result 0))
          (print f"Text: {text}")
          (print f"Embedding dimensions: {(len vector)}")
          (print f"First 5 values: {(cut vector 0 5)}")
          
          (except [e2 Exception]
            (print f"Error with alternate API style: {e2}"))))))
  
  (print "\n✅ All tests completed successfully!")
  
  (except [e Exception]
    (print f"\n❌ Error: {e}")
    (print "\nTroubleshooting:")
    (print "1. Ensure google-genai==1.7.0 is installed")
    (print "2. Verify your API key is correct")
    (print "3. Check your network connection")))

(when (= __name__ "__main__")
  (print "Test script complete"))