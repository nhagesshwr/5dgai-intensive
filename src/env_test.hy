#!/usr/bin/env hy
;;; Minimal Environment Test for Google GenAI in Hy

(import os sys)
(import dotenv)

;; Load environment
(dotenv.load_dotenv)

;; Print key info
(setv GOOGLE-API-KEY (os.getenv "GOOGLE_API_KEY"))
(print f"API Key length: {(len GOOGLE-API-KEY)}, prefix: {(cut GOOGLE-API-KEY 0 4)}, suffix: {(cut GOOGLE-API-KEY -4 None)}")

;; Try imports
(try
  (import google.generativeai)
  (print "✅ Imports successful")
  (try
    (print f"Package info: {google.generativeai.__version__}")
    (except [e AttributeError]
      (print "Package info: version not available")))
  
  (except [e Exception]
    (print f"❌ Import error: {e}")
    (print "Try: pip install google-genai==1.7.0")))

(when (= __name__ "__main__")
  (print "Environment test complete"))