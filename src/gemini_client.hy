"\"
Hy-based client for Google's Gemini API.

This module provides a Hy implementation of the Gemini API client,
demonstrating how to use Hy's Lisp-like syntax with Google's Gemini models.
\""

(import os
        json
        requests
        [dotenv [load-dotenv]]
        [typing [Dict List Any Optional Union]])

(defclass GeminiClient []
  "\"
  Client for interacting with Google's Gemini API.
  
  Attributes:
      api-key: API key for authenticating with Google AI Studio
      api-url: Base URL for the Gemini API
  \""
  
  (defn __init__ [self &optional [api-key None] 
                       &kwonly [api-url "https://generativelanguage.googleapis.com/v1beta"]]
    "\"
    Initialize the Gemini client.
    
    Args:
        api-key: Optional API key (defaults to environment variable)
        api-url: Base URL for the API
    \""
    ;; Load environment variables if API key not provided
    (load-dotenv)
    
    ;; Set the API key from parameter or environment
    (setv self.api-key (or api-key (os.getenv "AI_STUDIO_API_KEY")))
    (when (not self.api-key)
      (raise (ValueError "API key not provided and AI_STUDIO_API_KEY not found in environment")))
    
    ;; Set the API URL
    (setv self.api-url api-url))
  
  (defn generate-content [self prompt &optional 
                              [model "gemini-2.0-flash"]
                              [system None]
                              [temperature 0.7]
                              [max-tokens 1024]]
    "\"
    Generate content using the Gemini API.
    
    Args:
        prompt: The text prompt to send to the model
        model: The Gemini model to use
        system: Optional system instructions
        temperature: Controls randomness (0.0 to 1.0)
        max-tokens: Maximum number of tokens to generate
        
    Returns:
        API response dictionary
    \""
    (setv url f"{self.api-url}/models/{model}:generateContent?key={self.api-key}")
    
    ;; Build the request payload
    (setv payload {"contents" [{"parts" [{"text" prompt}]}]
                   "generationConfig" {"temperature" temperature
                                      "maxOutputTokens" max-tokens}})
    
    ;; Add system instructions if provided
    (when system
      (setv (get payload "systemInstruction") {"parts" [{"text" system}]}))
    
    ;; Send the request to the API
    (setv response (requests.post url :json payload))
    
    ;; Check for errors
    (when (not (= response.status-code 200))
      (try
        (setv error-data (response.json))
        (setv error-message (get-in error-data ["error" "message"] "Unknown error"))
        (except [e [Exception]]
          (setv error-message (.text response))))
      (raise (Exception f"API Error ({response.status-code}): {error-message}")))
    
    ;; Return the parsed response
    (response.json))
  
  (defn chat [self messages &optional 
                  [model "gemini-2.0-flash"]
                  [temperature 0.7]
                  [max-tokens 1024]]
    "\"
    Have a chat conversation with the Gemini model.
    
    Args:
        messages: List of message dictionaries with 'role' and 'content' keys
        model: The Gemini model to use
        temperature: Controls randomness (0.0 to 1.0)
        max-tokens: Maximum number of tokens to generate
        
    Returns:
        API response dictionary
    \""
    (setv url f"{self.api-url}/models/{model}:generateContent?key={self.api-key}")
    
    ;; Convert messages to Gemini format
    (setv gemini-messages [])
    (for [msg messages]
      (.append gemini-messages
               {"role" (get msg "role")
                "parts" [{"text" (get msg "content")}]}))
    
    ;; Build the request payload
    (setv payload {"contents" gemini-messages
                   "generationConfig" {"temperature" temperature
                                      "maxOutputTokens" max-tokens}})
    
    ;; Send the request to the API
    (setv response (requests.post url :json payload))
    
    ;; Check for errors
    (when (not (= response.status-code 200))
      (try
        (setv error-data (response.json))
        (setv error-message (get-in error-data ["error" "message"] "Unknown error"))
        (except [e [Exception]]
          (setv error-message (.text response))))
      (raise (Exception f"API Error ({response.status-code}): {error-message}")))
    
    ;; Return the parsed response
    (response.json))
  
  (defn extract-text [self response]
    "\"
    Extract text content from API response.
    
    Args:
        response: The API response dictionary
        
    Returns:
        Extracted text content
    \""
    (try
      (get-in response ["candidates" 0 "content" "parts" 0 "text"])
      (except [e [KeyError IndexError]]
        "No text content found in response"))))

;; Example usage when run directly
(when (= __name__ "__main__")
  (try
    (setv client (GeminiClient))
    (setv response (.generate-content client "Hello, Gemini! What can you do?"))
    (print "Response from Gemini:")
    (print (.extract-text client response))
    (except [e [Exception]]
      (print f"Error: {e}"))))