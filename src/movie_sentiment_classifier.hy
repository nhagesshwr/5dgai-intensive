#!/usr/bin/env hy

(import google.generativeai :as genai)
(import os)

(defn classify-movie-review [review]
  (try
    (do
      ; Configure the Gemini API
      (genai.configure :api_key (os.getenv "AI_STUDIO_API_KEY"))
      
      ; Create the model - using gemini-2.0-flash as default per project standard
      (setv model (genai.GenerativeModel "gemini-2.0-flash"))
      
      ; Prepare the prompt
      (setv prompt 
        (+ "Classify movie reviews as POSITIVE, NEUTRAL or NEGATIVE. Review: " 
           review 
           " Sentiment:"))
      
      ; Generate the response
      (setv response 
        (.generate_content model prompt))
      
      ; Extract and return the sentiment
      (print (. response text))
      (. response text))
    (except [e Exception]
      (print f"An error occurred: {e}")
      None)))

(defn main []
  ; Example review from the prompt
  (setv review "\"Her\" is a disturbing study revealing the direction humanity is headed if AI is allowed to keep evolving, unchecked. I wish there were more movies like this masterpiece.")
  
  (classify-movie-review review))

; Run the main function if the script is executed directly
(when (= __name__ "__main__")
  (main))
