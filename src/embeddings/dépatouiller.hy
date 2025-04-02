#!/usr/bin/env hy
;;; Word Similarity: Labor, Leisure, and Existence
;;; A simple exploration of how AI understands the meanings of words

;; Import just what we need
(import os dotenv google.genai math)
(dotenv.load_dotenv)

;; ======== WORDS AND THEIR MEANINGS ========
;; These three verbs represent different aspects of human experience
(setv the_words ["dépatouiller" "flâner" "être"])  ;; French words
(setv meanings ["to untangle/solve a difficult problem"   ;; Working hard (labor)
               "to stroll leisurely without a goal"       ;; Taking it easy (leisure)
               "to be, to exist"])                        ;; Simply existing (existence)

;; Show what we're exploring
(print "\n📝 EXPLORING WORD MEANING RELATIONSHIPS 📝")
(print "========================================")
(print "Can AI understand the relationship between:")
(for [i (range (len the_words))]
  (print (+ "  • " (get the_words i) " — " (get meanings i))))
(print)
(print "Let's find out by calculating the distances between these concepts...")

;; Get AI's understanding of these words through embeddings
(print "\nStep 1: Converting words to number patterns (embeddings)")
(print "-------------------------------------------------------")
(setv client (google.genai.Client :api-key (os.getenv "GOOGLE_API_KEY")))
(setv embeddings {})

;; Get the numerical patterns for each word
(for [word the_words]
  (print f"  Getting pattern for '{word}'... " :end "")
  (try
    (setv embed-method (getattr client.models "embed_content"))
    (setv response (embed-method 
                      :model "models/embedding-001"
                      :contents [{"text" word}]))
    (setv embedding-obj (get response.embeddings 0))
    (setv (get embeddings word) (list embedding-obj.values))
    (print "✓")
    (except [e Exception]
      (print "couldn't get pattern -" e)
      ;; Use a dummy embedding for testing
      (setv (get embeddings word) [0.1 0.2 0.3 0.4]))))

;; Our measure of similarity
(defn word-similarity [word1 word2]
  (setv pattern1 (get embeddings word1))
  (setv pattern2 (get embeddings word2))
  (setv dot-product (sum (map (fn [x y] (* x y)) pattern1 pattern2)))
  (setv magnitude1 (math.sqrt (sum (map (fn [x] (* x x)) pattern1))))
  (setv magnitude2 (math.sqrt (sum (map (fn [x] (* x x)) pattern2))))
  (/ dot-product (* magnitude1 magnitude2)))

;; Calculate similarities
(print "\nStep 2: How closely related are these words?")
(print "------------------------------------------")
(print "  • On a scale of 0.0 (completely different) to 1.0 (identical):")
(print)

;; Find the relationships between the words
(setv labor-leisure (word-similarity "dépatouiller" "flâner"))
(setv labor-being (word-similarity "dépatouiller" "être"))
(setv leisure-being (word-similarity "flâner" "être"))

;; Show the results in a small, easy-to-read table
(print "              | Work        | Leisure     | Existence")
(print "              | (dépatouil) | (flâner)    | (être)")
(print "-------------------------------------------------------")
(setv ll-fmt (.format "{:.2f}" (float labor-leisure)))
(setv lb-fmt (.format "{:.2f}" (float labor-being)))
(setv eb-fmt (.format "{:.2f}" (float leisure-being)))

(print "Work         | 1.00        |" ll-fmt "|" lb-fmt)
(print "Leisure      |" ll-fmt "| 1.00        |" eb-fmt)
(print "Existence    |" lb-fmt "|" eb-fmt "| 1.00")

;; What does this tell us?
(print "\nStep 3: What does this mean?")
(print "-------------------------")
(if (> labor-being leisure-being)
    (do
      (print "  🤔 Interestingly, in AI's understanding,")
      (print "     WORKING (dépatouiller) is closer to EXISTING (être)")
      (print "     than LEISURE (flâner) is."))
    (if (> leisure-being labor-being)
        (do
          (print "  🤔 Interestingly, in AI's understanding,")
          (print "     LEISURE (flâner) is closer to EXISTING (être)")
          (print "     than WORKING (dépatouiller) is."))
        (do
          (print "  🤔 Interestingly, in AI's understanding,")
          (print "     WORKING and LEISURE are equally related to EXISTING."))))

;; A philosophical tidbit
(print)
(print "Maybe this tells us something about human existence?")
(print "Is our being defined more by our labor, our leisure, or both?")
(print "What do you think?")