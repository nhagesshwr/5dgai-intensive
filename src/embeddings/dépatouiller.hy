#!/usr/bin/env hy
;;; Minimal French verb embedding comparison
;;; Just three verbs: dépatouiller (untangle), flâner (stroll), être (be)

(import os json math)
(import dotenv)
(import google.genai)

;; Load env and API key
(dotenv.load_dotenv)
(setv API-KEY (os.getenv "GOOGLE_API_KEY"))

;; Define our three verbs
(setv verbs ["dépatouiller" "flâner" "être"])
(setv meanings ["untangle problems" "stroll leisurely" "to be/exist"])

(print "🇫🇷 French Verb Embedding Contrast 🇫🇷")
(print "--------------------------------------")
(for [i (range (len verbs))]
  (print (+ (get verbs i) ": " (get meanings i))))
(print)

;; Initialize Google GenAI client & get embeddings
(setv client (google.genai.Client :api-key API-KEY))
(setv embed-method (getattr client.models "embed_content"))

;; Get embeddings for each verb
(setv embeddings {})
(for [verb verbs]
  (print "Getting embedding for" verb "..." :end " ")
  (try
    (setv response (embed-method 
                    :model "models/embedding-001"
                    :contents [{"text" verb}]))
    (setv obj (get response.embeddings 0))
    (setv (get embeddings verb) (list obj.values))
    (print "✓")
    (except [e Exception]
      (print "Failed!")
      (setv (get embeddings verb) []))))

;; Cosine similarity function
(defn cosine-sim [a b]
  (setv dot-prod (sum (map (fn [x y] (* x y)) a b)))
  (setv mag-a (math.sqrt (sum (map (fn [x] (* x x)) a))))
  (setv mag-b (math.sqrt (sum (map (fn [x] (* x x)) b))))
  (/ dot-prod (* mag-a mag-b)))

;; Calculate similarity matrix
(print "\n📊 Similarity Matrix:")
(print "              " :end "")
(for [v verbs]
  (print (.ljust (cut v 0 5) 7) :end ""))
(print)
(print "          " :end "")
(print "-" (* 7 (len verbs)))

(for [v1 verbs]
  (print (.ljust (cut v1 0 5) 10) :end "")
  (for [v2 verbs]
    (setv emb1 (get embeddings v1))
    (setv emb2 (get embeddings v2))
    (if (and emb1 emb2)
      (do
        (setv sim (cosine-sim emb1 emb2))
        (setv sim-str (if (= v1 v2) 
                           "1.00" 
                           (.format "{:.2f}" (float sim))))
        (print (.ljust sim-str 7) :end ""))
      (print "---    " :end "")))
  (print))

;; Insight
(setv d-e-sim (cosine-sim (get embeddings "dépatouiller") (get embeddings "être")))
(setv f-e-sim (cosine-sim (get embeddings "flâner") (get embeddings "être")))
(print)
(print "🔍 Insight: " :end "")
(if (> d-e-sim f-e-sim)
    (print "Untangling problems is closer to 'being' than strolling!")
    (print "Strolling is closer to 'being' than problem-solving!"))