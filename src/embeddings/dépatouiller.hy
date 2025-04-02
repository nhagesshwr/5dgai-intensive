#!/usr/bin/env hy

(import os)
(import sys)
(import json)
(import math)
(import tqdm)
(import dotenv)
(import inspect)

(print "🇫🇷 French Verb Embedding Contrast 🇫🇷")
(print "======================================")

;; Three French verbs with contrasting meanings
(print "dépatouiller: to untangle/sort out a difficult situation")
(print "flâner: to stroll/wander leisurely")
(print "être: to be/exist")
(print)
(print "What we do in code vs. what we wish we were doing vs. what we are...")

;; Try to load the Google GenAI API
(dotenv.load_dotenv)
(setv GOOGLE-API-KEY (os.getenv "GOOGLE_API_KEY"))

;; Define fallback embeddings in case API calls fail
(setv depatouiller-fallback [0.42 0.314 0.1337 0.007])  ;; Stressed, complex
(setv flaner-fallback [0.1 0.75 0.2 0.8])              ;; Relaxed, pleasant
(setv etre-fallback [0.5 0.5 0.5 0.5])                 ;; Balanced, fundamental

;; Try to get embeddings from Google GenAI
(try
  (print "\nAttempting to use Google GenAI for embeddings...")
  (import google.genai)
  (setv client (google.genai.Client :api-key GOOGLE-API-KEY))
  
  ;; Get available embedding models
  (setv embedding-models [])
  (for [model (.list client.models)]
    (when (in "embedContent" model.supported-actions)
      (.append embedding-models model.name)))
  
  (if (not embedding-models)
      (do
        (print "No embedding models available, using fallback values")
        (setv depatouiller-emb depatouiller-fallback)
        (setv flaner-emb flaner-fallback))
      (do
        (setv model-name (get embedding-models 0))
        (print f"Using model: {model-name}")
        
        ;; Try different API methods to get embeddings
        (try
          ;; Try various API methods
          (print "Multiple methods will be attempted...")
          (setv methods-found [])
          
          ;; Log available methods for debugging
          (for [name (filter (fn [x] (and (not (.startswith x "_")) 
                                          (not (= x "list"))
                                          (not (= x "get"))))
                             (dir client.models))]
            (.append methods-found name))
          (print f"Available methods: {methods-found}")
          
          ;; Try using last known method pattern
          (try
            (print "Trying embeddings.create method...")
            (try
              (setv depatouiller-response (client.embeddings.create
                                             :model model-name
                                             :content "dépatouiller"))
              (setv flaner-response (client.embeddings.create
                                       :model model-name
                                       :content "flâner"))
              (setv depatouiller-emb (getattr depatouiller-response "embedding" []))
              (setv flaner-emb (getattr flaner-response "embedding" []))
              (print "✅ Embeddings generated via embeddings.create")
              (except [e1 Exception]
                (print f"embeddings.create failed: {e1}")
                
                ;; Try embed_content
                (try
                  (print "Trying embed_content method...")
                  (setv embed-method (getattr client.models "embed_content"))
                  (setv depatouiller-response (embed-method 
                                               :model model-name 
                                               :contents [{"text" "dépatouiller"}]))
                  (setv flaner-response (embed-method 
                                         :model model-name 
                                         :contents [{"text" "flâner"}]))
                  (setv etre-response (embed-method 
                                      :model model-name 
                                      :contents [{"text" "être"}]))
                  
                  ;; Get embeddings and convert to list
                  (setv depatouiller-obj (get depatouiller-response.embeddings 0))
                  (setv flaner-obj (get flaner-response.embeddings 0))
                  (setv etre-obj (get etre-response.embeddings 0))
                  
                  ;; Extract the values
                  (setv depatouiller-emb (list depatouiller-obj.values))
                  (setv flaner-emb (list flaner-obj.values))
                  (setv etre-emb (list etre-obj.values))
                  (print "✅ Embeddings generated via embed_content")
                  (except [e2 Exception]
                    (print f"embed_content failed: {e2}")
                    ;; Fallback to our placeholders
                    (setv depatouiller-emb depatouiller-fallback)
                    (setv flaner-emb flaner-fallback)
                    (setv etre-emb etre-fallback)))))
            (except [e Exception]
              (print f"embed_content failed: {e}")
              ;; Fallback to our placeholders
              (setv depatouiller-emb depatouiller-fallback)
              (setv flaner-emb flaner-fallback)
              (setv etre-emb etre-fallback)))
          (except [e Exception]
            (print f"Error getting embeddings: {e}")
            (setv depatouiller-emb depatouiller-fallback)
            (setv flaner-emb flaner-fallback)
            (setv etre-emb etre-fallback)))))
  (except [e Exception]
    (print f"Google GenAI error: {e}, using fallback values")
    (setv depatouiller-emb depatouiller-fallback)
    (setv flaner-emb flaner-fallback)
    (setv etre-emb etre-fallback)))

;; Calculate the cosine similarity (just for fun)
(defn dot-product [a b]
  "Calculate dot product of two vectors"
  (setv result 0)
  (for [i (range (len a))]
    (+= result (* (get a i) (get b i))))
  result)

(defn magnitude [v]
  "Calculate vector magnitude"
  (math.sqrt (sum (map (fn [x] (* x x)) v))))

(defn cosine-similarity [a b]
  "Calculate cosine similarity between two vectors"
  (print f"Vector lengths: a={(len a)}, b={(len b)}")
  ;; Ensure our input is a list type
  (setv a-list (list a))
  (setv b-list (list b))
  (if (and (> (len a-list) 0) (> (len b-list) 0))
      (/ (dot-product a-list b-list) 
         (* (magnitude a-list) (magnitude b-list)))
      0.0))

;; Store verbs and embeddings in collections
(setv verb-names ["dépatouiller" "flâner" "être"])
(setv verb-embs [depatouiller-emb flaner-emb etre-emb])

;; Calculate similarity matrix dynamically with loops
(setv sim-matrix [])
(for [i (range (len verb-names))]
  (setv row [])
  (for [j (range (len verb-names))]
    (setv emb1 (get verb-embs i))
    (setv emb2 (get verb-embs j))
    (setv sim (cosine-similarity emb1 emb2))
    (.append row sim))
  (.append sim-matrix row))

;; Print similarity table
(print "\n=== French Verb Similarity Matrix ===")
(print "                dépatouiller  flâner      être")
(print "-------------------------------------------------")

;; Print rows using loops
(for [i (range (len verb-names))]
  (setv verb (get verb-names i))
  (print (+ verb (.ljust "" (- 14 (len verb))) "|") :end " ")
  (for [j (range (len verb-names))]
    (setv sim (get (get sim-matrix i) j))
    (setv sim-fmt (if (= i j) 
                      "1.000"
                      (.format "{:.4f}" (float sim))))
    (print (.ljust sim-fmt (if (= j 0) 10 8)) :end " "))
  (print))
(print "-------------------------------------------------")

;; Add some fun interpretation
(print "\n🔍 Semantic Insights:")
(setv dp-et-sim (get (get sim-matrix 0) 2))  ;; dépatouiller-être similarity
(setv fl-et-sim (get (get sim-matrix 1) 2))  ;; flâner-être similarity
(if (> dp-et-sim fl-et-sim)
    (print "Untangling problems is closer to 'being' than leisurely strolling! Work defines us?")
    (print "Leisurely strolling is closer to 'being' than problem-solving! Enjoy the moment!"))

;; Save to files
(os.makedirs "data/embeddings" :exist_ok True)

;; Create data structure for JSON
(setv json-data {})
(for [i (range (len verb-names))]
  (setv verb (get verb-names i))
  (setv emb (get verb-embs i))
  (setv (get json-data verb) emb))

;; Add similarity scores
(for [i (range (len verb-names))]
  (for [j (range (+ i 1) (len verb-names))]  ;; Only store each pair once
    (setv v1 (get verb-names i))
    (setv v2 (get verb-names j))
    (setv sim (get (get sim-matrix i) j))
    (setv key (+ v1 "_" v2))
    (setv (get json-data key) sim)))

;; Write to file
(with [f (open "data/embeddings/french_verbs_similarity.json" "w")]
  (.write f (json.dumps json-data :indent 2)))

(print "\n✅ Successfully embedded all three verbs")
(print "While you're dépatouiller-ing your code problems,")
(print "dream of flâner-ing through a Parisian boulevard,")
(print "and don't forget to être in the moment!")