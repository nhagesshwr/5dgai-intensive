#!/usr/bin/env hy

(import os)
(import sys)
(import json)
(import pickle)

(defn load-verbs [path]
  "Load verbs from file - simple slurp approach"
  (with [f (open path)]
    (lfor line (.split (.read f) "\n") 
          :if (.strip line)
          (.strip line))))

(defn main []
  (print "French Verb Embeddings - Simple Test")
  
  ;; Load the 20 test verbs
  (setv verbs-path "/home/jwalsh/projects/aygp-dr/5dgai-intensive/resources/verbs/test_french_verbs.txt")
  (setv verbs (load-verbs verbs-path))
  (print f"Loaded {(len verbs)} French verbs:")
  (print (.join ", " verbs))
  
  ;; Create embeddings directory
  (setv output_dir "data/embeddings")
  (os.makedirs output_dir :exist_ok True)
  
  ;; Create dummy embeddings (4-dimensional vectors) for testing
  (print "Creating dummy embeddings (for testing purposes)...")
  (setv embeddings [])
  (for [verb verbs]
    (.append embeddings [0.1 0.2 0.3 0.4]))
  
  ;; Save as JSON
  (setv embeddings_dict {})
  (for [[i verb] (enumerate verbs)]
    (setv (get embeddings_dict verb) (get embeddings i)))
  
  (setv json_path (os.path.join output_dir "verb_embeddings.json"))
  (with [f (open json_path "w")]
    (.write f (json.dumps embeddings_dict :indent 2)))
  (print f"Dummy embeddings saved as JSON: {json_path}")
  
  ;; Save as pickle
  (setv pickle_path (os.path.join output_dir "verb_embeddings.pkl"))
  (with [f (open pickle_path "wb")]
    (pickle.dump {"verbs" verbs "embeddings" embeddings} f))
  (print f"Dummy embeddings saved as pickle: {pickle_path}"))

(when (= __name__ "__main__")
  (main))