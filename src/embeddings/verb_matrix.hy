#!/usr/bin/env hy
;;; Minimal French verb embedding similarity matrix
;;; Analyzes semantic relationships between 20 common French verbs

;; Minimal imports
(import os sys math json)
(import dotenv)
(import google.genai)

;; Load environment
(dotenv.load_dotenv)
(setv API-KEY (os.getenv "GOOGLE_API_KEY"))

;; Check for API key
(when (not API-KEY)
  (print "❌ Error: No GOOGLE_API_KEY found in .env file")
  (sys.exit 1))

(defn load-verbs [path]
  "Load verbs from text file"
  (with [f (open path "r" :encoding "utf-8")]
    (lfor line (.readlines f)
          :if (.strip line)
          (.strip line))))

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
  (if (and (> (len a) 0) (> (len b) 0))
      (/ (dot-product a b) 
         (* (magnitude a) (magnitude b)))
      0.0))

(defn generate-embeddings [client model-name verbs]
  "Generate embeddings for all verbs"
  (print f"Generating embeddings for {(len verbs)} verbs using {model-name}")
  
  (setv embeddings {})
  (setv embed-method (getattr client.models "embed_content"))
  
  (for [verb verbs]
    (print f"Processing '{verb}'..." :end " ")
    (try
      (setv response (embed-method 
                       :model model-name 
                       :contents [{"text" verb}]))
      (setv embedding-obj (get response.embeddings 0))
      (setv embedding-values (list embedding-obj.values))
      (setv (get embeddings verb) embedding-values)
      (print "✓")
      (except [e Exception]
        (print f"Error: {e}")
        ;; Use empty placeholder
        (setv (get embeddings verb) []))))
  
  embeddings)

(defn print-similarity-matrix [embeddings]
  "Print similarity matrix for all verb pairs"
  (setv verbs (list (.keys embeddings)))
  
  ;; Print header
  (print "\n=== Verb Similarity Matrix ===")
  (print "       " :end "")
  (for [v verbs]
    (setv v-short (cut v 0 5))
    (print (.ljust v-short 6) :end ""))
  (print)
  
  ;; Print rows
  (for [v1 verbs]
    (setv v1-short (cut v1 0 5))
    (print (.ljust v1-short 6) :end "")
    (for [v2 verbs]
      (setv emb1 (get embeddings v1))
      (setv emb2 (get embeddings v2))
      (setv sim (cosine-similarity emb1 emb2))
      (setv sim-str (.format "{:.3f}" (float sim)))
      (print (.ljust sim-str 6) :end ""))
    (print)))

(defn print-top-similar [embeddings]
  "Print top 3 similar verbs for each verb"
  (setv verbs (list (.keys embeddings)))
  
  (print "\n🔍 Top 3 most similar verbs for each:")
  (print "==================================")
  
  (for [v1 verbs]
    (setv similarities [])
    (for [v2 verbs]
      (when (!= v1 v2)
        (setv emb1 (get embeddings v1))
        (setv emb2 (get embeddings v2))
        (setv sim (cosine-similarity emb1 emb2))
        (.append similarities [v2 sim])))
    
    ;; Sort by similarity (descending)
    (setv sorted-sims (sorted similarities :key (fn [x] (- (get x 1)))))
    (setv top3 (cut sorted-sims 0 3))
    
    (print)
    (print v1 ":")
    (for [pair top3]
      (setv v2 (get pair 0))
      (setv sim (get pair 1))
      (setv sim-str (.format "{:.4f}" (float sim)))
      (print "  •" v2 ":" sim-str))))

(defn main []
  (print "🔤 French Verb Semantic Similarity 🔤")
  (print "===================================")
  
  ;; Initialize Google GenAI client
  (setv client (google.genai.Client :api-key API-KEY))
  
  ;; Get embedding models
  (setv embedding-models [])
  (for [model (.list client.models)]
    (when (in "embedContent" model.supported-actions)
      (.append embedding-models model.name)))
  
  (when (not embedding-models)
    (print "❌ No embedding models available")
    (sys.exit 1))
  
  (print "Available embedding models:")
  (for [model embedding-models]
    (print f"• {model}"))
  
  ;; Use first model
  (setv model-name (get embedding-models 0))
  
  ;; Load verbs
  (setv repo-root (os.path.dirname (os.path.dirname (os.path.dirname __file__))))
  (setv verbs-path (os.path.join repo-root "resources" "verbs" "test_french_verbs.txt"))
  (setv verbs (load-verbs verbs-path))
  
  (print f"Processing {(len verbs)} French verbs:")
  (print (.join ", " verbs))
  
  ;; Set output path
  (setv output-dir (os.path.join repo-root "data" "embeddings"))
  (os.makedirs output-dir :exist_ok True)
  
  ;; Generate embeddings
  (setv embeddings (generate-embeddings client model-name verbs))
  
  ;; Print similarity matrix
  (print-similarity-matrix embeddings)
  
  ;; Print top 3 most similar for each verb
  (print-top-similar embeddings)
  
  ;; Save embeddings
  (setv output-path (os.path.join output-dir "verb_embeddings_simple.json"))
  (with [f (open output-path "w" :encoding "utf-8")]
    (json.dump embeddings f :ensure_ascii False :indent 2))
  
  (print f"\nEmbeddings saved to: {output-path}")
  (print "✅ All done!"))

(when (= __name__ "__main__")
  (main))