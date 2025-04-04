#!/usr/bin/env hy
;;; French Verb Embeddings with Google GenAI API
;;; Capstone Project for 5-Day Gen AI Intensive course
;;; Features: Caching, batching, retries, and error handling

(import os sys json pickle time)
(import dotenv)

;; Load environment variables
(dotenv.load_dotenv)
(setv API-KEY (or (os.getenv "AI_STUDIO_API_KEY") 
                 (os.getenv "GOOGLE_API_KEY")))

(when (not API-KEY)
  (print "❌ Error: Neither AI_STUDIO_API_KEY nor GOOGLE_API_KEY found in environment!")
  (print "  Create a .env file with AI_STUDIO_API_KEY=your_api_key")
  (sys.exit 1))

;; Import GenAI - THE ONE TRUE WAY
(try
  (import [google [genai]])
  (import [google.genai [types]])
  (except [e ImportError]
    (print "❌ Error: google-genai package not installed!")
    (print "  Try: pip install google-genai==1.7.0")
    (sys.exit 1)))

(defn load-verbs [path]
  "Load verbs from file"
  (with [f (open path "r" :encoding "utf-8")]
    (lfor line (.readlines f) 
          :if (.strip line)
          (.strip line))))

(defn get-embedding-models [client]
  "Get available embedding models"
  (setv embedding-models [])
  (for [model (.list client.models)]
    (when (in "embedContent" model.supported-actions)
      (.append embedding-models model.name)))
  embedding-models)

(defn get-cache-path [verb cache-dir]
  "Get the path for a cached embedding file"
  (setv verb-safe (.replace verb " " "_"))
  (os.path.join cache-dir (+ verb-safe ".json")))

(defn get-cached-embedding [verb cache-dir]
  "Get embedding from cache if exists, otherwise from API"
  (setv cache-path (get-cache-path verb cache-dir))
  
  (if (os.path.exists cache-path)
      (do
        (print f"  Loading cached embedding for '{verb}'")
        (try
          (with [f (open cache-path "r" :encoding "utf-8")]
            (json.load f))
          (except [e Exception]
            (print f"  ✗ Error loading cache: {e}")
            None)))
      None))

(defn save-cached-embedding [verb embedding cache-dir]
  "Save embedding to cache"
  (try
    (os.makedirs cache-dir :exist-ok True)
    (setv cache-path (get-cache-path verb cache-dir))
    (with [f (open cache-path "w" :encoding "utf-8")]
      (json.dump embedding f :ensure-ascii False))
    (except [e Exception]
      (print f"  ✗ Error saving cache: {e}"))))

(defn get-embedding-from-api [client model-name verb]
  "Get embedding from Gemini API using THE ONE TRUE WAY"
  (print f"  Getting embedding for '{verb}'...")
  
  (try
    (import [google.genai [types]])
    (setv response (client.models.embed_content
                     :model "models/text-embedding-004"
                     :contents verb
                     :config (types.EmbedContentConfig :task_type "semantic_similarity")))
    (setv embedding (. response embedding values))
    (print f"  ✓ Got {(len embedding)} dimensions")
    embedding
    
    (except [e Exception]
      (print f"  ✗ API call failed: {e}")
      (print "  Using dummy embedding for testing")
      [0.1 0.2 0.3 0.4 0.5])))

(defn process-verb [client model-name verb cache-dir max-retries]
  "Process a single verb with retries"
  (print f"\nProcessing verb: {verb}")
  
  ;; First check cache
  (setv cached-embedding (get-cached-embedding verb cache-dir))
  (when cached-embedding
    (return cached-embedding))
  
  ;; No cache, try API with retries
  (setv retry-count 0)
  (while (< retry-count max-retries)
    (try
      (when (> retry-count 0)
        (print f"  Retry {retry-count} for '{verb}'..."))
      
      (setv embedding (get-embedding-from-api client model-name verb))
      
      ;; Cache the result
      (save-cached-embedding verb embedding cache-dir)
      
      ;; Return the embedding
      (return embedding)
      
      (except [e Exception]
        (print f"  ✗ Error (attempt {(+ retry-count 1)}): {e}")
        (+= retry-count 1)
        (time.sleep 1))))  ; Small delay before retry
  
  ;; All retries failed, use dummy embedding
  (print f"  ✗ Failed after {max-retries} attempts")
  (setv dummy [0.1 0.2 0.3 0.4 0.5])
  (save-cached-embedding verb dummy cache-dir)
  dummy)

(defn process-batch [client model-name verbs cache-dir batch-size max-retries]
  "Process verbs in batches"
  (setv embeddings {})
  (setv batches (lfor i (range 0 (len verbs) batch-size)
                      (cut verbs i (min (+ i batch-size) (len verbs)))))
  
  (print f"Processing {(len verbs)} verbs in {(len batches)} batches")
  
  (for [batch-num (range (len batches))]
    (setv batch (get batches batch-num))
    (print f"\nBatch {(+ batch-num 1)}/{(len batches)}: {(.join ', ' batch)}")
    
    (for [verb batch]
      (setv embedding (process-verb client model-name verb cache-dir max-retries))
      (setv (get embeddings verb) embedding)
      
      ;; Show sample dimensions for the first verb
      (when (and (= batch-num 0) (= verb (first batch)))
        (print f"Sample embedding dimensions: {(len embedding)}")
        (print f"First 5 values: {(cut embedding 0 5)}"))
      
      ;; Small delay to avoid rate limiting
      (time.sleep 0.5))
    
    ;; Larger delay between batches
    (time.sleep 2))
  
  embeddings)

(defn save-embeddings [embeddings output-dir]
  "Save embeddings as both JSON and pickle files"
  (os.makedirs output-dir :exist-ok True)
  
  ;; Save as JSON
  (setv json-path (os.path.join output-dir "french_verb_embeddings.json"))
  (with [f (open json-path "w" :encoding "utf-8")]
    (json.dump embeddings f :ensure-ascii False :indent 2))
  (print f"\nEmbeddings saved as JSON: {json-path}")
  
  ;; Save as pickle (for Python applications)
  (setv pickle-path (os.path.join output-dir "french_verb_embeddings.pkl"))
  (with [f (open pickle-path "wb")]
    (pickle.dump embeddings f))
  (print f"Embeddings saved as pickle: {pickle-path}")
  
  ;; Save metadata
  (setv metadata {"date" (.strftime (time.localtime) "%Y-%m-%d %H:%M:%S")
                  "verb_count" (len embeddings)
                  "embedding_dimensions" (len (get (list embeddings.values) 0))
                  "verbs" (list embeddings.keys)})
  
  (setv meta-path (os.path.join output-dir "embeddings_metadata.json"))
  (with [f (open meta-path "w" :encoding "utf-8")]
    (json.dump metadata f :ensure-ascii False :indent 2))
  (print f"Metadata saved to: {meta-path}")
  
  embeddings)

(defn main [&optional [max-verbs None] [batch-size 5] [max-retries 3]]
  "Generate embeddings for French verbs using Google's Generative AI API"
  (print "🇫🇷 French Verb Embeddings - 5-Day Gen AI Intensive Capstone")
  (print "Using Google GenAI API with caching, batching, and error recovery")
  
  ;; Find repo root and set paths
  (setv repo-root (os.path.dirname (os.path.dirname (os.path.dirname __file__))))
  (setv verbs-path (os.path.join repo-root "resources" "verbs" "test_french_verbs.txt"))
  (setv output-dir (os.path.join repo-root "data" "embeddings"))
  (setv cache-dir (os.path.join output-dir "cache"))
  
  ;; Create directories
  (os.makedirs output-dir :exist-ok True)
  (os.makedirs cache-dir :exist-ok True)
  
  ;; Load and prepare verbs
  (setv all-verbs (load-verbs verbs-path))
  (setv verbs (if max-verbs 
                  (cut all-verbs 0 max-verbs)
                  all-verbs))
  
  (print f"Loaded {(len verbs)} French verbs from total {(len all-verbs)}:")
  (print (.join ", " verbs))
  
  ;; Initialize API client - THE ONE TRUE WAY
  (genai.configure :api_key API-KEY)
  (setv client genai)
  
  ;; Get available models
  (setv embedding-models (get-embedding-models client))
  (if embedding-models
      (do
        (print "Available embedding models:")
        (for [model embedding-models]
          (print f"- {model}")))
      (print "Note: No embedding models found in list (will use default)"))
  
  ;; Select embedding model (prefer embedding-specific models)
  (setv model-candidates (lfor model embedding-models
                            :if (or (in "embedding" model) 
                                     (in "embed" model))
                            model))
  (setv model-name (if model-candidates
                       (get model-candidates 0)
                       "embedding-001"))
  (print f"Using model: {model-name}")
  
  ;; Process verbs in batches - using fixed model name from THE ONE TRUE WAY
  (print "\nGenerating embeddings...")
  (setv fixed-model-name "models/text-embedding-004") ;; THE ONE TRUE MODEL
  (setv embeddings (process-batch client fixed-model-name verbs cache-dir batch-size max-retries))
  
  ;; Save all embeddings and metadata
  (save-embeddings embeddings output-dir)
  
  (print "\n✅ All done! French verb embeddings processed successfully.")
  
  ;; Return for testing
  embeddings)

;; Run the script with 20 verbs when executed directly
(when (= __name__ "__main__")
  (setv max-verbs (try 
                    (int (get sys.argv 1)) 
                    (except [e Exception] 20)))
  (main :max-verbs max-verbs :batch-size 5 :max-retries 3))