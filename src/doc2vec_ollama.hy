;; doc2vec_ollama.hy
;; A Hy implementation of document embeddings using Ollama's nomic-embed-text model

(import [requests]
        [numpy :as np]
        [json]
        [os.path [join isdir]]
        [os [makedirs]]
        [pickle])

(defclass OllamaDocEmbeddings []
  "Document embeddings implementation using Ollama's embedding model.
   Similar concept to Doc2Vec but using the nomic-embed-text model."
  
  (defn __init__ [self &optional [model-name "nomic-embed-text"] [vector-size 768] [cache-dir ".ollama_cache"]]
    "Initialize with the specified model and settings"
    (setv self.model-name model-name)
    (setv self.vector-size vector-size)
    (setv self.ollama-url "http://localhost:11434/api/embeddings")
    (setv self.document-vectors {})
    (setv self.cache-dir cache-dir)
    
    ;; Create cache directory if it doesn't exist
    (when (not (isdir cache-dir))
      (makedirs cache-dir)))
  
  (defn embed-document [self text]
    "Generate an embedding for a single document using Ollama API"
    (try
      (let [response (.post requests self.ollama-url 
                      :json {"model" self.model-name
                            "prompt" text})]
        (if (= response.status-code 200)
          (get (.json response) "embedding")
          (do
            (print f"Error: {response.status-code} - {response.text}")
            None)))
      (except [e Exception]
        (print f"Exception during embedding: {e}")
        None)))
  
  (defn fit [self documents]
    "Create embeddings for all documents and store them"
    (print f"Creating embeddings for {(len documents)} documents...")
    (for [[idx doc] (enumerate documents)]
      (let [doc-text (.join " " doc)]  ; Convert list of tokens to a space-separated string
        (setv (get self.document-vectors idx) (self.embed-document doc-text))
        (when (= (% idx 10) 0)
          (print f"Processed {idx} documents"))))
    (print "Finished creating embeddings")
    self)
  
  (defn save [self filename]
    "Save the model to a file"
    (let [full-path (join self.cache-dir filename)]
      (with [f (open full-path "wb")]
        (pickle.dump self.document-vectors f))
      (print f"Model saved to {full-path}")))
  
  (defn load [self filename]
    "Load a saved model"
    (let [full-path (join self.cache-dir filename)]
      (with [f (open full-path "rb")]
        (setv self.document-vectors (pickle.load f)))
      (print f"Model loaded from {full-path}"))
    self)
  
  (defn infer-vector [self text-tokens]
    "Generate an embedding for new text"
    (let [text (.join " " text-tokens)]
      (self.embed-document text)))
  
  (defn similarity [self doc-id1 doc-id2]
    "Calculate cosine similarity between two documents"
    (let [vec1 (get self.document-vectors doc-id1)
          vec2 (get self.document-vectors doc-id2)]
      (if (and vec1 vec2)
        (/ (np.dot vec1 vec2)
           (* (np.linalg.norm vec1) (np.linalg.norm vec2)))
        0.0)))
  
  (defn most-similar [self query-vector &optional [topn 5]]
    "Find most similar documents to the query vector"
    (let [similarities (lfor [idx vec] (.items self.document-vectors)
                        [idx (/ (np.dot vec query-vector)
                              (* (np.linalg.norm vec) (np.linalg.norm query-vector)))])]
      (cut (sorted similarities :key (fn [x] (get x 1)) :reverse True) 0 topn))))

(defn create-embeddings [documents &optional [model-name "nomic-embed-text"] [output-path None]]
  "Create document embeddings for the given documents and optionally save model"
  ;; Create embeddings model
  (setv model (OllamaDocEmbeddings :model-name model-name))
  
  ;; Fit model on documents
  (print f"Training document embeddings on {(len documents)} documents...")
  (model.fit documents)
  
  ;; Save model if output path provided
  (when output-path
    (model.save output-path)
    (print f"Model saved to {output-path}"))
  
  ;; Return model for further use
  model)

(defn main []
  "Main entry point for command line use"
  (import [gensim.test.utils [common-texts]])
  
  ;; Create and train model
  (print "Creating document embeddings model...")
  (setv model (create-embeddings common-texts :output-path "ollama_doc2vec_v1"))
  
  ;; Test similarity
  (print "\nDocument similarities:")
  (for [i (range (min 3 (len common-texts)))]
    (for [j (range (min 3 (len common-texts)))]
      (when (< i j)
        (print f"Similarity between doc {i} and doc {j}: {(model.similarity i j):.4f}"))))
  
  ;; Test inference on new text
  (setv query-vec (model.infer-vector ["human" "interface"]))
  (print "\nMost similar documents to 'human interface':")
  (for [[doc-id similarity] (model.most-similar query-vec)]
    (print f"Document {doc-id}: {similarity:.4f}")))

(when (= __name__ "__main__")
  (main))
