#!/usr/bin/env hy
"\"
Nearest Neighbors Search with Text Embeddings

This script demonstrates various nearest neighbor search methods using text embeddings:
1. Brute force search
2. Ball tree algorithm (efficient for high-dimensional spaces)
3. Locality-Sensitive Hashing (LSH)

Part of Day 2 Assignment: 5-Day Gen AI Intensive
\""

(require [hy.pyops [*]])
(import argparse)
(import json)
(import sys)
(import [sklearn.neighbors [NearestNeighbors]])
(import [vertexai.language_models [TextEmbeddingModel]])
(import [lshashing [LSHRandom]])
(import numpy :as np)

;; Core functionality separated from CLI
(defn get-embeddings [texts model-name]
  "\"
  Generate text embeddings using Vertex AI.
  
  Args:
      texts: List of text strings to embed
      model-name: Name of the embedding model to use
      
  Returns:
      NumPy array of embeddings
  \""
  (let [model (TextEmbeddingModel.from_pretrained model-name)]
    (try
      (np.array (lfor embedding (model.get_embeddings texts) 
                     embedding.values))
      (except [e Exception]
        (print f"Error generating embeddings: {e}" :file sys.stderr)
        None))))

(defn search-brute-force [corpus-embeddings query-embedding k]
  "\"
  Perform brute force nearest neighbor search.
  
  Args:
      corpus-embeddings: Embeddings of the corpus texts
      query-embedding: Embedding of the query text
      k: Number of neighbors to return
      
  Returns:
      Tuple of (distances, indices)
  \""
  (let [nbrs (-> (NearestNeighbors :n_neighbors k :algorithm "brute")
                (.fit corpus-embeddings))]
    (.kneighbors nbrs (np.expand_dims query-embedding :axis 0))))

(defn search-ball-tree [corpus-embeddings query-embedding k]
  "\"
  Perform ball tree nearest neighbor search (efficient for high-dimensional vectors).
  
  Args:
      corpus-embeddings: Embeddings of the corpus texts
      query-embedding: Embedding of the query text
      k: Number of neighbors to return
      
  Returns:
      Tuple of (distances, indices)
  \""
  (let [nbrs (-> (NearestNeighbors :n_neighbors k :algorithm "ball_tree")
                (.fit corpus-embeddings))]
    (.kneighbors nbrs (np.expand_dims query-embedding :axis 0))))

(defn search-lsh [corpus-embeddings query-embedding k hash-tables probes]
  "\"
  Perform Locality-Sensitive Hashing (LSH) search.
  
  Args:
      corpus-embeddings: Embeddings of the corpus texts
      query-embedding: Embedding of the query text
      k: Number of neighbors to return
      hash-tables: Number of hash tables to use
      probes: Number of probes
      
  Returns:
      LSH result
  \""
  (let [lsh-random (LSHRandom corpus-embeddings hash-tables :parallel True)]
    (.knn_search lsh-random 
                corpus-embeddings 
                query-embedding 
                k 
                probes 
                :parallel True)))

(defn run-search [corpus texts query model-name k hash-tables probes output-path]
  "\"
  Run all three search methods and return results.
  
  Args:
      corpus: List of corpus texts
      texts: Combined list of all texts (corpus + query)
      query: Query text
      model-name: Name of the embedding model
      k: Number of neighbors
      hash-tables: Number of hash tables for LSH
      probes: Number of probes for LSH
      output-path: Optional path to save results as JSON
      
  Returns:
      Dictionary with search results
  \""
  ;; Generate embeddings for all texts
  (setv embedded-texts (get-embeddings texts model-name))
  (when (is embedded-texts None)
    (return None))
  
  ;; Split corpus and query embeddings
  (setv corpus-size (len corpus))
  (setv corpus-embeddings (get embedded-texts (slice 0 corpus-size)))
  (setv query-embedding (get embedded-texts corpus-size))
  
  ;; Run all three search methods
  (setv [brute-distances brute-indices] 
    (search-brute-force corpus-embeddings query-embedding k))
  
  (setv [ball-tree-distances ball-tree-indices] 
    (search-ball-tree corpus-embeddings query-embedding k))
  
  (setv lsh-result 
    (search-lsh corpus-embeddings query-embedding k hash-tables probes))
  
  ;; Prepare results
  (setv results {
    "query" query
    "corpus" corpus
    "brute_force" {
      "distances" (. brute-distances [0] tolist)
      "indices" (. brute-indices [0] tolist)
      "matches" (lfor idx (. brute-indices [0]) (get corpus idx))
    }
    "ball_tree" {
      "distances" (. ball-tree-distances [0] tolist)
      "indices" (. ball-tree-indices [0] tolist)
      "matches" (lfor idx (. ball-tree-indices [0]) (get corpus idx))
    }
    "lsh" {
      "distances" (. lsh-result [1] tolist)
      "indices" (. lsh-result [0] tolist)
      "matches" (lfor idx (. lsh-result [0]) (get corpus idx))
    }
  })
  
  ;; Save to file if output path provided
  (when output-path
    (with [f (open output-path "w")]
      (json.dump results f :indent 2)))
  
  results)

;; Command-line interface
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser 
                 :description "Nearest neighbor search with text embeddings")]
    
    (.add_argument parser "--corpus" :nargs "+" :required True
                  :help "Corpus texts to search within")
    
    (.add_argument parser "--query" :required True
                  :help "Query text to search for")
    
    (.add_argument parser "--model" :dest "model_name" :default "textembedding-gecko@004"
                  :help "Model name (default: textembedding-gecko@004)")
    
    (.add_argument parser "--k" :type int :default 2
                  :help "Number of nearest neighbors to return (default: 2)")
    
    (.add_argument parser "--hash-tables" :type int :default 4
                  :help "Number of hash tables for LSH (default: 4)")
    
    (.add_argument parser "--probes" :type int :default 3
                  :help "Number of probes for LSH (default: 3)")
    
    (.add_argument parser "--output" :dest "output_path" :default None
                  :help "Path to save results as JSON")
    
    (.parse_args parser)))

(defn main []
  "\"Main entry point for command line use.\""
  (let [args (parse-args)]
    
    ;; Combine corpus and query for a single embedding operation
    (setv texts (+ args.corpus [args.query]))
    
    ;; Run search
    (let [results (run-search 
                   args.corpus
                   texts
                   args.query
                   args.model_name
                   args.k
                   args.hash_tables
                   args.probes
                   args.output_path)]
      
      ;; Print results to stdout if no output file specified
      (when (and results (not args.output_path))
        (print (json.dumps results :indent 2))))))

;; Run main when script is executed directly
(when (= __name__ "__main__")
  (main))
