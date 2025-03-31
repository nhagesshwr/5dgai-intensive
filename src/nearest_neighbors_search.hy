;; nearest_neighbors_search.hy
;; Embeddings & Vector Stores
;; Part of Day 2 Assignment: 5-Day Gen AI Intensive
;; February 2025

(require [hy.pyops [*]])
(import [sklearn.neighbors [NearestNeighbors]])
(import [vertexai.language_models [TextEmbeddingModel]])
(import [lshashing [LSHRandom]])
(import numpy :as np)

;; Initialize the text embedding model
(setv model (TextEmbeddingModel.from_pretrained "textembedding-gecko@004"))

;; Test items and query
(setv test-items [
  "The earth is spherical."
  "The earth is a planet."
  "I like to eat at a restaurant."])

(setv query "the shape of earth")

;; Get embeddings for test items and query
(setv embedded-test-items 
  (np.array (lfor embedding (model.get_embeddings test-items) 
                 embedding.values)))

(setv embedded-query 
  (np.array (get (model.get_embeddings [query]) 0 "values")))

;; Naive brute force search
(setv n-neighbors 2)
(setv nbrs (-> (NearestNeighbors :n_neighbors n-neighbors :algorithm "brute")
               (.fit embedded-test-items)))

(setv [naive-distances naive-indices] 
  (.kneighbors nbrs (np.expand_dims embedded-query :axis 0)))

;; Using ball_tree algorithm due to high dimensional vectors (or kd_tree otherwise)
(setv nbrs (-> (NearestNeighbors :n_neighbors n-neighbors :algorithm "ball_tree")
               (.fit embedded-test-items)))

(setv [distances indices] 
  (.kneighbors nbrs (np.expand_dims embedded-query :axis 0)))

;; LSH (Locality-Sensitive Hashing)
(setv lsh-random-parallel 
  (LSHRandom embedded-test-items 4 :parallel True))

(setv lsh-result 
  (.knn_search lsh-random-parallel 
               embedded-test-items 
               embedded-query 
               n-neighbors 
               3 
               :parallel True))

;; Output for all 3 methods:
;; indices = [0, 1]
;; distances [0.66840428, 0.71048843] for the first 2 neighbors
;; ANN retrieved the same ranking of items as brute force in a much more scalable manner
