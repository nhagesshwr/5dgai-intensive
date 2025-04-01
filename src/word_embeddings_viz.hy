;; word_embeddings_viz.hy - Visualization of word embeddings using Hy

(import [gensim.models [Word2Vec]]
        [gensim.downloader :as api]
        [pprint]
        [matplotlib.pyplot :as plt]
        [sklearn.manifold [TSNE]]
        [numpy :as np])

(defn tsne-plot [models words &optional [seed 23]]
  "Creates TSNE models & plots for multiple word models for the given words"
  (plt.figure :figsize [(* (len models) 30) (* (len models) 30)])
  (setv model-ix 0)
  
  (for [model models]
    (setv labels [])
    (setv tokens [])
    
    (for [word words]
      (.append tokens (get model word))
      (.append labels word))
    
    (setv tsne-model (TSNE :perplexity 40 
                           :n_components 2 
                           :init "pca" 
                           :n_iter 2500 
                           :random_state seed))
    
    (setv new-values (.fit_transform tsne-model (np.array tokens)))
    
    (setv x [])
    (setv y [])
    
    (for [value new-values]
      (.append x (get value 0))
      (.append y (get value 1)))
    
    (+= model-ix 1)
    (plt.subplot 10 10 model-ix)
    
    (for [i (range (len x))]
      (plt.scatter (get x i) (get y i))
      (plt.annotate (get labels i)
                   :xy [(get x i) (get y i)]
                   :xytext [5 2]
                   :textcoords "offset points"
                   :ha "right"
                   :va "bottom"))
  
  (plt.tight_layout)
  (plt.show))

(defn visualize-embeddings [models-list words-list &optional [output-path None]]
  "Load embedding models and visualize the specified words"
  (setv loaded-models [])
  
  ;; Load each model
  (for [model-name models-list]
    (print f"Loading {model-name}...")
    (.append loaded-models (api.load model-name)))
  
  ;; Create visualization
  (print f"Creating TSNE visualization for {(len words-list)} words...")
  (tsne-plot loaded-models words-list)
  
  ;; Save visualization if output path provided
  (when output-path
    (plt.savefig output-path)
    (print f"Visualization saved to {output-path}"))
  
  ;; Return models for further use
  loaded-models)

(defn main []
  "Main entry point for command line use"
  ;; Use default models if run directly
  (setv models ["word2vec-google-news-300" "glove-twitter-25"])
  
  (print "Loading word embedding models...")
  (setv loaded-models (visualize-embeddings models []))
  
  (setv v2w-model (get loaded-models 0))
  (setv glove-model (get loaded-models 1))
  
  (print "\nWords most similar to 'computer' with word2vec and glove respectively:")
  (pprint.pprint (cut (v2w-model.most_similar "computer") 0 3))
  (pprint.pprint (cut (glove-model.most_similar "computer") 0 3))
  
  (print "\nGenerating 2D projection of common words")
  
  ;; Find common words between both models
  (setv sample-common-words 
        (cut (list 
               (& (set (cut v2w-model.index_to_key 100 10000))
                  (set (cut glove-model.index_to_key 100 10000))))
             0 100))
  
  ;; Create visualization of common words
  (tsne-plot loaded-models sample-common-words))

(when (= __name__ "__main__")
  (main))
