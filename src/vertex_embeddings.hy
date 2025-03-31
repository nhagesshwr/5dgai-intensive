;; vertex_embeddings.hy
;; Embeddings & Vector Stores
;; Part of Day 2 Assignment: 5-Day Gen AI Intensive
;; February 2025 29

(require [hy.pyops [*]])
(import vertexai)
(import vertexai.language_models [TextEmbeddingInput TextEmbeddingModel])
(import numpy :as np)
(import tensorflow :as tf)
(import tensorflow_hub :as hub)

;; Set the model name. For multilingual: use "text-multilingual-embedding-002"
(setv MODEL_NAME "text-embedding-004")

;; Set the task_type, text and optional title as the model inputs.
;; Available task_types are "RETRIEVAL_QUERY", "RETRIEVAL_DOCUMENT",
;; "SEMANTIC_SIMILARITY", "CLASSIFICATION", and "CLUSTERING"
(setv TASK_TYPE "RETRIEVAL_DOCUMENT")
(setv TITLE "Google")
(setv TEXT "Embed text.")

;; Use Vertex LLM text embeddings
(setv embeddings-vx (TextEmbeddingModel.from_pretrained "textembedding-gecko@004"))

(defn LLM-embed [text]
  (defn embed-text [text]
    (setv text-inp (TextEmbeddingInput :task_type "CLASSIFICATION" :text (.numpy text)))
    (np.array (get (. (embeddings-vx.get_embeddings [(list [text-inp]))) 0) "values")))
  
  (setv output (tf.py_function :func embed-text :inp [text] :Tout tf.float32))
  (.set_shape output #(768))
  output)

;; Embed strings using vertex LLMs
(setv LLM-embeddings (.map train-data (fn [x y] [(LLM-embed x) y])))

;; Embed strings in the tf.dataset using one of the tf hub models
(setv embedding "https://tfhub.dev/google/sentence-t5/st5-base/1")
(setv hub-layer (hub.KerasLayer embedding :input_shape [] :dtype tf.string :trainable True))

;; Train model
(setv model (tf.keras.Sequential))
(.add model hub-layer)  ;; omit this layer if using Vertex LLM embeddings
(.add model (tf.keras.layers.Dense 16 :activation "relu"))
(.add model (tf.keras.layers.Dense 1))

(.compile model 
  :optimizer "adam"
  :loss (tf.keras.losses.BinaryCrossentropy :from_logits True)
  :metrics #["accuracy"])

(setv history (.fit model (.batch (.shuffle train-data 100) 8)))

;; BigQuery SQL equivalent:
;; SELECT * FROM ML.GENERATE_TEXT_EMBEDDING(
;;   MODEL my_project.my_company.llm_embedding_model,
;;   (
;;   SELECT review as content
;;   FROM bigquery-public-data.imdb.reviews));
