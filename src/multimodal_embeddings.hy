;; multimodal_embeddings.hy
;; Embeddings & Vector Stores
;; Part of Day 2 Assignment: 5-Day Gen AI Intensive
;; February 2025 31

(require [hy.pyops [*]])
(import base64)
(import tensorflow :as tf)
(import [google.cloud [aiplatform]])
(import [google.protobuf [struct_pb2]])
(import tensorflow_hub :as hub)

;; Fine-tunable layer for image embeddings which can be used for downstream keras models
(setv image-embed 
  (hub.KerasLayer 
    "https://tfhub.dev/google/imagenet/efficientnet_v2_imagenet21k_ft1k_s/feature_vector/2" 
    :trainable False))

(defclass EmbeddingResponse []
  (defn __init__ [self &optional [text-embedding None] [image-embedding None]]
    (setv self.text-embedding text-embedding)
    (setv self.image-embedding image-embedding)))

(defclass EmbeddingPredictionClient []
  "Wrapper around Prediction Service Client."
  
  (defn __init__ [self project &optional 
                 [location "us-central1"]
                 [api-regional-endpoint "us-central1-aiplatform.googleapis.com"]]
    (setv client-options {"api_endpoint" api-regional-endpoint})
    (setv self.client (aiplatform.gapic.PredictionServiceClient :client_options client-options))
    (setv self.location location)
    (setv self.project project))
  
  (defn get-embedding [self &optional [text None] [gs-image-path None]]
    ;; Load the image from a bucket in Google Cloud Storage
    (when gs-image-path
      (with [f (tf.io.gfile.GFile gs-image-path "rb")]
        (setv image-bytes (.read f))))
    
    (when (and (not text) (not (if (try (get (locals) "image-bytes") (except [e Exception] None)) True False)))
      (raise (ValueError "At least one of text or image_bytes must be specified.")))
    
    ;; Initialize a protobuf data struct with the text and image inputs
    (setv instance (struct_pb2.Struct))
    
    (when text
      (setv (get instance.fields "text" "string_value") text))
    
    (when (try (get (locals) "image-bytes") (except [e Exception] None))
      (setv encoded-content (-> image-bytes
                               (base64.b64encode)
                               (.decode "utf-8")))
      (setv image-struct (get instance.fields "image" "struct_value"))
      (setv (get image-struct.fields "bytesBase64Encoded" "string_value") encoded-content))
    
    ;; Make predictions using the multimodal embedding model
    (setv instances [instance])
    (setv endpoint (+ f"projects/{self.project}/locations/{self.location}"
                      "/publishers/google/models/multimodalembedding@001"))
    
    (setv response (.predict self.client :endpoint endpoint :instances instances))
    
    (setv text-embedding None)
    (when text
      (setv text-emb-value (get response.predictions 0 "textEmbedding"))
      (setv text-embedding (lfor v text-emb-value v)))
    
    (setv image-embedding None)
    (when (try (get (locals) "image-bytes") (except [e Exception] None))
      (setv image-emb-value (get response.predictions 0 "imageEmbedding"))
      (setv image-embedding (lfor v image-emb-value v)))
    
    (EmbeddingResponse :text-embedding text-embedding :image-embedding image-embedding)))

;; Example usage:
;; (setv client (EmbeddingPredictionClient "your-project-id"))
;; (.get-embedding client :text "sample_text" :gs-image-path "gs://bucket_name../image_filename..")
