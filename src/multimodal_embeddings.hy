#!/usr/bin/env hy
"\"
Multimodal Embeddings with Vertex AI

Provides functionality to generate embeddings for both text and images
using Google Cloud's Vertex AI multimodal embedding models.

Part of Day 2 Assignment: 5-Day Gen AI Intensive
\""

(require [hy.pyops [*]])
(import argparse)
(import base64)
(import json)
(import os)
(import sys)
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
  "\"Wrapper around Prediction Service Client.\""
  
  (defn __init__ [self project &optional 
                 [location "us-central1"]
                 [api-regional-endpoint "us-central1-aiplatform.googleapis.com"]]
    (setv client-options {"api_endpoint" api-regional-endpoint})
    (setv self.client (aiplatform.gapic.PredictionServiceClient :client_options client-options))
    (setv self.location location)
    (setv self.project project))
  
  (defn get-embedding [self &optional [text None] [gs-image-path None]]
    "\"
    Generate embeddings for text and/or images using Vertex AI.
    
    Args:
        text: Optional text to embed
        gs-image-path: Optional GCS path to an image file to embed
        
    Returns:
        EmbeddingResponse object with text and/or image embeddings
    \""
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
    
    (EmbeddingResponse :text-embedding text-embedding :image-embedding image-embedding))))

;; Core functionality separated from CLI
(defn generate-embeddings [project text gs-image-path &optional 
                          [location "us-central1"]
                          [api-endpoint "us-central1-aiplatform.googleapis.com"]
                          [output-path None]]
  "\"
  Generate embeddings for text and/or images and optionally save to file.
  
  Args:
      project: GCP project ID
      text: Text to embed
      gs-image-path: GCS path to an image file to embed
      location: GCP region
      api-endpoint: Vertex AI API endpoint
      output-path: Optional path to save results as JSON
      
  Returns:
      Dictionary with text and/or image embeddings
  \""
  (try
    (let [client (EmbeddingPredictionClient project :location location :api-regional-endpoint api-endpoint)
          response (.get-embedding client :text text :gs-image-path gs-image-path)
          result {"text_embedding" response.text-embedding
                  "image_embedding" response.image-embedding}]
      
      ;; Save to file if output path provided
      (when output-path
        (with [f (open output-path "w")]
          (json.dump result f :indent 2)))
      
      result)
    (except [e Exception]
      (print f"Error generating embeddings: {e}" :file sys.stderr)
      None)))

;; CLI handling
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser 
                 :description "Generate embeddings for text and images using Vertex AI")]
    
    (.add_argument parser "--project" :required True
                   :help "GCP project ID")
    
    (.add_argument parser "--text" :default None
                   :help "Text to embed")
    
    (.add_argument parser "--image" :dest "gs_image_path" :default None
                   :help "GCS path to image file (gs://...)")
    
    (.add_argument parser "--location" :default "us-central1"
                   :help "GCP region (default: us-central1)")
    
    (.add_argument parser "--api-endpoint" :default "us-central1-aiplatform.googleapis.com"
                   :help "Vertex AI API endpoint")
    
    (.add_argument parser "--output" :dest "output_path" :default None
                   :help "Path to save embeddings as JSON")
    
    (.parse_args parser)))

(defn main []
  "\"Main entry point for command line use.\""
  (let [args (parse-args)]
    
    ;; Validate that at least one of text or image is provided
    (when (and (not args.text) (not args.gs_image_path))
      (print "Error: At least one of --text or --image must be provided" :file sys.stderr)
      (sys.exit 1))
    
    ;; Generate embeddings
    (let [result (generate-embeddings 
                   args.project
                   args.text
                   args.gs_image_path
                   :location args.location
                   :api-endpoint args.api_endpoint
                   :output-path args.output_path)]
      
      ;; Print results to stdout if no output file specified
      (when (and result (not args.output_path))
        (print (json.dumps result :indent 2))))))

;; Run main when script is executed directly
(when (= __name__ "__main__")
  (main))
