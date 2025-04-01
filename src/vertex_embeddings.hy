#!/usr/bin/env hy
"\"
Vertex AI Text Embeddings

Provides utilities for generating text embeddings using Vertex AI models
and building simple classification models with those embeddings.

Part of Day 2 Assignment: 5-Day Gen AI Intensive
\""

(require [hy.pyops [*]])
(import argparse)
(import json)
(import os)
(import sys)
(import vertexai)
(import [vertexai.language_models [TextEmbeddingInput TextEmbeddingModel]])
(import numpy :as np)
(import tensorflow :as tf)
(import tensorflow_hub :as hub)

;; Core functionality for generating embeddings
(defn get-embedding-model [model-name]
  "\"
  Initialize and return a Vertex AI text embedding model.
  
  Args:
      model-name: Name of the embedding model to use
      
  Returns:
      Initialized TextEmbeddingModel
  \""
  (TextEmbeddingModel.from_pretrained model-name))

(defn generate-embeddings [texts task-type model-name project-id location]
  "\"
  Generate embeddings for a list of texts using Vertex AI.
  
  Args:
      texts: List of text strings to embed
      task-type: Embedding task type (RETRIEVAL_DOCUMENT, RETRIEVAL_QUERY, etc.)
      model-name: Name of the embedding model to use
      project-id: GCP project ID
      location: GCP region
      
  Returns:
      List of embedding vectors
  \""
  ;; Initialize Vertex AI
  (when (and project-id location)
    (vertexai.init :project project-id :location location))
  
  ;; Get the model
  (setv model (get-embedding-model model-name))
  
  ;; Prepare inputs
  (setv inputs (lfor text texts
                    (TextEmbeddingInput :task_type task-type :text text)))
  
  ;; Generate embeddings
  (try
    (let [embeddings (.get_embeddings model inputs)]
      (lfor emb embeddings
            (get emb "values")))
    (except [e Exception]
      (print f"Error generating embeddings: {e}" :file sys.stderr)
      None)))

(defn create-tf-embedding-layer [model-url]
  "\"
  Create a TensorFlow Hub embedding layer.
  
  Args:
      model-url: TF Hub URL for the embedding model
      
  Returns:
      TensorFlow Hub Keras layer
  \""
  (hub.KerasLayer model-url :input_shape [] :dtype tf.string :trainable True))

(defn create-classification-model [embedding-layer hidden-units]
  "\"
  Create a simple classification model using an embedding layer.
  
  Args:
      embedding-layer: TensorFlow Hub embedding layer
      hidden-units: Number of units in hidden layer
      
  Returns:
      Compiled TensorFlow model
  \""
  (let [model (tf.keras.Sequential)]
    (.add model embedding-layer)
    (.add model (tf.keras.layers.Dense hidden-units :activation "relu"))
    (.add model (tf.keras.layers.Dense 1))
    
    (.compile model 
      :optimizer "adam"
      :loss (tf.keras.losses.BinaryCrossentropy :from_logits True)
      :metrics #["accuracy"])
    
    model))

(defn embed-and-save [texts task-type model-name output-path &optional 
                     [project-id None] [location None]]
  "\"
  Generate embeddings for texts and save to JSON file.
  
  Args:
      texts: List of text strings to embed
      task-type: Embedding task type
      model-name: Name of the embedding model to use
      output-path: Path to save embeddings JSON
      project-id: Optional GCP project ID
      location: Optional GCP region
      
  Returns:
      List of embedding vectors
  \""
  (let [embeddings (generate-embeddings texts task-type model-name project-id location)]
    (when (and embeddings output-path)
      (with [f (open output-path "w")]
        (json.dump {"embeddings" embeddings} f :indent 2)))
    embeddings))

;; Command-line interface
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser 
                 :description "Generate text embeddings with Vertex AI")]
    
    (.add_argument parser "--text" :nargs "+" :required True
                  :help "Text(s) to embed")
    
    (.add_argument parser "--task-type" :default "RETRIEVAL_DOCUMENT"
                  :choices ["RETRIEVAL_DOCUMENT" "RETRIEVAL_QUERY" 
                           "SEMANTIC_SIMILARITY" "CLASSIFICATION" "CLUSTERING"]
                  :help "Embedding task type")
    
    (.add_argument parser "--model" :dest "model_name" :default "textembedding-gecko@004"
                  :help "Model name (default: textembedding-gecko@004)")
    
    (.add_argument parser "--project" :dest "project_id" :default None
                  :help "GCP project ID")
    
    (.add_argument parser "--location" :default None
                  :help "GCP region")
    
    (.add_argument parser "--output" :dest "output_path" :default None
                  :help "Path to save embeddings as JSON")
    
    (.parse_args parser)))

(defn main []
  "\"Main entry point for command line use.\""
  (let [args (parse-args)]
    
    ;; Generate and save embeddings
    (let [embeddings (embed-and-save 
                      args.text
                      args.task_type
                      args.model_name
                      args.output_path
                      :project-id args.project_id
                      :location args.location)]
      
      ;; Print results to stdout if no output file specified or if generation failed
      (when (and embeddings (not args.output_path))
        (print (json.dumps {"embeddings" embeddings} :indent 2))))))

;; Run main when script is executed directly
(when (= __name__ "__main__")
  (main))
