#!/usr/bin/env hy
"\"
Vertex AI Vector Search Setup

Sets up a Vertex AI Vector Search index and endpoint for text retrieval,
and demonstrates basic RAG (Retrieval Augmented Generation) functionality.

Part of Day 2 Assignment: 5-Day Gen AI Intensive
\""

(require [hy.pyops [*]])
(import argparse)
(import json)
(import os)
(import sys)
(import [google.cloud [aiplatform]])
(import [langchain_google_vertexai [VertexAIEmbeddings VertexAI]])
(import [langchain_google_vertexai [VectorSearchVectorStore VectorSearchVectorStoreDatastore]])
(import [langchain.chains [RetrievalQA]])
(import [langchain.prompts.chat [ChatPromptTemplate SystemMessagePromptTemplate HumanMessagePromptTemplate]])

;; Core functionality

(defn create-vector-search-index [project-id region bucket display-name dimensions deployed-index-id]
  "\"
  Creates a Vertex AI Vector Search index and endpoint.
  
  Args:
      project-id: GCP project ID
      region: GCP region
      bucket: GCS bucket name
      display-name: Name for the index
      dimensions: Number of dimensions for embeddings
      deployed-index-id: ID for the deployed index
      
  Returns:
      Dictionary with index and endpoint information
  \""
  (try
    ;; Initialize Vertex AI
    (setv bucket-uri f"gs://{bucket}")
    (aiplatform.init :project project-id :location region :staging_bucket bucket-uri)
    
    ;; Create index
    (print "Creating Vector Search index...")
    (setv index 
      (aiplatform.MatchingEngineIndex.create_tree_ah_index
        :display_name display-name
        :dimensions dimensions
        :approximate_neighbors_count 150
        :distance_measure_type "DOT_PRODUCT_DISTANCE"
        :index_update_method "STREAM_UPDATE"))
    
    ;; Create endpoint
    (print "Creating index endpoint...")
    (setv endpoint 
      (aiplatform.MatchingEngineIndexEndpoint.create
        :display_name f"{display-name}-endpoint" 
        :public_endpoint_enabled True))
    
    ;; Deploy index to endpoint
    (print "Deploying index to endpoint (this may take up to 20 minutes)...")
    (setv endpoint 
      (.deploy_index endpoint
        :index index 
        :deployed_index_id deployed-index-id))
    
    ;; Return the created resources
    {"index" {"name" index.name
              "resource_name" index.resource_name
              "display_name" index.display_name}
     "endpoint" {"name" endpoint.name
                "resource_name" endpoint.resource_name
                "display_name" endpoint.display_name
                "deployed_indexes" endpoint.deployed_indexes}}
    
    (except [e Exception]
      (print f"Error creating Vector Search resources: {e}" :file sys.stderr)
      None)))

(defn load-existing-resources [index-id endpoint-id]
  "\"
  Loads existing Vector Search resources by ID.
  
  Args:
      index-id: ID of the existing Vector Search index
      endpoint-id: ID of the existing Vector Search endpoint
      
  Returns:
      Dictionary with index and endpoint objects
  \""
  (try
    (setv index (aiplatform.MatchingEngineIndex index-id))
    (setv endpoint (aiplatform.MatchingEngineIndexEndpoint endpoint-id))
    {"index" index "endpoint" endpoint}
    (except [e Exception]
      (print f"Error loading Vector Search resources: {e}" :file sys.stderr)
      None)))

(defn create-vector-store [project-id region bucket index endpoint embedding-model]
  "\"
  Creates a Vector Store from the given index and endpoint.
  
  Args:
      project-id: GCP project ID
      region: GCP region
      bucket: GCS bucket name
      index: MatchingEngineIndex object
      endpoint: MatchingEngineIndexEndpoint object
      embedding-model: Model name for embeddings
      
  Returns:
      VectorSearchVectorStore object
  \""
  (try
    ;; Create embedding model
    (setv embeddings (VertexAIEmbeddings :model_name embedding-model))
    
    ;; Create vector store
    (VectorSearchVectorStore.from_components
      :project_id project-id
      :region region
      :gcs_bucket_name bucket
      :index_id index.name
      :endpoint_id endpoint.name
      :embedding embeddings
      :stream_update True)
    (except [e Exception]
      (print f"Error creating Vector Store: {e}" :file sys.stderr)
      None)))

(defn add_texts_to_store [vector-store texts]
  "\"
  Adds text chunks to a Vector Store.
  
  Args:
      vector-store: VectorSearchVectorStore object
      texts: List of text chunks to add
      
  Returns:
      True if successful, False otherwise
  \""
  (try
    (.add_texts vector-store :texts texts)
    True
    (except [e Exception]
      (print f"Error adding texts to Vector Store: {e}" :file sys.stderr)
      False)))

(defn create-rag-chain [vector-store llm-model system-prompt]
  "\"
  Creates a RAG (Retrieval Augmented Generation) chain.
  
  Args:
      vector-store: VectorSearchVectorStore object
      llm-model: Name of the LLM model to use
      system-prompt: System prompt template
      
  Returns:
      RetrievalQA chain object
  \""
  (try
    ;; Create retriever
    (setv retriever (.as_retriever vector-store))
    
    ;; Create prompt template
    (setv messages [
      (SystemMessagePromptTemplate.from_template system-prompt)
      (HumanMessagePromptTemplate.from_template "{question}")
    ])
    (setv prompt (ChatPromptTemplate.from_messages messages))
    
    ;; Initialize LLM
    (setv llm (VertexAI :model_name llm-model))
    
    ;; Build and return RAG chain
    (RetrievalQA.from_chain_type 
      :llm llm 
      :chain_type "stuff"
      :retriever retriever 
      :return_source_documents True)
    
    (except [e Exception]
      (print f"Error creating RAG chain: {e}" :file sys.stderr)
      None)))

(defn process-query [rag-chain query]
  "\"
  Process a query through the RAG chain.
  
  Args:
      rag-chain: RetrievalQA chain object
      query: Question to ask
      
  Returns:
      Dictionary with query results
  \""
  (try
    (let [result (rag-chain {"query" query})]
      {"query" query
       "answer" (get result "result")
       "sources" (lfor doc (get result "source_documents") doc.page_content)})
    (except [e Exception]
      (print f"Error processing query: {e}" :file sys.stderr)
      None)))

(defn setup-complete-pipeline [args]
  "\"
  Sets up the complete Vector Search and RAG pipeline.
  
  Args:
      args: Command line arguments
      
  Returns:
      Dictionary with setup results
  \""
  (let [results {}]
    
    ;; Step 1: Create or load Vector Search resources
    (if (and args.index_id args.endpoint_id)
      ;; Load existing resources
      (do
        (print "Loading existing Vector Search resources...")
        (setv resources (load-existing-resources args.index_id args.endpoint_id))
        (when resources
          (setv (get results "resources") {"loaded" True
                                          "index_id" args.index_id
                                          "endpoint_id" args.endpoint_id})))
      
      ;; Create new resources
      (do
        (print "Creating new Vector Search resources...")
        (setv resources-info (create-vector-search-index 
                              args.project_id
                              args.region
                              args.bucket
                              args.display_name
                              args.dimensions
                              args.deployed_index_id))
        (when resources-info
          (print "Vector Search resources created successfully")
          (setv (get results "resources") resources-info)
          ;; Save config for future use
          (with [f (open "vector_search_config.json" "w")]
            (json.dump resources-info f :indent 2))
          
          ;; Load the resources as objects
          (setv resources (load-existing-resources 
                          (get-in resources-info ["index" "resource_name"])
                          (get-in resources-info ["endpoint" "resource_name"]))))))
    
    ;; Step 2: Create vector store if resources are available
    (when resources
      (print "Creating Vector Store...")
      (setv vector-store (create-vector-store
                         args.project_id
                         args.region
                         args.bucket
                         (get resources "index")
                         (get resources "endpoint")
                         args.embedding_model))
      
      (when vector-store
        (setv (get results "vector_store") "created")
        
        ;; Step 3: Add texts if provided
        (when args.texts
          (print "Adding texts to Vector Store...")
          (when (add_texts_to_store vector-store args.texts)
            (setv (get results "texts_added") True))
          
          ;; Step 4: Create RAG chain if requested
          (when args.create_rag
            (print "Creating RAG chain...")
            (setv rag-chain (create-rag-chain 
                            vector-store 
                            args.llm_model
                            args.system_prompt))
            
            (when rag-chain
              (setv (get results "rag_chain") "created")
              
              ;; Step 5: Process query if provided
              (when args.query
                (print "Processing query...")
                (let [query-result (process-query rag-chain args.query)]
                  (when query-result
                    (setv (get results "query_result") query-result)))))))))
    
    ;; Return the results
    results))

;; Command-line interface
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser 
                 :description "Set up Vertex AI Vector Search and RAG")]
    
    ;; Project and region settings
    (.add_argument parser "--project-id" :required True
                  :help "GCP project ID")
    
    (.add_argument parser "--region" :default "us-central1"
                  :help "GCP region (default: us-central1)")
    
    (.add_argument parser "--bucket" :required True
                  :help "GCS bucket name (without gs:// prefix)")
    
    ;; Existing or new resources
    (.add_argument parser "--index-id" :default None
                  :help "Existing Vector Search index ID (if omitted, creates new)")
    
    (.add_argument parser "--endpoint-id" :default None
                  :help "Existing Vector Search endpoint ID (if omitted, creates new)")
    
    ;; New resource settings
    (.add_argument parser "--display-name" :default "vector-search-index"
                  :help "Display name for new index (default: vector-search-index)")
    
    (.add_argument parser "--dimensions" :type int :default 768
                  :help "Number of dimensions for embeddings (default: 768)")
    
    (.add_argument parser "--deployed-index-id" :default "deployed-index-01"
                  :help "ID for deployed index (default: deployed-index-01)")
    
    ;; Model settings
    (.add_argument parser "--embedding-model" :default "text-embedding-005"
                  :help "Embedding model name (default: text-embedding-005)")
    
    (.add_argument parser "--llm-model" :default "gemini-pro"
                  :help "LLM model name for RAG (default: gemini-pro)")
    
    ;; Content and query
    (.add_argument parser "--texts" :nargs "+" :default None
                  :help "Text chunks to add to Vector Store")
    
    (.add_argument parser "--texts-file" :default None
                  :help "JSON file containing texts to add")
    
    (.add_argument parser "--create-rag" :action "store_true"
                  :help "Create a RAG chain")
    
    (.add_argument parser "--system-prompt" :default """You are an AI assistant.
Answer the questions using only the facts provided in the context.
If you don't know the answer based on the provided context, just say "I don't have enough information to answer this question."
Context: {summaries}"""
                  :help "System prompt for RAG")
    
    (.add_argument parser "--query" :default None
                  :help "Query to process through RAG chain")
    
    (.add_argument parser "--output" :dest "output_path" :default None
                  :help "Path to save setup results as JSON")
    
    ;; Parse arguments
    (setv args (.parse_args parser))
    
    ;; Load texts from file if specified
    (when args.texts_file
      (try
        (with [f (open args.texts_file "r")]
          (setv file-data (json.load f))
          (setv args.texts (get file-data "texts")))
        (except [e Exception]
          (print f"Error loading texts file: {e}" :file sys.stderr))))
    
    args))

(defn main []
  "\"Main entry point for command line use.\""
  (let [args (parse-args)]
    
    ;; Run the setup pipeline
    (let [results (setup-complete-pipeline args)]
      
      ;; Save results to file if requested
      (when (and results args.output_path)
        (with [f (open args.output_path "w")]
          (json.dump results f :indent 2)))
      
      ;; Print results to stdout
      (print "\nSetup Results:")
      (print (json.dumps results :indent 2)))))

;; Run main when script is executed directly
(when (= __name__ "__main__")
  (main))
