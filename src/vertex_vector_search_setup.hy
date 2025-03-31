;; vertex_vector_search_setup.hy
;; Embeddings & Vector Stores
;; Part of Day 2 Assignment: 5-Day Gen AI Intensive
;; February 2025

;; Before you start run this command:
;; pip install --upgrade --user --quiet google-cloud-aiplatform langchain_google_vertexai
;; after running pip install make sure you restart your kernel

(require [hy.pyops [*]])

;; TODO: Set values as per your requirements
;; Project and Storage Constants
(setv PROJECT_ID "<my_project_id>")
(setv REGION "<my_region>")
(setv BUCKET "<my_gcs_bucket>")
(setv BUCKET_URI f"gs://{BUCKET}")

;; The number of dimensions for the text-embedding-005 is 768
;; If other embedder is used, the dimensions would probably need to change.
(setv DIMENSIONS 768)

;; Index Constants
(setv DISPLAY_NAME "<my_matching_engine_index_id>")
(setv DEPLOYED_INDEX_ID "yourname01")  ;; you set this. Start with a letter.

;; Import necessary libraries
(import [google.cloud [aiplatform]])
(import [langchain_google_vertexai [VertexAIEmbeddings VertexAI]])
(import [langchain_google_vertexai [VectorSearchVectorStore VectorSearchVectorStoreDatastore]])
(import [langchain.chains [RetrievalQA]])
(import [langchain.prompts.chat [ChatPromptTemplate SystemMessagePromptTemplate HumanMessagePromptTemplate]])
(import [IPython.display [display Markdown]])

;; Initialize AI Platform
(aiplatform.init :project PROJECT_ID :location REGION :staging_bucket BUCKET_URI)

;; Create embedding model
(setv embedding-model (VertexAIEmbeddings :model_name "text-embedding-005"))

;; NOTE: This operation can take up to 30 seconds
(setv my-index 
  (aiplatform.MatchingEngineIndex.create_tree_ah_index
    :display_name DISPLAY_NAME
    :dimensions DIMENSIONS
    :approximate_neighbors_count 150
    :distance_measure_type "DOT_PRODUCT_DISTANCE"
    :index_update_method "STREAM_UPDATE"))  ;; allowed values BATCH_UPDATE, STREAM_UPDATE

;; Create an endpoint
(setv my-index-endpoint 
  (aiplatform.MatchingEngineIndexEndpoint.create
    :display_name f"{DISPLAY_NAME}-endpoint" 
    :public_endpoint_enabled True))

;; NOTE: This operation can take up to 20 minutes
(setv my-index-endpoint 
  (.deploy_index my-index-endpoint
    :index my-index 
    :deployed_index_id DEPLOYED_INDEX_ID))

;; Check deployed indexes
(print my-index-endpoint.deployed_indexes)

;; TODO: Replace 1234567890123456789 with your actual index ID
(setv my-index (aiplatform.MatchingEngineIndex "1234567890123456789"))

;; TODO: Replace 1234567890123456789 with your actual endpoint ID
;; Be aware that the Index ID differs from the endpoint ID
(setv my-index-endpoint (aiplatform.MatchingEngineIndexEndpoint "1234567890123456789"))

;; Input texts
(setv texts [
  "The cat sat on"
  "the mat."
  "I like to"
  "eat pizza for"
  "dinner."
  "The sun sets"
  "in the west."
])

;; Create a Vector Store
(setv vector-store 
  (VectorSearchVectorStore.from_components
    :project_id PROJECT_ID
    :region REGION
    :gcs_bucket_name BUCKET
    :index_id my-index.name
    :endpoint_id my-index-endpoint.name
    :embedding embedding-model
    :stream_update True))

;; Add vectors and mapped text chunks to your vector store
(.add_texts vector-store :texts texts)

;; Initialize the vector_store as retriever
(setv retriever (.as_retriever vector-store))

;; Perform simple similarity search on retriever
(print (.invoke retriever "What are my options in breathable fabric?"))

;; Example of how to create a RAG system with this retriever
;; Create custom prompt template
(setv prompt-template """You are an AI assistant.
Answer the questions using only the facts provided in the context.
If you don't know the answer based on the provided context, just say "I don't have enough information to answer this question."
Context: {summaries}""")

(setv messages [
  (SystemMessagePromptTemplate.from_template prompt-template)
  (HumanMessagePromptTemplate.from_template "{question}")
])

(setv prompt (ChatPromptTemplate.from_messages messages))

;; Initialize LLM
(setv llm (VertexAI :model_name "gemini-pro"))

;; Build RAG chain
(setv rag-chain 
  (RetrievalQA.from_chain_type 
    :llm llm 
    :chain_type "stuff"
    :retriever retriever 
    :return_source_documents True))

;; Define function to display results
(defn display-result [result]
  (setv formatted-output f"""### Question:
{(get result "query")}

### Answer:
{(get result "result")}

### Sources:
{(lfor doc (get result "source_documents") doc.page_content)}
""")
  (display (Markdown formatted-output)))

;; Example query
;; (setv query-result (rag-chain {"query" "What animals are mentioned in the texts?"}))
;; (display-result query-result)
