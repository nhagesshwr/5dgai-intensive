;; vertex_vector_search.hy
;; Embeddings & Vector Stores
;; Part of Day 2 Assignment: 5-Day Gen AI Intensive
;; February 2025
;;
;; Implementation leveraging HNSW (Hierarchical Navigable Small World) concept
;; for vector similarity search with O(logn) runtime

(require [hy.pyops [*]])
(import [google.cloud [aiplatform]])
(import [langchain.chains [RetrievalQA]])
(import [langchain.prompts [ChatPromptTemplate SystemMessagePromptTemplate HumanMessagePromptTemplate]])
(import [langchain_google_vertexai [VertexAI]])
(import [langchain_google_vertexai_vector_stores [VectorSearchVectorStore]])
(import [IPython.display [Markdown display]])

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

;; Retrieve the id of the most recently deployed index or manually look up the index deployed above
(setv index-id (-> my-index-endpoint 
                   (get "deployed_indexes") 
                   (get -1) 
                   (get "index") 
                   (.split "/") 
                   (get -1)))

(setv endpoint-id my-index-endpoint.name)

;; TODO: Replace 1234567890123456789 with your actual index ID
(setv my-index (aiplatform.MatchingEngineIndex index-id))

;; TODO: Replace 1234567890123456789 with your actual endpoint ID
;; Be aware that the Index ID differs from the endpoint ID
(setv my-index-endpoint (aiplatform.MatchingEngineIndexEndpoint endpoint-id))

;; Input texts
(setv texts [
  "The earth is spherical."
  "The earth is a planet."
  "I like to eat at a restaurant."
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
(setv retriever (.as_retriever vector-store :search_kwargs {"k" 1}))

;; Create custom prompt for your use case
(setv prompt-template """You are David, an AI knowledge bot.
Answer the questions using the facts provided. Use the provided pieces of context to answer
the users question.
If you don't know the answer, just say that "I don't know", don't try to make up an answer.
{summaries}""")

(setv messages [
  (SystemMessagePromptTemplate.from_template prompt-template)
  (HumanMessagePromptTemplate.from_template "{question}")
])

(setv prompt (ChatPromptTemplate.from_messages messages))
(setv chain-type-kwargs {"question" prompt})

;; Initialize your LLM model
(setv llm (VertexAI :model_name "gemini-pro"))

;; Build your chain for RAG+C
(setv chain 
  (RetrievalQA.from_chain_type 
    :llm llm 
    :chain_type "stuff"
    :retriever retriever 
    :return_source_documents True))

;; Print your results with Markup language
(defn print-result [result]
  (setv output-text f"""### Question:
{query}
### Answer:
{(get result "result")}
### Source:
{" ".join (list (set (lfor doc (get result "source_documents") doc.page_content)))}
""")
  output-text)

;; Execute the chain with a query
(setv query "What shape is the planet where humans live?")
(setv result (chain query))
(display (Markdown (print-result result)))
