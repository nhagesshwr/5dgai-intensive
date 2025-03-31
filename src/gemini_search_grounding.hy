#!/usr/bin/env hy

(import [google [genai]])
(import [google.genai.types [Tool GenerateContentConfig GoogleSearch GoogleSearchRetrieval DynamicRetrievalConfig]])
(import [os [getenv]])

(defn initialize-client [&optional [api-key (getenv "GEMINI_API_KEY")]]
  "Initialize the Gemini API client with the provided API key."
  (genai.Client :api-key api-key))

(defn search-as-tool [client query &optional [model-id "gemini-2.0-flash"]]
  "Use Google Search as a tool with Gemini 2.0 models.
   This allows the model to decide when to use Google Search."
  (let [google-search-tool (Tool :google-search (GoogleSearch))
        response (client.models.generate-content
                   :model model-id
                   :contents query
                   :config (GenerateContentConfig
                             :tools [google-search-tool]
                             :response-modalities ["TEXT"]))]
    response))

(defn print-search-tool-response [response]
  "Print the response text and grounding metadata from a search-as-tool query."
  (for [part (. (. (. response candidates [0]) content) parts)]
    (print (. part text)))
  (print "\n--- Search Grounding Metadata ---")
  (print (. (. (. (. response candidates [0]) grounding-metadata) search-entry-point) rendered-content)))

(defn search-retrieval [client query &optional [model-id "gemini-1.5-flash"] [threshold None]]
  "Use Google Search retrieval with Gemini 1.5 models.
   Note: This is only compatible with Gemini 1.5 models."
  (let [search-config (if threshold
                        (GoogleSearchRetrieval 
                          :dynamic-retrieval-config 
                          (DynamicRetrievalConfig :dynamic-threshold threshold))
                        (GoogleSearchRetrieval))
        response (client.models.generate-content
                   :model model-id
                   :contents query
                   :config (GenerateContentConfig
                             :tools [(Tool :google-search search-config)]))]
    response))

(defn print-search-retrieval-response [response]
  "Print the response from a search retrieval query."
  (print response))

(defn main []
  "Demonstrate the usage of Google Search grounding with Gemini models."
  (let [client (initialize-client)
        search-query "When is the next total solar eclipse in the United States?"]
    
    (print "=== Using Search as a Tool (Gemini 2.0) ===")
    (let [tool-response (search-as-tool client search-query)]
      (print-search-tool-response tool-response))
    
    (print "\n=== Using Search Retrieval (Gemini 1.5) ===")
    (let [retrieval-response (search-retrieval client search-query)]
      (print-search-retrieval-response retrieval-response))
    
    (print "\n=== Using Search Retrieval with Dynamic Threshold ===")
    (let [threshold-response (search-retrieval client "Who won Roland Garros this year?" :threshold 0.6)]
      (print-search-retrieval-response threshold-response))))

(when (= __name__ "__main__")
  (main))
