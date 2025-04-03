#!/usr/bin/env hy
;;; Simple LangGraph Agent Example in Hy
;;; Demonstrates a minimal barista ordering agent with Gemini and LangGraph

(import os)
(import json)
(import [typing [Dict List Literal Optional TypedDict Any]])
(import [dotenv [load-dotenv]])

;; Import LangGraph components
(import [langgraph.graph [StateGraph START END]])
(import [langchain-google-genai [ChatGoogleGenerativeAI]])
(import [langchain-core.messages [AIMessage HumanMessage SystemMessage]])

;; Define OrderState as a TypedDict
(defclass OrderState [TypedDict]
  (typing.field [messages List] [order List] [finished bool]))

;; System instructions for the barista bot
(setv BARISTABOT-SYSINT (SystemMessage :content "
You are BaristaBot, an AI assistant helping customers order coffee at a café.

Be friendly, helpful, and professional. Ask clarifying questions when needed.

Keep track of the customer's order items. When they want to complete their order,
provide a summary of all items and total cost.
"))

;; Define a simplified menu
(setv CAFE-MENU {
  "coffee" {
    "espresso" {"small" 2.50 "medium" 3.00 "large" 3.50}
    "latte" {"small" 3.50 "medium" 4.00 "large" 4.50}
  }
  "pastries" {
    "croissant" 2.50
    "muffin" 3.00
  }
})

(defn setup-client []
  "Setup and configure the Gemini client"
  ;; Load environment variables
  (load-dotenv)
  (setv api-key (os.getenv "AI_STUDIO_API_KEY"))
  
  ;; Create the LLM
  (ChatGoogleGenerativeAI :model "gemini-2.0-flash" 
                         :google_api_key api-key))

(defn chatbot [state]
  "The chatbot itself. A wrapper around the model's chat interface."
  (setv llm (setup-client))
  
  ;; Create message history with system instructions
  (setv message-history [(+ [BARISTABOT-SYSINT] (get state "messages"))])
  
  ;; Invoke the model
  (setv response (llm.invoke message-history))
  
  ;; Return updated state
  {"messages" [(get state "messages") response]})

(defn build-simple-graph []
  "Build a simple LangGraph for the barista agent"
  (print "Building simple LangGraph agent...")
  
  ;; Create graph builder with our state definition
  (setv graph-builder (StateGraph OrderState))
  
  ;; Add the chatbot function as a node
  (graph-builder.add_node "chatbot" chatbot)
  
  ;; Define the chatbot node as the entry point
  (graph-builder.add_edge START "chatbot")
  
  ;; Compile the graph
  (graph-builder.compile))

;; Example of how to use the agent
(defn run-agent []
  "Run the barista agent with a simple test conversation"
  (setv chat-graph (build-simple-graph))
  
  ;; Create initial state
  (setv state {"messages" [(HumanMessage :content "Hi, I'd like to see the menu please.")]})
  
  ;; Invoke the graph
  (setv result (chat-graph.invoke state))
  
  ;; Print the result
  (print "Customer: Hi, I'd like to see the menu please.")
  (print "BaristaBot:" (get (get result "messages" -1) "content")))

(defn main []
  "Main function to demonstrate LangGraph with Gemini"
  (print "LangGraph Agent Example with Gemini")
  (print "===================================")
  
  ;; Check if we can run the agent
  (try
    (import langgraph.graph)
    (import langchain_google_generai)
    
    ;; Run a simplified example
    (run-agent)
    
    (except [e ImportError]
      (print "Error: Required packages not installed.")
      (print f"Details: {e}")
      (print "Try installing: pip install langgraph langchain-google-genai"))))

(when (= __name__ "__main__")
  (main))