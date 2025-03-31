#!/usr/bin/env hy
;; Paper Summarizer in Hy
;; Summarizes academic papers using Google's Gemini API

(import [os]
        [sys]
        [pathlib [Path]]
        [google.genai [genai types]]
        [httpx])

(defn load-prompt [prompt-path]
  "Load the prompt template from the given path"
  (with [f (open prompt-path "r")]
    (.read f)))

(defn get-pdf-content [paper-path]
  "Get PDF content either from a local file or by downloading from URL"
  (if (.startswith paper-path "http")
      ;; Download the PDF
      (do
        (print f"Downloading PDF from {paper-path}")
        (let [response (httpx.get paper-path)
              content (.content response)]
          content))
      ;; Read the local file
      (do
        (print f"Reading local PDF file {paper-path}")
        (.read_bytes (Path paper-path)))))

(defn summarize-paper [paper-path prompt-path output-path]
  "Summarize a paper using Gemini API"
  (print f"Summarizing paper: {paper-path}")
  (print f"Using prompt from: {prompt-path}")
  (print f"Output will be saved to: {output-path}")
  
  ;; Initialize the API client
  (setv client (genai.Client))
  
  ;; Load the prompt template
  (setv prompt (load-prompt prompt-path))
  
  ;; Get PDF content
  (setv pdf-content (get-pdf-content paper-path))
  
  ;; Generate summary using Gemini API
  (print "Generating summary...")
  (setv response (client.models.generate_content
                   :model "gemini-1.5-flash"
                   :contents [(types.Part.from_bytes
                                :data pdf-content
                                :mime_type "application/pdf")
                              prompt]))
  
  ;; Save the summary
  (print f"Saving summary to {output-path}")
  (with [f (open output-path "w")]
    (.write f (.text response)))
  
  (print "Summary generation complete!")
  (.text response))

(defmain [&rest args]
  "Main entry point for the script"
  (if (< (len args) 4)
      (do
        (print "Usage: hy paper_summarizer.hy <paper_path> <prompt_path> <output_path>")
        (sys.exit 1)))
  
  (setv [_ paper-path prompt-path output-path] args)
  
  (try
    (summarize-paper paper-path prompt-path output-path)
    (except [e Exception]
      (print f"Error: {e}")
      (sys.exit 1))))