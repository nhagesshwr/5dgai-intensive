#!/usr/bin/env hy
;; Paper Summarizer in Hy
;; Summarizes academic papers using Google's Gemini API

(import os)
(import sys)
(import pathlib)
(import google.genai)
(import httpx)

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
        (.read_bytes (pathlib.Path paper-path)))))

(defn summarize-paper [paper-path prompt-path output-path]
  "Summarize a paper using Gemini API"
  (print f"Summarizing paper: {paper-path}")
  (print f"Using prompt from: {prompt-path}")
  (print f"Output will be saved to: {output-path}")
  
  ;; Initialize the API client
  (setv client (google.genai.genai.Client))
  
  ;; Load the prompt template
  (setv prompt (load-prompt prompt-path))
  
  ;; Get PDF content
  (setv pdf-content (get-pdf-content paper-path))
  
  ;; Generate summary using Gemini API
  (print "Generating summary...")
  (setv response (.generate_content client.models
                                   :model "gemini-1.5-flash"
                                   :contents [(google.genai.types.Part.from_bytes
                                               :data pdf-content
                                               :mime_type "application/pdf")
                                             prompt]))
  
  ;; Save the summary
  (print f"Saving summary to {output-path}")
  (with [f (open output-path "w")]
    (.write f (.text response)))
  
  (print "Summary generation complete!"))

(defn main []
  "Main entry point for the script"
  (when (< (len sys.argv) 4)
    (print "Usage: hy paper_summarizer.hy <paper_path> <prompt_path> <output_path>")
    (sys.exit 1))
  
  (setv paper-path (get sys.argv 1))
  (setv prompt-path (get sys.argv 2))
  (setv output-path (get sys.argv 3))
  
  (summarize-paper paper-path prompt-path output-path))

(when (= __name__ "__main__")
  (main))