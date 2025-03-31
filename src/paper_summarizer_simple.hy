#!/usr/bin/env hy
;; Simple Paper Summarizer in Hy
;; Takes a PDF path and prints summary to stdout

(import sys
        [pathlib [Path]]
        [google.genai [genai types]])

(defn summarize-paper [pdf-path]
  "Summarize a PDF paper using Gemini API and print to stdout"
  ;; Initialize API client
  (setv client (genai.Client))
  
  ;; Read PDF content
  (setv pdf-content (.read_bytes (Path pdf-path)))
  
  ;; Generate summary
  (setv response (.generate_content client.models
                                   :model "gemini-1.5-flash"
                                   :contents [(types.Part.from_bytes
                                               :data pdf-content
                                               :mime_type "application/pdf")
                                             "Summarize this academic paper in markdown format."]))
  
  ;; Return the summary text
  (.text response))

(defn main []
  "Main entry point for command line use"
  (when (< (len sys.argv) 2)
    (print "Usage: hy paper_summarizer_simple.hy <pdf_path>")
    (sys.exit 1))
  
  ;; Get PDF path from arguments
  (setv pdf-path (get sys.argv 1))
  
  ;; Check if PDF exists
  (when (not (Path pdf-path).exists)
    (print f"Error: PDF file {pdf-path} not found" :file sys.stderr)
    (sys.exit 1))
  
  ;; Generate and print summary
  (print (summarize-paper pdf-path)))

(when (= __name__ "__main__")
  (main))