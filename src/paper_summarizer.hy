#!/usr/bin/env hy
"\"
Paper Summarizer

Summarizes academic papers using Google's Gemini API with efficient dependency
tracking to avoid regenerating summaries when inputs have not changed.

Part of 5-Day Gen AI Intensive Course
\""

(import os)
(import sys)
(import json)
(import pathlib)
(import google.genai)
(import httpx)
(import hashlib)
(import argparse)
(import datetime)

(defn calculate-fingerprint [pdf-content prompt-text]
  "\"
  Calculate a fingerprint for PDF content and prompt to track changes.
  
  Args:
      pdf-content: Binary content of the PDF
      prompt-text: Text of the prompt template
      
  Returns:
      Hash string representing the fingerprint
  \""
  (setv hash-obj (hashlib.sha256))
  (.update hash-obj pdf-content)
  (.update hash-obj (.encode prompt-text "utf-8"))
  (.hexdigest hash-obj))

(defn load-prompt [prompt-path]
  "\"
  Load the prompt template from the given path.
  
  Args:
      prompt-path: Path to the prompt template file
      
  Returns:
      Prompt text
  \""
  (with [f (open prompt-path "r")]
    (.read f)))

(defn get-pdf-content [paper-path]
  "\"
  Get PDF content either from a local file or by downloading from URL.
  
  Args:
      paper-path: Path or URL to the PDF
      
  Returns:
      Binary content of the PDF
  \""
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

(defn check-for-existing-summary [output-path paper-path prompt-path]
  "\"
  Check if an existing summary needs to be regenerated.
  
  Args:
      output-path: Path where the summary is stored
      paper-path: Path to the PDF file
      prompt-path: Path to the prompt template
      
  Returns:
      Tuple of (needs_update, fingerprint)
      needs_update: Boolean indicating if the summary needs to be regenerated
      fingerprint: Current fingerprint of the inputs
  \""
  ;; If output doesn't exist, we need to generate it
  (when (not (os.path.exists output-path))
    (print f"No existing summary found at {output-path}")
    (return [True None]))
  
  ;; Load the PDF content and prompt
  (setv pdf-content (get-pdf-content paper-path))
  (setv prompt-text (load-prompt prompt-path))
  
  ;; Calculate the current fingerprint
  (setv current-fingerprint (calculate-fingerprint pdf-content prompt-text))
  
  ;; Check for metadata file
  (setv metadata-path f"{output-path}.meta")
  
  (if (os.path.exists metadata-path)
      ;; We have metadata, check fingerprint
      (do
        (with [f (open metadata-path "r")]
          (setv metadata (json.load f))
          (setv stored-fingerprint (get metadata "fingerprint"))
          
          (if (= stored-fingerprint current-fingerprint)
              ;; Fingerprints match, no need to regenerate
              (do
                (print f"Inputs unchanged since last generation ({(get metadata "timestamp")})")
                [False current-fingerprint])
              ;; Fingerprints don't match, need to regenerate
              (do
                (print "Inputs have changed, regenerating summary")
                [True current-fingerprint]))))
      ;; No metadata file, need to regenerate
      (do
        (print "No metadata found for existing summary, regenerating")
        [True current-fingerprint])))

(defn save-metadata [output-path fingerprint]
  "\"
  Save metadata for the generated summary.
  
  Args:
      output-path: Path to the summary file
      fingerprint: Fingerprint of the input data
      
  Returns:
      None
  \""
  (setv metadata-path f"{output-path}.meta")
  (setv timestamp (str (datetime.datetime.now)))
  
  (with [f (open metadata-path "w")]
    (json.dump {"fingerprint" fingerprint
                "timestamp" timestamp} f :indent 2)))

(defn summarize-paper [paper-path prompt-path output-path &optional [force False] [model "gemini-1.5-flash"]]
  "\"
  Summarize a paper using Gemini API, with dependency checking.
  
  Args:
      paper-path: Path to the PDF file
      prompt-path: Path to the prompt template
      output-path: Path to save the summary
      force: Force regeneration even if inputs haven't changed
      model: Gemini model to use
      
  Returns:
      True if summary was generated, False if skipped
  \""
  (print f"Processing paper: {paper-path}")
  
  ;; Check if we need to regenerate the summary
  (setv [needs-update fingerprint] 
    (if force 
        [True None]
        (check-for-existing-summary output-path paper-path prompt-path)))
  
  (when (not needs-update)
    (print f"Skipping generation - existing summary is up to date")
    (return False))
  
  (print f"Summarizing paper: {paper-path}")
  (print f"Using prompt from: {prompt-path}")
  (print f"Output will be saved to: {output-path}")
  
  ;; Initialize the API client
  (setv client (google.genai.genai.Client))
  
  ;; Load the prompt template
  (setv prompt (load-prompt prompt-path))
  
  ;; Get PDF content
  (setv pdf-content (get-pdf-content paper-path))
  
  ;; Calculate fingerprint if not done already
  (when (is fingerprint None)
    (setv fingerprint (calculate-fingerprint pdf-content prompt)))
  
  ;; Generate summary using Gemini API
  (print f"Generating summary using {model}...")
  (setv response (.generate_content client.models
                                   :model model
                                   :contents [(google.genai.types.Part.from_bytes
                                               :data pdf-content
                                               :mime_type "application/pdf")
                                             prompt]))
  
  ;; Save the summary
  (print f"Saving summary to {output-path}")
  (with [f (open output-path "w")]
    (.write f (.text response)))
  
  ;; Save metadata
  (save-metadata output-path fingerprint)
  
  (print "Summary generation complete!")
  True)

(defn summarize-multiple-papers [papers-list prompt-path &optional [output-dir None] [force False] [model "gemini-1.5-flash"]]
  "\"
  Summarize multiple papers specified in a list.
  
  Args:
      papers-list: List of paths to PDF files
      prompt-path: Path to the prompt template
      output-dir: Directory to save summaries (default: use same dir as PDFs)
      force: Force regeneration even if inputs haven't changed
      model: Gemini model to use
      
  Returns:
      Dictionary with results for each paper
  \""
  (setv results {})
  
  (for [paper-path papers-list]
    ;; Determine output path
    (setv paper-filename (os.path.basename paper-path))
    (setv paper-basename (get (os.path.splitext paper-filename) 0))
    (setv summary-filename f"{paper-basename}.summary.txt")
    
    (setv output-path 
      (if output-dir
          (os.path.join output-dir summary-filename)
          (os.path.join (os.path.dirname paper-path) summary-filename)))
    
    ;; Ensure output directory exists
    (os.makedirs (os.path.dirname output-path) :exist_ok True)
    
    ;; Summarize the paper
    (try
      (setv generated (summarize-paper paper-path prompt-path output-path :force force :model model))
      (setv (get results paper-path) {"success" True
                                     "generated" generated
                                     "output_path" output-path})
      (except [e Exception]
        (setv (get results paper-path) {"success" False
                                       "error" (str e)}))))
  
  results)

;; Parser for command-line arguments
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser 
                 :description "Summarize academic papers using Gemini API")]
    
    ;; Input options
    (.add_argument parser "--paper" :dest "papers" :action "append"
                  :help "Path to a PDF paper (can be specified multiple times)")
    
    (.add_argument parser "--papers-file" 
                  :help "File containing list of paper paths (one per line)")
    
    (.add_argument parser "--prompt" :required True
                  :help "Path to prompt template file")
    
    ;; Output options
    (.add_argument parser "--output-dir"
                  :help "Directory to save summaries (default: same as paper)")
    
    ;; Processing options
    (.add_argument parser "--force" :action "store_true"
                  :help "Force regeneration even if paper unchanged")
    
    (.add_argument parser "--model" :default "gemini-1.5-flash"
                  :help "Gemini model to use (default: gemini-1.5-flash)")
    
    ;; Legacy mode - support old command line syntax
    (.add_argument parser "legacy_args" :nargs "*" 
                  :help="Legacy positional arguments (paper prompt output)")
    
    (.parse_args parser)))

(defn main []
  "\"Main entry point for the script\""
  (let [args (parse-args)]
    
    ;; Handle legacy mode (3 positional args)
    (when (= (len args.legacy_args) 3)
      (setv paper-path (get args.legacy_args 0))
      (setv prompt-path (get args.legacy_args 1))
      (setv output-path (get args.legacy_args 2))
      (summarize-paper paper-path prompt-path output-path :force args.force :model args.model)
      (return 0))
    
    ;; Check that we have paper sources
    (when (and (not args.papers) (not args.papers_file))
      (print "Error: No papers specified. Use --paper or --papers-file")
      (return 1))
    
    ;; Collect list of papers
    (setv papers [])
    (when args.papers
      (.extend papers args.papers))
    
    (when args.papers_file
      (with [f (open args.papers_file "r")]
        (for [line (.readlines f)]
          (setv line (.strip line))
          (when (and line (not (.startswith line "#")))
            (.append papers line)))))
    
    ;; Summarize the papers
    (setv results (summarize-multiple-papers 
                  papers 
                  args.prompt 
                  :output-dir args.output_dir
                  :force args.force
                  :model args.model))
    
    ;; Print summary of results
    (setv success-count (sum (lfor k (results.values) (get k "success"))))
    (setv generated-count (sum (lfor k (results.values) (and (get k "success") (get k "generated")))))
    (setv skipped-count (- (len papers) generated-count))
    
    (print f"\nSummary:")
    (print f"  Total papers: {(len papers)}")
    (print f"  Successfully processed: {success-count}")
    (print f"  Updated summaries: {generated-count}")
    (print f"  Skipped (unchanged): {skipped-count}")
    
    (when (< success-count (len papers))
      (print "\nErrors:")
      (for [[paper info] (.items results)]
        (when (not (get info "success"))
          (print f"  {paper}: {(.get info \"error\")}")))
      (return 1))
    
    0))

(when (= __name__ "__main__")
  (sys.exit (main)))