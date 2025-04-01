#!/usr/bin/env hy
"\"
Summarize Top PDFs

This script finds the first 10 PDFs in the papers directory and
summarizes them using the paper_summarizer.hy script, with proper
dependency tracking to avoid unnecessary regeneration.

Part of 5-Day Gen AI Intensive Course
\""

(import os)
(import sys)
(import subprocess)
(import argparse)

(defn find-pdf-files [base-dir]
  "\"
  Find all PDF files in the given directory and its subdirectories.
  
  Args:
      base-dir: Base directory to search
      
  Returns:
      List of PDF file paths
  \""
  (setv pdf-files [])
  
  (for [[root dirs files] (os.walk base-dir)]
    (for [file files]
      (when (.endswith (.lower file) ".pdf")
        (.append pdf-files (os.path.join root file)))))
  
  ;; Sort by modification time (newest first)
  (.sort pdf-files :key (fn [f] (- (os.path.getmtime f))))
  
  pdf-files)

(defn write-papers-list [papers limit output-file]
  "\"
  Write a list of paper paths to a file.
  
  Args:
      papers: List of paper paths
      limit: Maximum number of papers to include
      output-file: Path to write the paper list
      
  Returns:
      Number of papers written
  \""
  (setv count 0)
  
  (with [f (open output-file "w")]
    (.write f "# List of papers to summarize\n")
    (.write f "# One paper path per line\n\n")
    
    (for [paper papers]
      (when (< count limit)
        (.write f f"{paper}\n")
        (setv count (+ count 1)))))
  
  count)

(defn main []
  "\"Main entry point for the script\""
  ;; Parse command line arguments
  (let [parser (argparse.ArgumentParser 
                 :description "Summarize the first N PDF files in the papers directory")]
    
    (.add_argument parser "--papers-dir" :default "papers"
                  :help "Directory containing PDF papers (default: papers)")
    
    (.add_argument parser "--count" :type int :default 10
                  :help "Number of PDFs to summarize (default: 10)")
    
    (.add_argument parser "--prompt" :default "prompts/summarize-paper.md"
                  :help "Path to prompt template (default: prompts/summarize-paper.md)")
    
    (.add_argument parser "--output-dir" :default "papers/summaries"
                  :help "Directory to save summaries (default: papers/summaries)")
    
    (.add_argument parser "--force" :action "store_true"
                  :help "Force regeneration even if papers haven't changed")
    
    (.add_argument parser "--model" :default "gemini-1.5-flash"
                  :help "Gemini model to use (default: gemini-1.5-flash)")
    
    (setv args (.parse_args parser)))
  
  ;; Ensure papers directory exists
  (when (not (os.path.isdir args.papers_dir))
    (print f"Error: Papers directory not found: {args.papers_dir}")
    (return 1))
  
  ;; Ensure prompt file exists
  (when (not (os.path.exists args.prompt))
    (print f"Error: Prompt template not found: {args.prompt}")
    (return 1))
  
  ;; Create output directory if it doesn't exist
  (os.makedirs args.output_dir :exist_ok True)
  
  ;; Find PDF files
  (print f"Finding PDF files in {args.papers_dir}...")
  (setv pdf-files (find-pdf-files args.papers_dir))
  
  (when (not pdf-files)
    (print "Error: No PDF files found")
    (return 1))
  
  (print f"Found {(len pdf-files)} PDF files")
  
  ;; Write paper list to temporary file
  (setv papers-list-file (os.path.join args.output_dir "papers-list.txt"))
  (setv count (write-papers-list pdf-files args.count papers-list-file))
  
  (print f"Selected {count} papers for summarization")
  
  ;; Build command for the paper summarizer
  (setv cmd ["hy" "src/paper_summarizer.hy" 
            "--papers-file" papers-list-file
            "--prompt" args.prompt
            "--output-dir" args.output_dir])
  
  (when args.force
    (.append cmd "--force"))
  
  (when args.model
    (.append cmd "--model")
    (.append cmd args.model))
  
  ;; Execute the paper summarizer
  (print "Running paper summarizer...")
  (print f"Command: {' '.join cmd}")
  (setv result (subprocess.call cmd))
  
  (when (!= result 0)
    (print "Error: Paper summarizer failed")
    (return result))
  
  (print f"\nSummaries generated successfully in {args.output_dir}")
  0)

(when (= __name__ "__main__")
  (sys.exit (main)))