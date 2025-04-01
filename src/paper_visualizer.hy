#!/usr/bin/env hy
;; paper_visualizer.hy - Visualize relationships between AI research papers
;; Usage: poetry run hy paper_visualizer.hy

(import os)
(import sys)
(import json)
(import re)
(from collections import defaultdict)
(from datetime import datetime)

;; Configuration
(setv PAPERS-DIR (os.path.abspath (os.path.expanduser "../papers")))
(setv CATALOG-FILE (os.path.join PAPERS-DIR "papers_catalog.json"))
(setv OUTPUT-DIR (os.path.join PAPERS-DIR "viz"))

;; Create output directory if it doesn't exist
(os.makedirs OUTPUT-DIR :exist-ok True)

;; Load the catalog
(defn load-catalog []
  """Load the existing catalog if it exists."""
  (try
    (do
      (when (os.path.exists CATALOG-FILE)
        (with [f (open CATALOG-FILE "r")]
          (setv catalog (json.load f))
          (print f"Loaded catalog with {(len catalog)} papers")
          catalog))
        {})  ; Return empty dict if file doesn't exist
    (except [e json.JSONDecodeError]
      (print f"Warning: Could not parse catalog file. Starting fresh.")
      {})))

;; Generate Mermaid timeline diagram
(defn generate-timeline-diagram [catalog]
  """Generate a Mermaid timeline diagram of papers by year."""
  (setv years (defaultdict list))
  
  ;; Group papers by year
  (for [[filename paper] (.items catalog)]
    (setv year (get paper "year" "unknown"))
    (.append (get years year) paper))
  
  ;; Start diagram
  (setv mermaid-code "```mermaid\ntimeline\n")
  
  ;; Add years in order
  (for [year (sorted (list (.keys years)))]
    (.append mermaid-code f"    title {year}\n")
    (for [paper (get years year)]
      (setv author (get paper "author" "unknown"))
      (setv topic (.replace (get paper "topic" "unknown") "-" " "))
      (.append mermaid-code f"    section {author}\n")
      (.append mermaid-code f"    {topic} : {(get paper 'filename')}\n")))
  
  ;; Close diagram
  (.append mermaid-code "```\n")
  
  ;; Write to file
  (setv output-file (os.path.join OUTPUT-DIR "papers_timeline.md"))
  (with [f (open output-file "w")]
    (f.write (.join "" mermaid-code)))
  
  (print f"Generated timeline diagram: {output-file}")
  mermaid-code)

;; Generate Mermaid mind map diagram by topic
(defn generate-topic-diagram [catalog]
  """Generate a Mermaid mind map diagram of papers by topic."""
  (setv topics (defaultdict list))
  
  ;; Extract unique topics
  (for [[filename paper] (.items catalog)]
    (setv topic (.replace (get paper "topic" "unknown") "-" " "))
    (.append (get topics topic) paper))
  
  ;; Start diagram
  (setv mermaid-code "```mermaid\nmindmap\n  root((AI Research))\n")
  
  ;; Add topics
  (for [topic (sorted (list (.keys topics)))]
    (.append mermaid-code f"    {topic}\n")
    (for [paper (get topics topic)]
      (setv author (get paper "author" "unknown"))
      (setv year (get paper "year" "unknown"))
      (.append mermaid-code f"      {author} {year}\n")
      (.append mermaid-code f"        [{(get paper 'filename')}]\n")))
  
  ;; Close diagram
  (.append mermaid-code "```\n")
  
  ;; Write to file
  (setv output-file (os.path.join OUTPUT-DIR "papers_topics.md"))
  (with [f (open output-file "w")]
    (f.write (.join "" mermaid-code)))
  
  (print f"Generated topic diagram: {output-file}")
  mermaid-code)

;; Generate Mermaid graph diagram by citations
(defn generate-author-network [catalog]
  """Generate a Mermaid graph diagram of papers by author network."""
  (setv authors (defaultdict list))
  
  ;; Group papers by author
  (for [[filename paper] (.items catalog)]
    (setv author (get paper "author" "unknown"))
    (.append (get authors author) paper))
  
  ;; Start diagram
  (setv mermaid-code "```mermaid\ngraph TD\n")
  
  ;; Add authors and papers
  (for [author (sorted (list (.keys authors)))]
    (.append mermaid-code f"  {author}[{author}]\n")
    (for [paper (get authors author)]
      (setv paper-id (.replace (get paper "filename" "") ".pdf" ""))
      (setv topic (.replace (get paper "topic" "unknown") "-" " "))
      (.append mermaid-code f"  {paper-id}[\"{topic}\"]\n")
      (.append mermaid-code f"  {author} --> {paper-id}\n")))
  
  ;; Add year connections
  (setv years (defaultdict list))
  (for [[filename paper] (.items catalog)]
    (setv year (get paper "year" "unknown"))
    (.append (get years year) paper))
  
  (for [year (sorted (list (.keys years)))]
    (.append mermaid-code f"  year{year}(('{year}'))\n")
    (for [paper (get years year)]
      (setv paper-id (.replace (get paper "filename" "") ".pdf" ""))
      (.append mermaid-code f"  year{year} -.-> {paper-id}\n")))
  
  ;; Close diagram
  (.append mermaid-code "```\n")
  
  ;; Write to file
  (setv output-file (os.path.join OUTPUT-DIR "papers_authors.md"))
  (with [f (open output-file "w")]
    (f.write (.join "" mermaid-code)))
  
  (print f"Generated author network diagram: {output-file}")
  mermaid-code)

;; Generate an org-mode table of papers
(defn generate-org-table [catalog]
  """Generate an org-mode table of papers."""
  (setv org-content "#+TITLE: AI Research Papers Catalog\n")
  (.append org-content "#+DATE: " (.strftime (datetime.now) "%Y-%m-%d") "\n\n")
  (.append org-content "* Papers Catalog\n\n")
  (.append org-content "| Filename | Author | Year | Topic | Source | Reference |\n")
  (.append org-content "|----------+--------+------+-------+--------+----------|\n")
  
  (for [[filename paper] (sorted (.items catalog))]
    (setv author (get paper "author" "unknown"))
    (setv year (get paper "year" "unknown"))
    (setv topic (.replace (get paper "topic" "unknown") "-" " "))
    (setv source (get paper "source" "unknown"))
    (setv reference (get paper "reference" ""))
    (.append org-content f"| {filename} | {author} | {year} | {topic} | {source} | {reference} |\n"))
  
  ;; Write to file
  (setv output-file (os.path.join OUTPUT-DIR "papers_catalog.org"))
  (with [f (open output-file "w")]
    (f.write (.join "" org-content)))
  
  (print f"Generated org-mode table: {output-file}")
  org-content)

;; Main function
(defmain [&rest args]
  (print "AI Research Papers Visualizer")
  (print f"Papers directory: {PAPERS-DIR}")
  (print f"Output directory: {OUTPUT-DIR}")
  
  ;; Load catalog
  (setv catalog (load-catalog))
  
  (if (not catalog)
      (print "Error: No papers found in catalog.")
      (do
        ;; Generate all visualizations
        (generate-timeline-diagram catalog)
        (generate-topic-diagram catalog)
        (generate-author-network catalog)
        (generate-org-table catalog)
        (print "Visualization complete. Files saved to output directory."))))
