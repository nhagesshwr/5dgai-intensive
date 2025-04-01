#!/usr/bin/env hy
;; paper_downloader.hy - Download AI research papers with citation tracking
;; Usage: poetry run hy paper_downloader.hy

(import os)
(import sys)
(import json)
(import [datetime [datetime]])
(import [urllib.parse [urlparse]])
(import requests)
(import re)

;; Configuration
(setv PROJECT-ROOT (os.path.abspath (os.path.expanduser ".")))
(setv PAPERS-DIR (os.path.join PROJECT-ROOT "papers"))
(setv WHITEPAPERS-DIR (os.path.join PROJECT-ROOT "whitepapers"))
(setv BIB-FILE (os.path.join PROJECT-ROOT "bib.json"))

;; Ensure directories exist
(os.makedirs PAPERS-DIR :exist-ok True)
(os.makedirs WHITEPAPERS-DIR :exist-ok True)

;; Paper metadata extraction patterns
(setv FILENAME-PATTERN (re.compile r"(?P<author>[a-zA-Z-]+)-(?P<year>\d{4})-(?P<topic>[\w-]+)\.pdf"))

;; Sources and their URL patterns
(setv SOURCES {"arxiv" "https://arxiv.org/pdf/{id}.pdf"
               "openreview" "https://openreview.net/pdf?id={id}"
               "direct" "{url}"})

;; Load bibliography data
(defn load-bibliography []
  """Load the existing bibliography if it exists."""
  (try
    (do
      (when (os.path.exists BIB-FILE)
        (with [f (open BIB-FILE "r")]
          (setv bib (json.load f))
          (print f"Loaded bibliography with {(len bib)} entries")
          bib))
        {"references" []})  ; Return empty bib if file doesn't exist
    (except [e json.JSONDecodeError]
      (print f"Warning: Could not parse bibliography file. Starting fresh.")
      {"references" []})))

;; Save bibliography data
(defn save-bibliography [bib]
  """Save the bibliography to disk."""
  (with [f (open BIB-FILE "w")]
    (json.dump bib f :indent 2 :sort-keys True))
  (print f"Saved bibliography with {(len (get bib 'references'))} entries to {BIB-FILE}"))

;; Add entry to bibliography
(defn add-bib-entry [paper-data]
  """Add a paper to the bibliography."""
  (setv bib (load-bibliography))
  (setv references (get bib "references"))
  
  ;; Check if paper already exists in bibliography
  (setv paper-id (get paper-data "id"))
  (setv existing (lfor ref references :if (= (get ref "id") paper-id) ref))
  
  (if existing
      (do
        (print f"Paper {paper-id} already exists in bibliography")
        (first existing))
      (do
        (print f"Adding {paper-id} to bibliography")
        (.append references paper-data)
        (save-bibliography bib)
        paper-data)))

;; Format paper data for bibliography
(defn format-bib-entry [filename author year title source url paper-id reference citations]
  """Format paper data for bibliography."""
  (setv entry {
    "id" (if paper-id paper-id filename)
    "type" "article"
    "title" title
    "author" [{"name" author}]
    "year" (int year)
    "source" source
    "url" url
    "filename" filename
    "reference" reference
    "citations" (if citations citations [])
  })
  
  entry)

;; Download a paper and update bibliography
(defn download-paper [url filename title author year source paper-id reference citations]
  """
  Download a paper and update bibliography.
  """
  ;; Full path to the output file
  (setv output-path (os.path.join PAPERS-DIR filename))
  
  ;; Check if file already exists
  (when (os.path.exists output-path)
    (print f"SKIP: {filename} (already exists)")
    ;; Still update bibliography
    (add-bib-entry (format-bib-entry filename author year title source url paper-id reference citations))
    (return output-path))
  
  ;; Download the paper
  (print f"DOWNLOAD: {filename} from {url}")
  
  (try
    (do
      ;; Make the request
      (setv response (requests.get url :stream True))
      (response.raise-for-status)
      
      ;; Save the PDF
      (with [f (open output-path "wb")]
        (for [chunk (response.iter-content :chunk-size 8192)]
          (f.write chunk)))
      
      ;; Add to bibliography
      (add-bib-entry (format-bib-entry filename author year title source url paper-id reference citations))
      
      (print f"SUCCESS: {filename}")
      output-path)
    (except [e Exception]
      (print f"FAILED: {filename} - {(str e)}")
      None)))

;; Download papers from batch JSON
(defn batch-download [batch-file]
  """Download papers from a batch JSON file."""
  (try
    (do
      (with [f (open batch-file "r")]
        (setv papers (json.load f))
        (print f"Loaded {(len papers)} papers from {batch-file}")
        
        (setv success-count 0)
        
        (for [paper papers]
          (setv url (cond
                     [(= (get paper "source") "arxiv") 
                      (.format (get SOURCES "arxiv") :id (get paper "paper-id"))]
                     [(= (get paper "source") "openreview") 
                      (.format (get SOURCES "openreview") :id (get paper "paper-id"))]
                     [True (get paper "url")]))
          
          ;; Extract metadata
          (setv filename (get paper "filename"))
          (setv title (get paper "title" filename))
          (setv author (get paper "author" (first (.split filename "-"))))
          (setv year (get paper "year" (second (.split filename "-"))))
          (setv source (get paper "source" "direct"))
          (setv paper-id (get paper "paper-id" filename))
          (setv reference (get paper "reference" ""))
          (setv citations (get paper "citations" []))
          
          (when (download-paper url filename title author year source paper-id reference citations)
            (setv success-count (+ success-count 1))))
        
        (print f"Successfully downloaded {success-count} out of {(len papers)} papers")
        success-count))
    (except [e Exception]
      (print f"Error processing batch file: {(str e)}")
      0)))

;; Scan for citations between papers
(defn scan-for-citations []
  """Scan bibliography for papers that cite each other."""
  (setv bib (load-bibliography))
  (setv references (get bib "references"))
  
  ;; Create a map of paper IDs to titles for quick lookup
  (setv paper-map {})
  (for [ref references]
    (setv (get paper-map (get ref "id")) (get ref "title")))
  
  ;; For each paper, scan its reference field for mentions of other papers
  (for [paper references]
    (setv ref-text (get paper "reference" ""))
    (setv paper-citations [])
    
    (for [[paper-id title] (.items paper-map)]
      (when (and (not (= paper-id (get paper "id")))
                 (or (in paper-id ref-text) 
                     (in title ref-text)))
        (.append paper-citations paper-id)))
    
    (when paper-citations
      (print f"Paper {(get paper 'id')} cites: {paper-citations}")
      (setv (get paper "citations") paper-citations)))
  
  ;; Save updated bibliography
  (save-bibliography bib))

;; Generate references Mermaid diagram
(defn generate-citation-graph []
  """Generate a Mermaid diagram of paper citations."""
  (setv bib (load-bibliography))
  (setv references (get bib "references"))
  
  (setv mermaid-code "```mermaid\ngraph TD\n")
  
  ;; Add nodes for each paper
  (for [paper references]
    (setv paper-id (.replace (get paper "id") "-" "_"))
    (setv title (get paper "title"))
    (.append mermaid-code f"  {paper-id}[\"{title}\"]\n"))
  
  ;; Add edges for citations
  (for [paper references]
    (setv paper-id (.replace (get paper "id") "-" "_"))
    (for [citation (get paper "citations" [])]
      (setv citation-id (.replace citation "-" "_"))
      (.append mermaid-code f"  {paper-id} --> {citation-id}\n")))
  
  ;; Close diagram
  (.append mermaid-code "```\n")
  
  ;; Write to file
  (setv output-file (os.path.join PROJECT-ROOT "citation_graph.md"))
  (with [f (open output-file "w")]
    (f.write (.join "" mermaid-code)))
  
  (print f"Generated citation graph: {output-file}")
  mermaid-code)

;; Main function
(defmain [&rest args]
  (setv batch-file (if (>= (len args) 2)
                      (get args 1)
                      "papers_batch.json"))
  
  (print "AI Research Paper Downloader")
  (print f"Papers directory: {PAPERS-DIR}")
  (print f"Bibliography file: {BIB-FILE}")
  
  (cond
    ;; Scan for citations
    [(in "--scan-citations" args)
     (print "Scanning for citations between papers...")
     (scan-for-citations)]
    
    ;; Generate citation graph
    [(in "--graph" args)
     (print "Generating citation graph...")
     (generate-citation-graph)]
    
    ;; Download from batch file
    [(os.path.exists batch-file)
     (print f"Downloading papers from batch file: {batch-file}")
     (batch-download batch-file)]
    
    ;; No valid action
    [True
     (print "Usage:")
     (print "  poetry run hy paper_downloader.hy [BATCH_FILE]")
     (print "  poetry run hy paper_downloader.hy --scan-citations")
     (print "  poetry run hy paper_downloader.hy --graph")
     (print "")
     (print "If BATCH_FILE is not specified, defaults to papers_batch.json")
     (print "")
     (print "The batch file should be a JSON file with an array of paper objects:")
     (print "  [{")
     (print "    \"source\": \"arxiv\",")
     (print "    \"paper-id\": \"2109.01652\",") 
     (print "    \"filename\": \"wei-2023-zero-shot.pdf\",")
     (print "    \"title\": \"Finetuned Language Models Are Zero-Shot Learners\",")
     (print "    \"author\": \"Wei\",")
     (print "    \"year\": \"2023\",")
     (print "    \"reference\": \"Wei et al. 'Finetuned Language Models Are Zero-Shot Learners'\",")
     (print "    \"citations\": [\"brown-2023-few-shot\", \"kaplan-2020-scaling-laws\"]")
     (print "  }, ...]")])))
