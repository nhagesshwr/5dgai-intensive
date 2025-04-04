#\!/usr/bin/env hy

;; Import the scaling functionality
(import [verb_scaling [load-verbs load-full-verb-list run-scaling-test]])
(import os)

(defn main []
  (print "🔎 Scaling French Verb Embeddings to 100 verbs")
  
  ;; Setup paths
  (setv repo-root (os.path.dirname (os.path.dirname (os.path.dirname __file__))))
  (setv full-verbs-path (os.path.join repo-root "resources" "verbs" "french_verbs_list.txt"))
  (setv output-dir (os.path.join repo-root "data" "embeddings"))
  (os.makedirs output-dir :exist-ok True)
  
  ;; Load verbs
  (setv verbs (load-full-verb-list full-verbs-path 100))
  (print f"Loaded {(len verbs)} verbs from the full verb list")
  
  ;; Process with a batch size of 10
  (setv batch-size 10)
  (setv max-retries 3)
  (print f"Processing with batch size {batch-size} and {max-retries} retries")
  
  ;; Run the scaling test
  (setv results (run-scaling-test verbs batch-size max-retries output-dir))
  
  (print "\n✅ Processing complete\!")
  (print f"Processed {(len verbs)} verbs"))

(when (= __name__ "__main__")
  (main))
