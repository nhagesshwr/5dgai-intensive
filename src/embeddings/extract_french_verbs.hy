#!/usr/bin/env hy

(import xml.etree.ElementTree :as ET)
(import os.path)
(import sys)
(import random)

(defn extract-verbs [xml-path &optional [limit 700]]
  "Extract French verbs from the verbiste XML file."
  (when (not (os.path.exists xml-path))
    (print f"Error: File {xml-path} not found.")
    (sys.exit 1))
  
  ;; Essential verbs that we want to confirm
  (setv essential-verbs ["être" "avoir" "aller" "parler" "faire" 
                        "prendre" "vouloir" "savoir" "pouvoir" "venir" 
                        "dire" "donner" "penser" "aider" "aimer" 
                        "devoir" "habiter" "regarder" "utiliser" "essayer"])
  
  ;; Parse the XML file
  (try
    (setv tree (ET.parse xml-path))
    (setv root (tree.getroot))
    (print f"Root tag: {root.tag}")
    (print f"Number of verb entries: {(len root)}")
    (except [e Exception]
      (print f"Error parsing XML: {e}")
      (sys.exit 1)))
  
  ;; Extract all verbs
  (setv all-verbs [])
  (for [v root]
    ;; Find the 'i' element (infinitive) within each 'v' element
    (setv i-element (v.find "i"))
    (when i-element
      (setv infinitive (i-element.text))
      (when infinitive
        (.append all-verbs (.lower infinitive)))))
  
  (print f"Extracted {(len all-verbs)} verbs from XML")
  
  ;; Check for essential verbs
  (setv essential-found [])
  (setv essential-missing [])
  
  (for [verb essential-verbs]
    (if (in verb all-verbs)
        (.append essential-found verb)
        (.append essential-missing verb)))
  
  ;; Report on essential verbs
  (print f"Found {(len essential-found)} of {(len essential-verbs)} essential verbs")
  
  (when essential-missing
    (print "Missing essential verbs:")
    (for [verb essential-missing]
      (print f"  ✗ {verb}")))
  
  ;; If the limit is less than the total, sample randomly but include essential verbs
  (if (< limit (len all-verbs))
      (do
        (setv non-essential-verbs (list (filter (fn [v] (not (in v essential-found))) all-verbs)))
        (setv sample-size (min (- limit (len essential-found)) (len non-essential-verbs)))
        (setv sampled-verbs (+ essential-found (random.sample non-essential-verbs sample-size)))
        (print f"Sampled {(len sampled-verbs)} verbs (including {(len essential-found)} essential verbs)")
        sampled-verbs)
      all-verbs))

(defn main []
  (setv xml-path "/usr/local/share/verbiste-0.1/verbs-fr.xml")
  (setv all-verbs (extract-verbs xml-path 700))
  
  ;; Save all sampled verbs
  (with [f (open "/home/jwalsh/projects/aygp-dr/5dgai-intensive/src/embeddings/french_verbs.txt" "w")]
    (for [verb all-verbs]
      (f.write f"{verb}\n")))
  
  ;; Save essential verbs for embedding testing (just the top 20)
  (setv essential-verbs (cut all-verbs 0 20))
  (with [f (open "/home/jwalsh/projects/aygp-dr/5dgai-intensive/src/embeddings/test_french_verbs.txt" "w")]
    (for [verb essential-verbs]
      (f.write f"{verb}\n"))))

(when (= __name__ "__main__")
  (main))