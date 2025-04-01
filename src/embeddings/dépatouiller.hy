#!/usr/bin/env hy

(import os)
(import json)
(import math)

(print "🇫🇷 French Verb Embedding Contrast 🇫🇷")
(print "======================================")

;; Two contrasting verbs
(print "dépatouiller: to untangle/sort out a difficult situation")
(print "flâner: to stroll/wander leisurely")
(print)
(print "What we do in code vs. what we wish we were doing...")

;; Create our very serious embeddings
(setv depatouiller-emb [0.42 0.314 0.1337 0.007])  ;; Stressed, complex
(setv flaner-emb [0.1 0.75 0.2 0.8])              ;; Relaxed, pleasant

;; Calculate the cosine similarity (just for fun)
(defn dot-product [a b]
  (sum (map (fn [x y] (* x y)) a b)))

(defn magnitude [v]
  (math.sqrt (sum (map (fn [x] (* x x)) v))))

(defn cosine-similarity [a b]
  (/ (dot-product a b) (* (magnitude a) (magnitude b))))

(setv similarity (cosine-similarity depatouiller-emb flaner-emb))
(print "\nSimilarity between debugging and strolling:" (float similarity))
(print "(Unsurprisingly, they're quite different activities!)")

;; Save to files
(os.makedirs "data/embeddings" :exist_ok True)
(with [f (open "data/embeddings/debugging_vs_strolling.json" "w")]
  (.write f (json.dumps {"dépatouiller" depatouiller-emb
                        "flâner" flaner-emb
                        "similarity" similarity} :indent 2)))

(print "\n✅ Successfully embedded both verbs")
(print "While you're dépatouiller-ing your code problems,")
(print "dream of flâner-ing through a Parisian boulevard instead!")