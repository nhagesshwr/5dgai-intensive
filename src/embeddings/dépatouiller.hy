#!/usr/bin/env hy

(import os)
(import json)

(print "🇫🇷 Dépatouiller: to untangle/sort out a difficult situation 🇫🇷")
(print "The perfect verb for debugging embedding issues!")

;; Create our very serious embedding for dépatouiller
(setv embedding [0.42 0.314 0.1337 0.007])

;; Save it to a file with much fanfare
(os.makedirs "data/embeddings" :exist_ok True)
(with [f (open "data/embeddings/dépatouiller.json" "w")]
  (.write f (json.dumps {"dépatouiller" embedding} :indent 2)))

(print "\n✅ Successfully embedded 'dépatouiller'")
(print "Now you can dépatouiller all your embedding problems!")