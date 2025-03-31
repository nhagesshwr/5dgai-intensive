#!/usr/bin/env hy
;; Tests for the livestream transcriber in Hy

(import os)
(import unittest)
(import os.path)
(import tempfile)
(import src.livestream_transcriber)

(setv exists os.path.exists)
(setv join os.path.join)
(setv dirname os.path.dirname)

(defn test-paths []
  "Test that the expected file paths can be found"
  ;; Check if the Hy source exists
  (setv src-path (join (dirname (dirname (dirname __file__))) "src" "livestream_transcriber.hy"))
  (assert (exists src-path) "livestream_transcriber.hy should exist")
  
  (print "All path tests passed!"))

(defn test-argparse []
  "Test argument parsing"
  (setv parser (src.livestream_transcriber.setup-argparse))
  (assert parser "Argument parser should be created")
  
  (print "Argument parser test passed!"))

(defn main []
  (test-paths)
  (test-argparse)
  (print "All tests passed!"))

(when (= __name__ "__main__")
  (main))