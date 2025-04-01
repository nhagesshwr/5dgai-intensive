#!/usr/bin/env hy
"\"
20 Newsgroups Dataset Loader

This script provides utilities for loading the 20 Newsgroups dataset
using scikit-learn, with a Hy-native interface.

Part of 5-Day Gen AI Intensive Course
\""

(require [hy.pyops [*]])
(import argparse)
(import json)
(import os)
(import sys)
(import [sklearn.datasets [fetch_20newsgroups load_files]])
(import [sklearn.utils [Bunch]])
(import hashlib)

(defn calculate-hash [data]
  "\"
  Calculate a hash of the input data for fingerprinting.
  
  Args:
      data: String data to hash
      
  Returns:
      Hash string
  \""
  (setv hash-obj (hashlib.md5))
  (.update hash-obj (.encode (str data) "utf-8"))
  (.hexdigest hash-obj))

(defn fetch-newsgroups [&optional 
                       [subset "all"] 
                       [categories None] 
                       [shuffle True] 
                       [random-state 42] 
                       [remove ["headers" "footers" "quotes"]]
                       [output-dir None]]
  "\"
  Fetch the 20 newsgroups dataset with optional caching to disk.
  
  Args:
      subset: 'train', 'test', or 'all' (default: 'all')
      categories: List of categories to load (default: all categories)
      shuffle: Whether to shuffle the data (default: True)
      random-state: Random state for reproducibility (default: 42)
      remove: List of text parts to remove (default: ['headers', 'footers', 'quotes'])
      output-dir: Directory to save the dataset (default: None)
      
  Returns:
      Dataset object with data, target, and metadata
  \""
  ;; Determine cache path if output_dir is provided
  (setv cache-path None)
  (when output-dir
    (os.makedirs output-dir :exist_ok True)
    
    ;; Create a hash of the parameters for the cache filename
    (setv param-hash (calculate-hash 
                     (str {"subset" subset
                          "categories" categories
                          "shuffle" shuffle
                          "random_state" random-state
                          "remove" remove})))
    
    (setv cache-path (os.path.join output-dir f"newsgroups_{subset}_{param-hash}.json")))
  
  ;; Check if cached version exists
  (when (and cache-path (os.path.exists cache-path))
    (print f"Loading cached dataset from {cache-path}")
    (with [f (open cache-path "r")]
      (setv data (json.load f))
      (setv dataset (Bunch))
      (setv dataset.data (get data "data"))
      (setv dataset.target (get data "target"))
      (setv dataset.target_names (get data "target_names"))
      (setv dataset.filenames (get data "filenames" []))
      (setv dataset.DESCR (get data "DESCR" ""))
      (return dataset)))
  
  ;; If not cached or cache not requested, fetch from sklearn
  (print f"Fetching 20 Newsgroups dataset (subset: {subset})")
  (setv dataset (fetch_20newsgroups 
                :subset subset
                :categories categories
                :shuffle shuffle
                :random_state random-state
                :remove remove))
  
  ;; Cache to disk if requested
  (when cache-path
    (print f"Caching dataset to {cache-path}")
    (setv data {"data" dataset.data
               "target" (lfor t dataset.target (int t))  ;; Convert numpy ints to Python ints
               "target_names" dataset.target_names
               "filenames" (if (hasattr dataset "filenames") dataset.filenames [])
               "DESCR" (if (hasattr dataset "DESCR") dataset.DESCR "")})
    (with [f (open cache-path "w")]
      (json.dump data f :ensure_ascii False)))
  
  dataset)

(defn get-category-data [dataset category-name]
  "\"
  Get data for a specific category from the dataset.
  
  Args:
      dataset: Dataset object returned by fetch-newsgroups
      category-name: Name of the category to extract
      
  Returns:
      List of documents in the requested category
  \""
  (when (not (in category-name dataset.target_names))
    (raise (ValueError f"Category '{category-name}' not found in dataset")))
  
  (setv category-idx (.index dataset.target_names category-name))
  (lfor [i doc] (enumerate dataset.data)
        :if (= (get dataset.target i) category-idx)
        doc))

(defn save-category-texts [dataset category-name output-path]
  "\"
  Save all texts from a specific category to a text file.
  
  Args:
      dataset: Dataset object returned by fetch-newsgroups
      category-name: Name of the category to extract
      output-path: Path to save the texts
      
  Returns:
      Number of documents saved
  \""
  (setv texts (get-category-data dataset category-name))
  (with [f (open output-path "w" :encoding "utf-8")]
    (for [text texts]
      (.write f f"{text}\n\n---\n\n")))
  (len texts))

;; Parser for command-line arguments
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser 
                 :description "Load and process the 20 Newsgroups dataset")]
    
    (.add_argument parser "--subset" :default "all" :choices ["train" "test" "all"]
                  :help "Dataset subset to load (train, test, or all)")
    
    (.add_argument parser "--categories" :nargs "*" :default None
                  :help "List of categories to load")
    
    (.add_argument parser "--output-dir" :default None
                  :help "Directory to save the cached dataset")
    
    (.add_argument parser "--list-categories" :action "store_true"
                  :help "List all available categories and exit")
    
    (.add_argument parser "--extract-category" :default None
                  :help "Extract texts from a specific category")
    
    (.add_argument parser "--output" :default None
                  :help "Output file path for extracted texts")
    
    (.parse_args parser)))

;; Main function
(defn main []
  "\"Main entry point for command line use.\""
  (let [args (parse-args)]
    
    ;; List categories and exit if requested
    (when args.list_categories
      (setv categories (. (fetch_20newsgroups :subset "train") target_names))
      (print "Available categories in 20 Newsgroups dataset:")
      (for [[i category] (enumerate categories)]
        (print f"  {i}. {category}"))
      (return 0))
    
    ;; Load the dataset
    (setv dataset (fetch-newsgroups 
                  :subset args.subset
                  :categories args.categories
                  :output-dir args.output_dir))
    
    ;; Print basic info
    (print f"Loaded {(len dataset.data)} documents from {(len dataset.target_names)} categories")
    (print "Categories:")
    (for [category dataset.target_names]
      (print f"  - {category}"))
    
    ;; Extract category if requested
    (when args.extract_category
      (if args.output
        (do
          (setv count (save-category-texts dataset args.extract_category args.output))
          (print f"Saved {count} documents from category '{args.extract_category}' to {args.output}"))
        (do
          (print f"Error: --output must be specified when using --extract-category")
          (return 1))))
    
    0))

;; Standard entry point
(when (= __name__ "__main__")
  (sys.exit (main)))