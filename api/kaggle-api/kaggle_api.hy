#!/usr/bin/env hy

(import json)
(import os)
(import sys)
(import [kaggle [KaggleApi]])
(import [pandas :as pd])
(import [pathlib [Path]])

(defn init-api []
  "Initialize and authenticate the Kaggle API"
  (let [api (KaggleApi)]
    (.authenticate api)
    api))

(defn get-competitions [api &optional [search None] [category "all"] [sort-by "latestDeadline"]]
  "List available competitions"
  (let [comps (.competitions_list api 
                                  :search search
                                  :category category 
                                  :sort_by sort-by)]
    (pd.DataFrame comps)))

(defn get-competition-files [api competition]
  "List files for a specific competition"
  (let [files (.competition_list_files api competition)]
    (pd.DataFrame files)))

(defn download-competition-files [api competition &optional [file-name None] [path "./"]]
  "Download competition files"
  (.competition_download_files api competition 
                              :file_name file-name 
                              :path path))

(defn get-datasets [api &optional [search None] [sort-by "hottest"]]
  "List available datasets"
  (let [datasets (.dataset_list api 
                              :search search
                              :sort_by sort-by)]
    (pd.DataFrame datasets)))

(defn get-dataset-files [api dataset]
  "List files for a specific dataset"
  (let [files (.dataset_list_files api dataset)]
    (pd.DataFrame files)))

(defn download-dataset [api dataset &optional [file-name None] [path "./"] [unzip False]]
  "Download dataset files"
  (.dataset_download_files api dataset 
                         :file_name file-name 
                         :path path
                         :unzip unzip))

(defn get-kernels [api &optional [search None] [language "all"] [kernel-type "all"]]
  "List available kernels"
  (let [kernels (.kernels_list api 
                             :search search 
                             :language language
                             :kernel_type kernel-type)]
    (pd.DataFrame kernels)))

(defn pull-kernel [api kernel &optional [path "./"] [metadata False]]
  "Pull down a kernel"
  (.kernels_pull api kernel 
                :path path 
                :metadata metadata))

(defn get-models [api &optional [search None] [sort-by "hotness"]]
  "List available models"
  (let [models (.model_list api 
                          :search search 
                          :sort_by sort-by)]
    (pd.DataFrame models)))

(defmain [&rest args]
  "CLI entry point"
  (let [api (init-api)]
    (if (>= (len args) 2)
      (let [command (get args 1)]
        (cond 
          [(= command "competitions") 
           (print (get-competitions api))]
          [(= command "datasets") 
           (print (get-datasets api))]
          [(= command "kernels") 
           (print (get-kernels api))]
          [(= command "models") 
           (print (get-models api))]
          [True (print "Unknown command. Use: competitions, datasets, kernels, or models")]))
      (print "Usage: hy kaggle_api.hy [command]"))))
