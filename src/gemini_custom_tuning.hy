#!/usr/bin/env hy
;; Gemini Custom Model Tuning Example in Hy
;; Demonstrates how to fine-tune a Gemini model for a custom task

(import os)
(import json)
(import [dotenv [load-dotenv]])
(import [google.generativeai [configure Client]])
(import [google.generativeai.types [Model CreateTuningJobRequest TuningExample TuningHyperparameters]])
(import [sklearn.model-selection [train-test-split]])
(import [sklearn.datasets [fetch-20newsgroups]])
(import [tqdm [tqdm]])

(defn setup-client []
  "Setup and configure the Gemini client"
  ;; Load environment variables
  (load-dotenv)
  (setv api-key (os.getenv "AI_STUDIO_API_KEY"))
  (configure :api-key api-key)
  (Client))

(defn load-newsgroups-data []
  "Load the 20 Newsgroups dataset for text classification"
  (print "Loading 20 Newsgroups dataset...")
  
  ;; Choose a subset of categories for simplicity
  (setv categories ["comp.graphics" "sci.med" "rec.autos" "talk.politics.guns"])
  
  ;; Load training data
  (setv newsgroups-train (fetch-20newsgroups 
                           :subset "train" 
                           :categories categories
                           :remove ["headers" "footers" "quotes"]))
  
  ;; Load test data
  (setv newsgroups-test (fetch-20newsgroups 
                          :subset "test" 
                          :categories categories
                          :remove ["headers" "footers" "quotes"]))
  
  (print f"Loaded {(len newsgroups-train.data)} training samples and {(len newsgroups-test.data)} test samples")
  (print f"Categories: {newsgroups-train.target-names}")
  
  {"train" {"data" newsgroups-train.data 
            "target" newsgroups-train.target
            "target_names" newsgroups-train.target-names}
   "test" {"data" newsgroups-test.data 
           "target" newsgroups-test.target
           "target_names" newsgroups-test.target-names}})

(defn format-example [text category category-names]
  "Format a single example for tuning"
  (setv category-name (get category-names category))
  {"input-text" text
   "output-category" category-name})

(defn prepare-tuning-data [dataset &optional [max-examples 200]]
  "Prepare the tuning data in the format required by Gemini"
  (print "Preparing tuning data...")
  
  ;; Get the data
  (setv train-data (get dataset "train"))
  (setv texts (get train-data "data"))
  (setv targets (get train-data "target"))
  (setv target-names (get train-data "target_names"))
  
  ;; Limit the number of examples if needed
  (setv n-examples (min (len texts) max-examples))
  (setv texts (cut texts 0 n-examples))
  (setv targets (cut targets 0 n-examples))
  
  ;; Create tuning examples
  (setv tuning-examples [])
  (for [i (range (len texts))]
    (.append tuning-examples (format-example (get texts i) 
                                            (get targets i) 
                                            target-names)))
  
  ;; Split into training and validation sets
  (setv [train-set val-set] (train-test-split tuning-examples 
                                             :test-size 0.2 
                                             :random-state 42))
  
  ;; Format for the Gemini API
  (setv formatted-train-examples 
        (list (map (fn [ex] {"content" (+ "Classify this text into one of these categories: "
                                         (.join ", " target-names)
                                         "\n\nText: " (get ex "input-text"))
                            "response" (get ex "output-category")})
                  train-set)))
  
  (setv formatted-val-examples 
        (list (map (fn [ex] {"content" (+ "Classify this text into one of these categories: "
                                        (.join ", " target-names)
                                        "\n\nText: " (get ex "input-text"))
                           "response" (get ex "output-category")})
                 val-set)))
  
  (print f"Prepared {(len formatted-train-examples)} training examples and {(len formatted-val-examples)} validation examples")
  
  {"train_examples" formatted-train-examples
   "validation_examples" formatted-val-examples})

(defn create-tuning-job [client display-name tuning-data]
  "Create a tuning job for a custom model"
  (print f"Creating tuning job: {display-name}")
  
  ;; Convert examples to proper format
  (setv training-examples 
        (list (map (fn [ex] (TuningExample 
                              :content (get ex "content")
                              :response (get ex "response")))
                  (get tuning-data "train_examples"))))
  
  (setv validation-examples 
        (list (map (fn [ex] (TuningExample 
                              :content (get ex "content")
                              :response (get ex "response")))
                  (get tuning-data "validation_examples"))))
  
  ;; Set hyperparameters
  (setv hyperparameters (TuningHyperparameters 
                          :batch_size 8 
                          :learning_rate 0.001 
                          :epoch_count 3))
  
  ;; Create the tuning request
  (setv request (CreateTuningJobRequest 
                  :base_model "gemini-2.0-flash" 
                  :display_name display-name
                  :training_examples training-examples
                  :validation_examples validation-examples
                  :hyperparameters hyperparameters))
  
  ;; Submit the tuning job
  (try
    (do
      (setv tuning-job (client.tuning-jobs.create request))
      (print f"Tuning job created with ID: {tuning-job.id}")
      tuning-job)
    (except [e Exception]
      (print f"Error creating tuning job: {e}")
      None)))

(defn check-tuning-job [client job-id]
  "Check the status of a tuning job"
  (try
    (do
      (setv job (client.tuning-jobs.get :name job-id))
      (print f"Job ID: {job.id}")
      (print f"Status: {job.state}")
      (print f"Created: {job.create-time}")
      (print f"Updated: {job.update-time}")
      (when (hasattr job "tuned-model")
        (print f"Tuned model: {job.tuned-model}"))
      job)
    (except [e Exception]
      (print f"Error checking tuning job: {e}")
      None)))

(defn test-tuned-model [client model-id test-data &optional [num-examples 10]]
  "Test the tuned model on example data"
  (print f"Testing tuned model: {model-id}")
  
  ;; Get test data
  (setv texts (get (get test-data "test") "data"))
  (setv targets (get (get test-data "test") "target"))
  (setv target-names (get (get test-data "test") "target_names"))
  
  ;; Limit number of examples for testing
  (setv num-examples (min (len texts) num-examples))
  
  ;; Run inference on test examples
  (setv correct 0)
  (for [i (range num-examples)]
    (setv text (get texts i))
    (setv true-category (get target-names (get targets i)))
    
    (setv prompt (+ "Classify this text into one of these categories: "
                   (.join ", " target-names)
                   "\n\nText: " text))
    
    (try
      (do
        (setv response (client.models.generate-content 
                         :model model-id
                         :contents prompt))
        (setv predicted-category (get (. (. response candidates [0]) content) parts [0] text))
        
        ;; Clean up prediction
        (setv predicted-category (.strip predicted-category))
        
        (print f"Example {(+ i 1)}:")
        (print f"Text: {(cut text 0 100)}...")
        (print f"True category: {true-category}")
        (print f"Predicted: {predicted-category}")
        
        ;; Check if prediction is correct (with some flexibility)
        (when (in true-category predicted-category)
          (+= correct 1))
        
        (print "---\n"))
      (except [e Exception]
        (print f"Error in prediction: {e}"))))
  
  ;; Calculate accuracy
  (setv accuracy (/ correct num-examples))
  (print f"Test accuracy: {(* accuracy 100):.2f}% ({correct}/{num-examples})")
  
  accuracy)

(defn save-tuning-config [config filename]
  "Save tuning configuration to a file"
  (with [f (open filename "w")]
    (json.dump config f :indent 2)))

(defn main []
  "Main function to demonstrate Gemini model tuning"
  (print "Setting up Gemini client...")
  (setv client (setup-client))
  
  ;; Check if we're running in a testing/demo mode
  (setv demo-mode True)
  
  (if demo-mode
      ;; In demo mode, just show process without creating actual tuning job
      (do
        (print "Running in demo mode (no actual tuning job will be created)")
        (setv dataset (load-newsgroups-data))
        (setv tuning-data (prepare-tuning-data dataset :max-examples 20))
        
        ;; Save example configuration
        (save-tuning-config {"train_examples" (cut (get tuning-data "train_examples") 0 2)
                            "validation_examples" (cut (get tuning-data "validation_examples") 0 2)}
                           "tuning_config_example.json")
        
        (print "\nTuning job would be created with these parameters:")
        (print "- Base model: gemini-2.0-flash")
        (print "- Display name: newsgroups-classifier")
        (print "- Training examples: " (len (get tuning-data "train_examples")))
        (print "- Validation examples: " (len (get tuning-data "validation_examples")))
        (print "- Hyperparameters: batch_size=8, learning_rate=0.001, epoch_count=3")
        
        (print "\nExample tuning data saved to tuning_config_example.json"))
      
      ;; In real mode, create actual tuning job (requires API permissions)
      (do
        (print "Creating actual tuning job (requires appropriate API permissions)")
        (setv dataset (load-newsgroups-data))
        (setv tuning-data (prepare-tuning-data dataset :max-examples 100))
        
        ;; Create the tuning job
        (setv job (create-tuning-job client "newsgroups-classifier" tuning-data))
        
        (when job
          ;; Save configuration
          (save-tuning-config {"job_id" job.id
                              "train_examples" (cut (get tuning-data "train_examples") 0 2)
                              "validation_examples" (cut (get tuning-data "validation_examples") 0 2)}
                             "tuning_job_config.json")
          
          (print "\nTuning job created and saved to tuning_job_config.json")
          (print "Check status with: check-tuning-job client job.id")))))

(when (= __name__ "__main__")
  (main))