# CLI Design Principles for 5D-GAI Intensive

## Core Philosophy

Our command-line tools follow the Unix philosophy and KISS principle:

1. **Single Responsibility**: Each tool does one thing and does it well
2. **Clear Mandate**: Every tool has a specific, well-defined purpose
3. **Composability**: Tools can be combined through pipes and redirects
4. **File-Based I/O**: Input comes from files or stdin, output goes to files or stdout

## Standard CLI Design Pattern

Every Hy-based CLI tool in this repository follows a consistent pattern:

### 1. Clear Separation of Concerns

- Core functionality is separated from CLI handling
- Functions are designed to be reusable by other scripts
- No side effects in core functions (e.g., no printing to stdout)

### 2. Standardized Structure

```hy
#!/usr/bin/env hy
"\"
Brief description of what the tool does

Longer description with details about functionality.

Part of 5-Day Gen AI Intensive
\""

;; Imports
(require [hy.pyops [*]])
(import argparse)
(import json)
(import sys)
(import [other.modules [as needed]])

;; Core functionality separated from CLI
(defn core-function [param1 param2 &optional [opt-param None]]
  "\"
  Do the main work.
  
  Args:
      param1: Description
      param2: Description
      opt-param: Description
      
  Returns:
      Description of return value
  \""
  ;; Implementation
  (do-work param1 param2 opt-param))

;; CLI argument parsing
(defn parse-args []
  "\"Parse command line arguments.\""
  (let [parser (argparse.ArgumentParser :description "Tool description")]
    ;; Add arguments
    (.add_argument parser "--required-arg" :required True
                  :help "Description of required argument")
    
    (.add_argument parser "--optional-arg" :default default-value
                  :help "Description of optional argument")
    
    (.add_argument parser "--output" :dest "output_path" :default None
                  :help "Path to save results as JSON")
    
    (.parse_args parser)))

;; Main function as entry point
(defn main []
  "\"Main entry point for command line use.\""
  (let [args (parse-args)]
    ;; Call core functions with args
    (let [result (core-function args.required_arg args.optional_arg)]
      
      ;; Save to file if requested
      (when (and result args.output_path)
        (with [f (open args.output_path "w")]
          (json.dump result f :indent 2)))
      
      ;; Print to stdout if no output file
      (when (and result (not args.output_path))
        (print (json.dumps result :indent 2))))))

;; Standard entry point
(when (= __name__ "__main__")
  (main))
```

### 3. Command-Line Argument Handling

- Uses `argparse` for consistent argument parsing
- Arguments follow a consistent pattern:
  - Required arguments specified with `--arg-name`
  - Optional arguments have sensible defaults
  - File output controlled with `--output` parameter
- Help text is comprehensive and consistent

### 4. Error Handling

- Core functions wrap operations in try/except blocks
- Errors are reported clearly to stderr
- Non-zero exit codes for failures
- JSON output for machine-readable results

### 5. Input/Output Conventions

- Tools accept file paths as input
- Tools can write output to specified files via `--output`
- If no output file is specified, results print to stdout as JSON
- Binary data is handled consistently (e.g., base64 encoding)

### 6. Documentation

- Every function has a docstring explaining:
  - What the function does
  - Parameters with descriptions
  - Return values with descriptions
- Command-line arguments include help text
- Module-level docstring explains the purpose of the script

## Integration with Make

These CLI tools are designed to integrate well with Make for dependency tracking:

- Tools produce clear exit codes
- Tools use file-based I/O for compatibility with Make's dependency model
- Tools avoid side effects that would complicate dependency tracking

## Examples

The repository contains several examples of this CLI design pattern:

- `src/paper_summarizer.hy`: Summarizes academic papers
- `src/livestream_transcriber.hy`: Transcribes YouTube livestreams
- `src/multimodal_embeddings.hy`: Generates embeddings for text and images
- `src/vertex_embeddings.hy`: Generates text embeddings using Vertex AI
- `src/nearest_neighbors_search.hy`: Demonstrates vector similarity search methods
- `src/vertex_vector_search_setup.hy`: Sets up Vertex AI Vector Search

## Refactoring Existing Scripts

When refactoring existing scripts to follow this pattern:

1. Separate core functionality from CLI handling
2. Add proper argument parsing with `argparse`
3. Implement standardized error handling
4. Ensure file-based I/O follows conventions
5. Add proper documentation
6. Follow the standard entry point pattern