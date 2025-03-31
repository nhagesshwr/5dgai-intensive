#!/usr/bin/env bash
# Tangle an Org file into source code files
set -euo pipefail

# Check if a file was specified
if [ $# -lt 1 ]; then
  echo "Usage: $0 <file.org> [<output_dir>]"
  exit 1
fi

ORG_FILE="$1"
OUTPUT_DIR="${2:-}"

# Check if file exists
if [ ! -f "$ORG_FILE" ]; then
  echo "Error: File $ORG_FILE not found"
  exit 1
fi

# If output directory specified, create it
if [ -n "$OUTPUT_DIR" ]; then
  mkdir -p "$OUTPUT_DIR"
  TANGLE_ARG="\"$OUTPUT_DIR\""
else
  TANGLE_ARG="nil"
fi

# Tangle the file
echo "Tangling $ORG_FILE..."
emacs --batch \
  --eval "(require 'org)" \
  --eval "(setq org-confirm-babel-evaluate nil)" \
  --eval "(org-babel-tangle-file \"$ORG_FILE\" $TANGLE_ARG)"

echo "Tangling complete!"