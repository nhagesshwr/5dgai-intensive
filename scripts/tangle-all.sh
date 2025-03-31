#!/usr/bin/env bash
# Tangle all Org files in the project
set -euo pipefail

# Parent directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Create target directories
mkdir -p src/hy src/python

# Find all org files and tangle them
echo "Tangling all Org files..."

# Skip certain directories
find . -name "*.org" | grep -v -E '(/\.|/docs/|/node_modules/)' | while read -r file; do
  echo "Tangling $file..."
  emacs --batch \
    --eval "(require 'org)" \
    --eval "(setq org-confirm-babel-evaluate nil)" \
    --eval "(org-babel-tangle-file \"$file\")"
done

echo "All Org files tangled!"