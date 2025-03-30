#!/bin/bash
# Tangles all org files in the repository
# Uses Emacs in batch mode for clean operation

set -euo pipefail

# Directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." &> /dev/null && pwd)"

echo "Tangling all Org files in $REPO_ROOT..."

# Find all .org files (excluding docs directory)
find "$REPO_ROOT" -name "*.org" -not -path "*/docs/*" | while read -r file; do
  echo "Tangling $file..."
  emacs --batch \
    --eval "(require 'org)" \
    --eval "(setq org-confirm-babel-evaluate nil)" \
    --eval "(org-babel-tangle-file \"$file\")"
done

echo "Tangling complete!"