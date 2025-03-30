#!/bin/bash
# Lint and format all files in the repository
# Handles Python, Shell, Org mode and Elisp files

set -euo pipefail

# Directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." &> /dev/null && pwd)"

echo "Linting all files in $REPO_ROOT..."

# Function to check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# Check if required tools are installed
MISSING_TOOLS=()
if ! command_exists black; then MISSING_TOOLS+=("black"); fi
if ! command_exists ruff; then MISSING_TOOLS+=("ruff"); fi
if ! command_exists shellcheck; then MISSING_TOOLS+=("shellcheck"); fi
if ! command_exists shfmt; then MISSING_TOOLS+=("shfmt"); fi

if [ ${#MISSING_TOOLS[@]} -gt 0 ]; then
    echo "The following required tools are missing:"
    for tool in "${MISSING_TOOLS[@]}"; do
        echo "  - $tool"
    done
    echo "Please install them and try again."
    echo "Recommendation: poetry add --dev black ruff"
    echo "                apt-get install shellcheck"
    echo "                go install mvdan.cc/sh/v3/cmd/shfmt@latest"
    exit 1
fi

# Python linting and formatting
echo "Linting Python files..."
find "$REPO_ROOT" -name "*.py" | while read -r file; do
    echo "  Processing $file"
    black --quiet "$file"
    ruff check --fix "$file"
done

# Shell script linting and formatting
echo "Linting shell scripts..."
find "$REPO_ROOT" -name "*.sh" | while read -r file; do
    echo "  Processing $file"
    shellcheck "$file"
    shfmt -w -s -i 4 "$file"
done

# Org-mode linting
echo "Linting Org files..."
find "$REPO_ROOT" -name "*.org" | while read -r file; do
    echo "  Processing $file"
    emacs --batch \
        --load="$REPO_ROOT/.emacs.d/init.el" \
        --eval "(require 'org)" \
        --visit="$file" \
        --eval "(org-lint)" \
        --eval "(org-mode)" \
        --eval "(org-table-map-tables 'org-table-align)" \
        --eval "(save-buffer)" \
        --kill
done

# Elisp linting
echo "Linting Elisp files..."
find "$REPO_ROOT" -name "*.el" | while read -r file; do
    echo "  Processing $file"
    emacs --batch \
        --eval "(setq byte-compile-error-on-warn nil)" \
        --eval "(byte-compile-file \"$file\")"
    
    # Clean up the .elc files afterward
    if [ -f "${file}c" ]; then
        rm "${file}c"
    fi
done

# Checking for JSON/YAML formatting (if available)
if command_exists prettier; then
    echo "Formatting JSON/YAML files..."
    find "$REPO_ROOT" \( -name "*.json" -o -name "*.yaml" -o -name "*.yml" \) | while read -r file; do
        echo "  Processing $file"
        prettier --write "$file"
    done
fi

echo "Linting complete!"