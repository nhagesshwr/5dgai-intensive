#!/bin/bash
# Script to set up GitHub labels using GitHub CLI
# Requires gh CLI to be installed and authenticated

set -e

echo "Setting up GitHub labels from .github/labels.json..."

# Check if gh is installed
if ! command -v gh &> /dev/null; then
    echo "Error: GitHub CLI (gh) is not installed. Please install it first."
    exit 1
fi

# Check if user is authenticated
if ! gh auth status &> /dev/null; then
    echo "Error: You need to authenticate with GitHub first. Run 'gh auth login'."
    exit 1
fi

# Get the repository name from the remote URL
REPO_URL=$(git config --get remote.origin.url)
if [ -z "$REPO_URL" ]; then
    echo "Error: No git remote found. Make sure this repository has a remote."
    exit 1
fi

# Extract owner and repo name from URL
if [[ $REPO_URL == *"github.com"* ]]; then
    REPO_PATH=$(echo $REPO_URL | sed -E 's/.*github.com[\/:]([^\/]+\/[^\/]+)(\.git)?$/\1/')
else
    echo "Error: Remote URL doesn't appear to be a GitHub repository."
    exit 1
fi

echo "Repository identified as: $REPO_PATH"

# Check if labels.json exists
if [ ! -f .github/labels.json ]; then
    echo "Error: .github/labels.json not found."
    exit 1
fi

# For each label in the JSON file
cat .github/labels.json | jq -c '.[]' | while read -r label; do
    NAME=$(echo $label | jq -r '.name')
    COLOR=$(echo $label | jq -r '.color')
    DESC=$(echo $label | jq -r '.description')
    
    echo "Creating/updating label: $NAME"
    gh label create "$NAME" --color "$COLOR" --description "$DESC" --force
done

echo "Label setup complete!"