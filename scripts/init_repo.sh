#!/bin/bash
# Initialize the GitHub repository
# Requires gh CLI to be installed and authenticated

set -e

echo "Initializing GitHub repository for 5D-GAI Intensive..."

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

# Check if the user has provided an organization name
if [ "$#" -eq 1 ]; then
    ORG_NAME="$1"
    echo "Creating repository under organization: $ORG_NAME"
    CREATE_CMD="gh repo create $ORG_NAME/5dgai-intensive"
else
    echo "Creating repository under your personal account"
    CREATE_CMD="gh repo create 5dgai-intensive"
fi

# Load metadata from the repo-metadata.json file
if [ -f .github/repo-metadata.json ]; then
    DESCRIPTION=$(cat .github/repo-metadata.json | jq -r '.description')
    HOMEPAGE=$(cat .github/repo-metadata.json | jq -r '.homepage')
    VISIBILITY=$([ $(cat .github/repo-metadata.json | jq -r '.private') == "true" ] && echo "--private" || echo "--public")
    
    # Join topics with commas
    TOPICS=$(cat .github/repo-metadata.json | jq -r '.topics | join(",")')
    
    # Create the repository
    $CREATE_CMD --description "$DESCRIPTION" $VISIBILITY --homepage "$HOMEPAGE" --source=. --push
    
    # Set topics
    if [ ! -z "$TOPICS" ]; then
        echo "Setting repository topics..."
        gh repo edit --add-topic "$TOPICS"
    fi
    
    echo "Repository created successfully!"
    echo "Setting up labels..."
    ./scripts/setup_labels.sh
    
    echo "Repository initialization complete!"
else
    echo "Error: .github/repo-metadata.json not found."
    exit 1
fi