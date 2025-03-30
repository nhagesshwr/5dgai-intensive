#!/bin/bash
# Initialize directory structure and environment for 5-Day Gen AI Intensive
set -euo pipefail

echo "Setting up environment for 5-Day Gen AI Intensive course..."

# Create directory structure
mkdir -p notebooks/{day1,day2,day3,day4,day5}
mkdir -p papers/{day1,day2,day3,day4,day5}
mkdir -p podcasts resources src templates api config capstone

# Initialize Python environment using Poetry
if command -v poetry > /dev/null; then
    echo "Setting up Poetry environment..."
    poetry init \
        --name "gen-ai-intensive" \
        --description "Google's 5-Day Gen AI Intensive Course" \
        --author "Jason Walsh <j@wal.sh>" \
        --python "^3.11"
    
    poetry add \
        numpy pandas matplotlib seaborn scikit-learn \
        tensorflow torch transformers datasets \
        langchain langchain-openai llama-index openai anthropic google-generativeai \
        jupyter ipykernel ipywidgets kaggle \
        plotly gradio python-dotenv requests tqdm

    echo "Poetry environment setup complete"
else
    echo "Poetry not found, please install it or use the requirements.txt file"
fi

echo "Environment setup complete!"
echo "Next steps:"
echo "1. Run 'poetry install' to set up the virtual environment"
echo "2. Run 'poetry shell' to activate the environment"
echo "3. Set up your API credentials in your environment"
