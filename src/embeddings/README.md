# French Verb Embeddings - Capstone Project

This is a capstone project for the 5-Day Gen AI Intensive course demonstrating how to use Google's Generative AI API to create embeddings for French verbs.

## Features

- Generates embeddings for 20 common French verbs
- Uses Google's Generative AI API with the latest embedding models
- Implements caching to avoid redundant API calls
- Processes verbs in batches to optimize throughput
- Includes multiple fallback methods for different API versions
- Implements retry mechanisms for error recovery
- Saves embeddings in both JSON and pickle formats
- Creates metadata for the embeddings collection

## Installation

```bash
# Create a virtual environment (optional but recommended)
python -m venv .venv-embeddings
source .venv-embeddings/bin/activate

# Install dependencies
pip install -r requirements.txt
```

## Usage

```bash
# Run with default settings (processes all 20 verbs)
hy verb_embeddings.hy

# Process only the first 5 verbs
hy verb_embeddings.hy 5

# Process all verbs with custom batch size and retries
hy verb_embeddings.hy 20 3 2  # 20 verbs, batch size 3, 2 retries
```

## API Key Setup

Place your Google AI Studio API key in a `.env` file in the project root:

```
AI_STUDIO_API_KEY=your_api_key_here
```

## Output

The script generates the following files in the `data/embeddings` directory:

- `french_verb_embeddings.json`: Embeddings in JSON format
- `french_verb_embeddings.pkl`: Embeddings in Python pickle format
- `embeddings_metadata.json`: Metadata about the embeddings
- `cache/`: Directory containing cached embeddings for individual verbs