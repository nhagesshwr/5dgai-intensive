#!/usr/bin/env python
# Test for google-genai 1.7.0
import os
from google import genai

# Configure API
API_KEY = os.getenv("AI_STUDIO_API_KEY")
genai.configure(api_key=API_KEY)

# Print version
print(f"Using google-genai version: {genai.__version__}")

# Simple embedding test
verb = "être"
response = genai.embed_content(
    model="embedding-001",
    content=verb
)

# Print result
print(f"Generated embedding for '{verb}' with {len(response.embedding)} dimensions")
print(f"First 5 values: {response.embedding[:5]}")