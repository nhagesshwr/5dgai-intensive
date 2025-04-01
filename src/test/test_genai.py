#!/usr/bin/env python3
"""
Test script for Google GenAI API integration
Tests basic generation and embedding functionality
"""

import os
from dotenv import load_dotenv

# Load environment variables
load_dotenv()
GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")

if not GOOGLE_API_KEY:
    print("❌ Error: GOOGLE_API_KEY not found in environment!")
    print("  Create a .env file with GOOGLE_API_KEY=your_api_key")
    exit(1)

print(f"API Key: {'*' * (len(GOOGLE_API_KEY) - 8)}{GOOGLE_API_KEY[-4:]}")

try:
    # Import GenAI dependencies
    from google import genai
    
    # Initialize client
    client = genai.Client(api_key=GOOGLE_API_KEY)
    
    # Test 1: Basic generation
    print("\n🧪 Test 1: Basic content generation")
    print("---------------------------------")
    response = client.models.generate_content(
        model="gemini-2.0-flash",
        contents="Explain AI to me like I'm a kid.")
    print(response.text)
    
    # Test 2: List available embedding models
    print("\n🧪 Test 2: Available embedding models")
    print("---------------------------------")
    embedding_models = []
    for model in client.models.list():
        if 'embedContent' in model.supported_actions:
            embedding_models.append(model.name)
            print(f"- {model.name}")
    
    # Test 3: Generate an embedding (if any embedding models available)
    if embedding_models:
        print("\n🧪 Test 3: Generate sample embedding")
        print("---------------------------------")
        embedding_model = embedding_models[0]
        print(f"Using model: {embedding_model}")
        
        text = "dépatouiller"
        try:
            # Try the new API style (1.7.0)
            embedding_result = client.embeddings.create(
                model=embedding_model,
                content=text
            )
            # Print the first 5 dimensions of the embedding
            vector = embedding_result.embedding
            print(f"Text: {text}")
            print(f"Embedding dimensions: {len(vector)}")
            print(f"First 5 values: {vector[:5]}")
        except Exception as e1:
            print(f"Error with new API style: {e1}")
            try:
                # Try the alternate API style
                embedding_result = client.get_embeddings(
                    model=embedding_model,
                    texts=[text]
                )
                # Print the first 5 dimensions of the embedding
                vector = embedding_result[0]
                print(f"Text: {text}")
                print(f"Embedding dimensions: {len(vector)}")
                print(f"First 5 values: {vector[:5]}")
            except Exception as e2:
                print(f"Error with alternate API style: {e2}")
    else:
        print("\n❌ No embedding models available")

    print("\n✅ All tests completed successfully!")
        
except Exception as e:
    print(f"\n❌ Error: {e}")
    print("\nTroubleshooting:")
    print("1. Ensure google-genai==1.7.0 is installed")
    print("2. Verify your API key is correct")
    print("3. Check your network connection")