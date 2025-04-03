#!/usr/bin/env python
"""
Ultra-minimal test for Google API embeddings with a single French verb
"""

import os
import sys
import json
from pathlib import Path

# Try to load environment variables if dotenv is available
try:
    from dotenv import load_dotenv
    load_dotenv()
except ImportError:
    print("Warning: python-dotenv not installed, using environment variables directly")

# Get API key
API_KEY = os.getenv("AI_STUDIO_API_KEY") or os.getenv("GOOGLE_API_KEY")

if not API_KEY:
    print("❌ Error: No API key found! Set AI_STUDIO_API_KEY in .env file")
    sys.exit(1)

# Check if we can import google.generativeai
try:
    import google.generativeai as genai
    print("✓ Successfully imported google.generativeai")
except ImportError:
    print("❌ Error: Failed to import google.generativeai")
    print("Try: pip install google-generativeai")
    sys.exit(1)

def main():
    """Test Gemini API with simple embedding of 'être'"""
    print("\n🔍 Testing Google API with a single French verb...")
    
    # Configure API
    genai.configure(api_key=API_KEY)
    
    # Get embedding model
    try:
        # List models
        models = genai.list_models()
        embed_models = [m for m in models if "embedContent" in m.supported_actions]
        
        if embed_models:
            print(f"✓ Found {len(embed_models)} embedding models:")
            for model in embed_models:
                print(f"  - {model.name}")
            
            # Use first model
            model_name = embed_models[0].name
        else:
            model_name = "embedding-001"  # Default fallback
            print(f"No embedding models found, using default: {model_name}")
        
        # Get the model
        embedding_model = genai.get_model(model_name)
        print(f"Using model: {model_name}")
        
        # Test with a single verb
        test_verb = "être"
        print(f"\nGenerating embedding for '{test_verb}'...")
        
        # Get embedding
        result = embedding_model.embed_content(test_verb)
        
        # Check result
        if hasattr(result, "embedding") and hasattr(result.embedding, "values"):
            embedding = result.embedding.values
            print(f"✓ Success! Got embedding with {len(embedding)} dimensions")
            print(f"First 5 values: {embedding[:5]}")
            
            # Save to file
            output_dir = Path.cwd() / "data" / "embeddings"
            output_dir.mkdir(parents=True, exist_ok=True)
            
            output_file = output_dir / "etre_embedding.json"
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(embedding, f, indent=2)
            
            print(f"Embedding saved to: {output_file}")
            return True
        else:
            print("❌ Error: Unexpected response format")
            print(f"Response: {result}")
            return False
    
    except Exception as e:
        print(f"❌ Error during API call: {e}")
        return False

if __name__ == "__main__":
    success = main()
    print("\n✓ Test complete!" if success else "\n❌ Test failed!")