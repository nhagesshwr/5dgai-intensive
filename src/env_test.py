#!/usr/bin/env python3
"""
Minimal Environment Test for Google GenAI
"""

import os
from dotenv import load_dotenv

# Load environment
load_dotenv()

# Print key info
GOOGLE_API_KEY = os.getenv("GOOGLE_API_KEY")
print(f"{len(GOOGLE_API_KEY)=}, {GOOGLE_API_KEY[:4]=}, {GOOGLE_API_KEY[-4:]=}")

# Try imports
try:
    from google import genai
    from google.genai import types
    print("✅ Imports successful")
    print(f"Package info: {getattr(genai, '__version__', 'version not available')}")
except Exception as e:
    print(f"❌ Import error: {e}")
    print("Try: pip install google-genai==1.7.0")