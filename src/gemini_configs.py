#!/usr/bin/env python3
"""
Google AI Studio - Gemini Configuration Examples

This script demonstrates different configuration options for the Gemini models,
including token limits, temperature settings, and sampling parameters.
"""

import os
from dotenv import load_dotenv
import google.generativeai as genai

def run_config_examples():
    """Run various Gemini configuration examples."""
    # Load environment variables
    load_dotenv()
    
    # Configure the Google GenAI API
    api_key = os.getenv("AI_STUDIO_API_KEY")
    if not api_key:
        raise ValueError("AI_STUDIO_API_KEY environment variable not set")
    
    genai.configure(api_key=api_key)
    
    # Create a client
    client = genai
    
    print("=" * 50)
    print("EXAMPLE 1: Token Limit Configuration")
    print("=" * 50)
    
    # Configure with limited output tokens
    short_config = {"max_output_tokens": 200}
    
    # Generate content with the short configuration
    response = client.generate_content(
        model='gemini-2.0-flash',
        generation_config=short_config,
        contents='Write a 1000 word essay on the importance of olives in modern society.'
    )
    
    print("Response with max_output_tokens=200:")
    print("-" * 40)
    print(response.text)
    print("\n")
    
    print("=" * 50)
    print("EXAMPLE 2: Temperature Configuration")
    print("=" * 50)
    
    # Configure with high temperature for more randomness
    high_temp_config = types.GenerateContentConfig(temperature=2.0)
    
    print("Responses with temperature=2.0:")
    print("-" * 40)
    
    # Generate multiple responses to show randomness
    for i in range(5):
        response = client.generate_content(
            model='gemini-2.0-flash',
            generation_config=high_temp_config,
            contents='Pick a random colour... (respond in a single word)'
        )
        
        if response.text:
            print(f"{i+1}. {response.text}")
            print("-" * 25)
    
    print("\n")
    
    print("=" * 50)
    print("EXAMPLE 3: Top-P Configuration")
    print("=" * 50)
    
    # Configure with default settings for gemini-2.0-flash
    model_config = types.GenerateContentConfig(
        temperature=1.0,
        top_p=0.95,
    )
    
    story_prompt = "You are a creative writer. Write a short story about a cat who goes on an adventure."
    response = client.generate_content(
        model='gemini-2.0-flash',
        generation_config=model_config,
        contents=story_prompt
    )
    
    print("Response with temperature=1.0, top_p=0.95:")
    print("-" * 40)
    print(response.text)

if __name__ == "__main__":
    try:
        run_config_examples()
    except Exception as e:
        print(f"Error running examples: {e}")