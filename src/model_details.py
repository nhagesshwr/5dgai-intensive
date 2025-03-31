#!/usr/bin/env python3
"""
Google AI Studio - Detailed Model Information

This script provides detailed information about the gemini-2.0-flash model
in Google AI Studio.
"""

import os
import json
import pprint
from dotenv import load_dotenv
import google.generativeai as genai

def get_model_details(model_name="models/gemini-2.0-flash"):
    """
    Get detailed information about a specific model.
    
    Args:
        model_name: Name of the model to examine
        
    Returns:
        Detailed model information
    """
    # Load environment variables
    load_dotenv()
    
    # Configure the Google GenAI API
    api_key = os.getenv("AI_STUDIO_API_KEY")
    if not api_key:
        raise ValueError("AI_STUDIO_API_KEY environment variable not set")
    
    genai.configure(api_key=api_key)
    
    # Get the list of models
    try:
        models = genai.list_models()
        target_model = None
        
        # Find the specific model
        for model in models:
            if model.name == model_name:
                target_model = model
                break
        
        if not target_model:
            print(f"Model {model_name} not found")
            return
        
        # Print model details
        print("\nModel Information for:", model_name)
        print("====================================")
        
        # Extract important model attributes
        print(f"Display Name: {target_model.display_name}")
        print(f"Description: {target_model.description}")
        print(f"Input Token Limit: {target_model.input_token_limit}")
        print(f"Output Token Limit: {target_model.output_token_limit}")
        
        # Print temperature range
        if hasattr(target_model, 'temperature_range'):
            print(f"Temperature Range: {target_model.temperature_range}")
            
        # Print supported generation methods
        if hasattr(target_model, 'supported_generation_methods'):
            print("\nSupported Generation Methods:")
            for method in target_model.supported_generation_methods:
                print(f"  - {method}")
                
        # Get model as dictionary
        print("\nRaw Model Details:")
        model_dict = {}
        for key, value in vars(target_model).items():
            if not key.startswith('_'):
                model_dict[key] = value
                
        pprint.pprint(model_dict)
        
    except Exception as e:
        print(f"Error getting model details: {e}")

if __name__ == "__main__":
    get_model_details()