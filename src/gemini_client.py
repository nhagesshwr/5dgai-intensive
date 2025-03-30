"""
Google AI Gemini API Client

This script provides utilities for interacting with the Google AI Gemini API.
"""

import os
import json
import requests
from dotenv import load_dotenv
from typing import Dict, List, Any, Optional

# Load environment variables
load_dotenv()

class GeminiClient:
    """Client for interacting with Google's Gemini API."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the Gemini client.
        
        Args:
            api_key: Google AI Studio API key. If None, will try to load from environment.
        """
        self.api_key = api_key or os.environ.get("AI_STUDIO_API_KEY")
        if not self.api_key:
            raise ValueError("No API key provided. Set AI_STUDIO_API_KEY environment variable or pass as parameter.")
        
        self.base_url = "https://generativelanguage.googleapis.com/v1beta"
    
    def generate_content(self, 
                         prompt: str, 
                         model: str = "gemini-2.0-flash",
                         system_instruction: Optional[str] = None) -> Dict[str, Any]:
        """
        Generate content using Gemini API.
        
        Args:
            prompt: The text prompt to send
            model: The model to use (default: gemini-2.0-flash)
            system_instruction: Optional system instruction
            
        Returns:
            Dict containing the API response
        """
        url = f"{self.base_url}/models/{model}:generateContent?key={self.api_key}"
        
        payload = {
            "contents": [{
                "parts": [{"text": prompt}]
            }]
        }
        
        if system_instruction:
            payload["systemInstruction"] = {
                "parts": [{"text": system_instruction}]
            }
        
        headers = {
            "Content-Type": "application/json"
        }
        
        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        
        return response.json()
    
    def chat(self, 
             messages: List[Dict[str, Any]], 
             model: str = "gemini-2.0-flash",
             system_instruction: Optional[str] = None) -> Dict[str, Any]:
        """
        Have a chat conversation using Gemini API.
        
        Args:
            messages: List of message dictionaries with 'role' and 'content'
            model: The model to use (default: gemini-2.0-flash)
            system_instruction: Optional system instruction
            
        Returns:
            Dict containing the API response
        """
        url = f"{self.base_url}/models/{model}:generateContent?key={self.api_key}"
        
        # Format messages for Gemini API
        contents = []
        for msg in messages:
            content = {
                "role": msg["role"],
                "parts": [{"text": msg["content"]}]
            }
            contents.append(content)
        
        payload = {
            "contents": contents
        }
        
        if system_instruction:
            payload["systemInstruction"] = {
                "parts": [{"text": system_instruction}]
            }
        
        headers = {
            "Content-Type": "application/json"
        }
        
        response = requests.post(url, headers=headers, json=payload)
        response.raise_for_status()
        
        return response.json()
    
    def extract_text(self, response: Dict[str, Any]) -> str:
        """
        Extract text content from API response.
        
        Args:
            response: The API response dictionary
            
        Returns:
            Extracted text content
        """
        try:
            return response['candidates'][0]['content']['parts'][0]['text']
        except (KeyError, IndexError):
            return "No text content found in response"


if __name__ == "__main__":
    # Example usage
    client = GeminiClient()
    
    # Simple prompt
    response = client.generate_content("Explain how AI works in 3 sentences.")
    print("Response:", client.extract_text(response))
    
    # Chat conversation
    chat_messages = [
        {"role": "user", "content": "What are three benefits of using generative AI?"},
        {"role": "model", "content": "1. Enhanced creativity and idea generation\n2. Automation of content creation\n3. Personalization of user experiences"},
        {"role": "user", "content": "Can you elaborate on the third point?"}
    ]
    
    chat_response = client.chat(chat_messages)
    print("\nChat Response:", client.extract_text(chat_response))
