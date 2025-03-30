from src.gemini_client import GeminiClient

# Initialize client with API key
client = GeminiClient()

# Generate content
response = client.generate_content("Explain generative AI in one paragraph.")
print(client.extract_text(response))