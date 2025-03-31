# Example using Vertex AI Text Embeddings (pseudocode as it requires API setup)
from vertexai.language_models import TextEmbeddingModel, TextEmbeddingInput

def embed_texts(texts, task_type="RETRIEVAL_DOCUMENT"):
    """Generate embeddings for a list of texts using Vertex AI."""
    # Initialize the model (requires proper setup in actual environment)
    model = TextEmbeddingModel.from_pretrained("text-embedding-004")
    
    # Create embedding inputs with appropriate task type
    inputs = [TextEmbeddingInput(text=text, task_type=task_type) for text in texts]
    
    # Generate embeddings
    embeddings = model.get_embeddings(inputs)
    
    # Print embedding dimension
    print(f"Embedding dimension: {len(embeddings[0].values)}")
    return embeddings

# Demo with sample texts
sample_texts = [
    "The earth is spherical.",
    "The earth is a planet.",
    "I like to eat at a restaurant."
]

# This is just pseudo-code - would require actual API setup
# embeddings = embed_texts(sample_texts)
# print(f"Number of embeddings: {len(embeddings)}")
