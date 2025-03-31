# Example of simple vector search with FAISS
import numpy as np
import faiss

# Sample vector data (embedding dimension = 10)
dim = 10          # Dimension of embeddings
n_vectors = 100   # Number of vectors in our database
np.random.seed(42)
vectors = np.random.random((n_vectors, dim)).astype('float32')  # Random vectors as database

# Create a FAISS index for vector search
index = faiss.IndexFlatL2(dim)  # L2 distance (Euclidean)
index.add(vectors)              # Add vectors to the index

# Query vector (what we're searching for)
query = np.random.random((1, dim)).astype('float32')

# Search for the 5 nearest vectors
k = 5  # Number of nearest neighbors to retrieve
distances, indices = index.search(query, k)

print(f"Query vector shape: {query.shape}")
print(f"Found {len(indices[0])} nearest neighbors")
print(f"Neighbor indices: {indices[0]}")
print(f"Distances: {distances[0]}")
