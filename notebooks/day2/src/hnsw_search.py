# Using FAISS with HNSW (simplified)
import numpy as np
import faiss

# Parameters for HNSW
d = 64          # Dimension of embeddings
nb = 10000      # Number of vectors in database
M = 16          # Number of connections per layer (higher = better recall, more memory)
efConstruction = 40  # Size of dynamic candidate list (higher = better recall, slower construction)

# Generate random database vectors
np.random.seed(42)
database = np.random.random((nb, d)).astype('float32')

# Create HNSW index
index = faiss.IndexHNSWFlat(d, M)
index.hnsw.efConstruction = efConstruction
index.add(database)

# Generate query vector
query = np.random.random((1, d)).astype('float32')

# Set search parameters
index.hnsw.efSearch = 20  # Size of dynamic candidate list for search (higher = better recall, slower search)

# Search
k = 5  # Number of nearest neighbors to retrieve
distances, indices = index.search(query, k)

print(f"Query vector shape: {query.shape}")
print(f"Top {k} results - indices: {indices[0]}")
print(f"Distances: {distances[0]}")
