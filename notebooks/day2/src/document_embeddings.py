# Example using Doc2Vec for document embeddings
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
from gensim.utils import simple_preprocess

# Sample documents
documents = [
    "The earth is spherical.",
    "The earth is a planet.",
    "I like to eat at a restaurant."
]

# Preprocess and tag documents
tagged_docs = [TaggedDocument(simple_preprocess(doc), [i]) for i, doc in enumerate(documents)]

# Train a Doc2Vec model
model = Doc2Vec(tagged_docs, vector_size=10, window=2, min_count=1, workers=4, epochs=20)

# Get document vectors
doc_vectors = [model.dv[i] for i in range(len(documents))]

# Calculate similarity between documents
from scipy.spatial.distance import cosine

similarity_0_1 = 1 - cosine(doc_vectors[0], doc_vectors[1])
similarity_0_2 = 1 - cosine(doc_vectors[0], doc_vectors[2])

print(f"Similarity between doc 0 and doc 1: {similarity_0_1:.4f}")
print(f"Similarity between doc 0 and doc 2: {similarity_0_2:.4f}")
