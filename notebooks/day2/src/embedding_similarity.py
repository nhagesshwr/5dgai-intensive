# Simple example of comparing semantic similarity using embeddings
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np

# Hypothetical embeddings for words
computer_embed = np.array([[0.2, 0.8, 0.7, 0.1]])
laptop_embed = np.array([[0.25, 0.75, 0.6, 0.2]])
car_embed = np.array([[0.9, 0.2, 0.1, 0.7]])

# Calculate similarities
print(f"Computer-Laptop similarity: {cosine_similarity(computer_embed, laptop_embed)[0][0]:.4f}")
print(f"Computer-Car similarity: {cosine_similarity(computer_embed, car_embed)[0][0]:.4f}")
