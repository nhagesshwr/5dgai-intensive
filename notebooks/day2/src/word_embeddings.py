# Example of loading pre-trained word embeddings
# !pip install gensim
import gensim.downloader as api

# Load pre-trained Word2Vec embeddings
word_vectors = api.load('word2vec-google-news-300')

# Find similar words
similar_words = word_vectors.most_similar('computer', topn=3)
print("Words similar to 'computer':")
for word, similarity in similar_words:
    print(f"  {word}: {similarity:.4f}")

# Calculate similarity between two words
similarity = word_vectors.similarity('computer', 'laptop')
print(f"\nSimilarity between 'computer' and 'laptop': {similarity:.4f}")
