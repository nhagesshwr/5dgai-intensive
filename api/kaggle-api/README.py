import os
import pandas as pd
from kaggle.api.kaggle_api_extended import KaggleApi

# Initialize the Kaggle API
api = KaggleApi()
api.authenticate()

# Get competitions and convert to DataFrame
competitions = api.competitions_list(search="generative ai")
df = pd.DataFrame(competitions)

# Display basic stats
print(f"Total competitions: {len(df)}")
print("\nCategories:")
print(df['category'].value_counts())

print("\nRecent competitions:")
print(df[['title', 'deadline']].head(5))
