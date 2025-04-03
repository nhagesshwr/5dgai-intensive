#!/usr/bin/env python
from dotenv import load_dotenv
import os
from google import genai
from google.genai import types

load_dotenv()
API_KEY = os.getenv("AI_STUDIO_API_KEY")
genai.configure(api_key=API_KEY)

client = genai.Client()

verbs = ["être", "avoir", "aller", "parler", "faire", "prendre", "vouloir", "savoir", "pouvoir", "venir"]

response = client.models.embed_content(
    model='models/text-embedding-004',
    contents=verbs,
    config=types.EmbedContentConfig(task_type='semantic_similarity')
)