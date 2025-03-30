# 5D-GAI Intensive

Resources, utilities, and implementations for Google's 5-Day Generative AI Intensive course.

[![Python Version](https://img.shields.io/badge/python-3.11-blue.svg)](https://python.org)
[![Poetry](https://img.shields.io/badge/poetry-managed-blueviolet)](https://python-poetry.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## About

This repository contains materials and implementations related to Google's 5-Day Generative AI Intensive course, focused on working with Google's Gemini models and other generative AI tools. It includes:

- 🤖 Gemini API client implementations
- 📊 Jupyter notebooks for daily exercises
- 🔧 Utility functions for AI model interaction
- 🐳 Dockerized development environment
- 📚 References and resources for the course

## Features

- **Gemini API Client**: Clean Python interface to Google's Gemini API
- **Environment Management**: Poetry-based dependency management
- **Dockerized Setup**: Containerized environment for consistent development
- **Structured Layout**: Organized directories for notebooks, source code, and resources
- **Jupyter Integration**: Ready-to-use Jupyter environment for experimentation

## Setup

### Prerequisites

- Python 3.11+
- Poetry (recommended) or pip
- Docker (optional)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/5dgai-intensive.git
   cd 5dgai-intensive
   ```

2. Install dependencies:
   ```bash
   poetry install
   ```

3. Set up environment variables:
   ```bash
   cp config/.envrc.template .env
   # Edit .env to add your API keys
   ```

4. Activate environment:
   ```bash
   poetry shell
   ```

### Docker Setup

Alternatively, use Docker:

```bash
docker-compose up notebook
```

Then access Jupyter at http://localhost:8888

## Usage

```python
from src.gemini_client import GeminiClient

# Initialize client with API key
client = GeminiClient()

# Generate content
response = client.generate_content("Explain generative AI in one paragraph.")
print(client.extract_text(response))
```

## Project Structure

```
5dgai-intensive/
├── api/              # API endpoint definitions
├── config/           # Configuration templates
├── notebooks/        # Jupyter notebooks for exercises
│   └── day1/         # Organized by course day
├── papers/           # Research papers and references
├── resources/        # Additional resources
├── src/              # Source code
│   └── gemini_client.py  # Gemini API client
├── docker-compose.yml    # Docker configuration
├── pyproject.toml        # Python project configuration
└── README.md             # This file
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.