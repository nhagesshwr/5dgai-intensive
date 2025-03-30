# 5D-GAI Intensive Development Guide

## Build & Test Commands
- Setup environment: `poetry install` and `poetry shell`
- Run Python scripts: `python src/script_name.py`
- Run Jupyter: `jupyter notebook` or `jupyter lab`
- Import module in Python: `from src.gemini_client import GeminiClient`
- Environment setup: `source .env` (create from config/.envrc.template)

## Code Style Guidelines
- **Python**: Type annotations required for all functions
- **Formatting**: Google-style docstrings with Args/Returns sections
- **Imports**: Group in order: stdlib → third-party → local modules
- **Error Handling**: Use try/except with specific exceptions
- **Naming**: snake_case for variables/functions, PascalCase for classes
- **Environment**: Use dotenv for loading environment variables
- **API Keys**: Never hardcode - use environment variables
- **Models**: Default to gemini-2.0-flash unless specified
- **Testing**: Create test file with same name in tests/ directory

## Git Conventions
- **Commits**: Use Conventional Commits format: `<type>(<scope>): <description>`
- **Types**: feat, fix, docs, style, refactor, test, chore
- **Example**: `feat(api): add Gemini client implementation`
- **Attribution**: Use Git trailers instead of inline messages
  ```
  feat(client): add support for system instructions
  
  This commit adds support for system instructions to the GeminiClient class.
  
  Co-authored-by: Claude <claude@anthropic.com>
  Signed-off-by: jwalsh <jasonwalsh@gmail.com>
  ```

## Docker Usage
- Build environment: `docker-compose build`
- Run notebooks: `docker-compose up notebook`
- Run API service: `docker-compose up api`
- Access Jupyter: http://localhost:8888