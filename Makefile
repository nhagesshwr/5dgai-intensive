# 5D-GAI Intensive Course - Makefile
# Helper commands for project management

.PHONY: help setup dev clean lint test api-test docker docker-jupyter docker-api

# Colors for terminal output
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
BLUE   := $(shell tput -Txterm setaf 4)
RESET  := $(shell tput -Txterm sgr0)

# Default target when running 'make'
help:
	@echo "${BLUE}5D-GAI Intensive Course${RESET} - Helper Commands"
	@echo ""
	@echo "${YELLOW}Usage:${RESET}"
	@echo "  ${YELLOW}make${RESET} ${GREEN}<command>${RESET}"
	@echo ""
	@echo "${YELLOW}Available commands:${RESET}"
	@echo "  ${GREEN}help${RESET}           Show this help message"
	@echo "  ${GREEN}setup${RESET}          Setup Python environment with Poetry"
	@echo "  ${GREEN}dev${RESET}            Start Poetry shell for development"
	@echo "  ${GREEN}clean${RESET}          Remove build artifacts and cache files"
	@echo "  ${GREEN}lint${RESET}           Run code linting"
	@echo "  ${GREEN}test${RESET}           Run tests"
	@echo "  ${GREEN}api-test${RESET}       Test API connectivity with Gemini"
	@echo "  ${GREEN}docker${RESET}         Build all Docker containers"
	@echo "  ${GREEN}docker-jupyter${RESET} Run Jupyter notebook server in Docker"
	@echo "  ${GREEN}docker-api${RESET}     Run API service in Docker"
	@echo ""
	@echo "${YELLOW}Examples:${RESET}"
	@echo "  ${YELLOW}make setup${RESET}    # Install project dependencies"
	@echo "  ${YELLOW}make api-test${RESET} # Verify Gemini API connectivity"

# Setup Python environment
setup:
	@echo "${BLUE}Setting up Python environment with Poetry...${RESET}"
	@if ! command -v poetry > /dev/null; then \
		echo "${YELLOW}Poetry not found. Installing...${RESET}"; \
		curl -sSL https://install.python-poetry.org | python3 -; \
	fi
	@echo "${GREEN}Installing dependencies...${RESET}"
	@poetry install
	@mkdir -p notebooks/{day1,day2,day3,day4,day5}
	@mkdir -p papers/{day1,day2,day3,day4,day5}
	@mkdir -p resources data config
	@if [ ! -f .env ]; then \
		echo "${YELLOW}Creating .env file template...${RESET}"; \
		echo "# API Keys" > .env; \
		echo "AI_STUDIO_API_KEY=\"\"" >> .env; \
		echo "KAGGLE_USERNAME=\"\"" >> .env; \
		echo "KAGGLE_KEY=\"\"" >> .env; \
		echo "OPENAI_API_KEY=\"\"" >> .env; \
		echo "ANTHROPIC_API_KEY=\"\"" >> .env; \
	fi
	@echo "${GREEN}Setup complete! Run 'make dev' to activate the environment.${RESET}"

# Activate Poetry shell for development
dev:
	@echo "${BLUE}Starting Poetry shell...${RESET}"
	@poetry shell

# Clean build artifacts and cache files
clean:
	@echo "${BLUE}Cleaning project...${RESET}"
	@find . -type d -name __pycache__ -exec rm -rf {} +
	@find . -type d -name .pytest_cache -exec rm -rf {} +
	@find . -type d -name .ipynb_checkpoints -exec rm -rf {} +
	@find . -type f -name "*.pyc" -delete
	@find . -type f -name "*.pyo" -delete
	@find . -type f -name "*.pyd" -delete
	@find . -type f -name ".coverage" -delete
	@find . -type d -name "*.egg-info" -exec rm -rf {} +
	@find . -type d -name "*.egg" -exec rm -rf {} +
	@find . -type d -name ".tox" -exec rm -rf {} +
	@find . -type d -name "*.dist-info" -exec rm -rf {} +
	@find . -type d -name "*htmlcov" -exec rm -rf {} +
	@echo "${GREEN}Project cleaned!${RESET}"

# Run code linting
lint:
	@echo "${BLUE}Running code linting...${RESET}"
	@poetry run black src tests
	@poetry run isort src tests
	@poetry run flake8 src tests
	@echo "${GREEN}Linting complete!${RESET}"

# Run tests
test:
	@echo "${BLUE}Running tests...${RESET}"
	@poetry run pytest -xvs tests
	@echo "${GREEN}Tests complete!${RESET}"

# Test API connectivity with Gemini
api-test:
	@echo "${BLUE}Testing Gemini API connectivity...${RESET}"
	@poetry run python -c "\
	from src.gemini_client import GeminiClient; \
	import sys; \
	try: \
		client = GeminiClient(); \
		response = client.generate_content('Hello, Gemini! Respond with a short greeting.'); \
		print(f'${GREEN}API Response: ${RESET}' + client.extract_text(response)); \
		print(f'${GREEN}API Test successful!${RESET}'); \
	except Exception as e: \
		print(f'${YELLOW}API Test failed: ${RESET}' + str(e), file=sys.stderr); \
		sys.exit(1);"

# Build Docker containers
docker:
	@echo "${BLUE}Building Docker containers...${RESET}"
	@docker-compose build
	@echo "${GREEN}Docker build complete!${RESET}"

# Run Jupyter notebook server in Docker
docker-jupyter:
	@echo "${BLUE}Starting Jupyter notebook server in Docker...${RESET}"
	@docker-compose up notebook
	@echo "${GREEN}Jupyter notebook server stopped.${RESET}"

# Run API service in Docker
docker-api:
	@echo "${BLUE}Starting API service in Docker...${RESET}"
	@docker-compose up api
	@echo "${GREEN}API service stopped.${RESET}"