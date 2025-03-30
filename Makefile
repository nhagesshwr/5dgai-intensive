# 5D-GAI Intensive Course - Makefile
# Helper commands for project management

.PHONY: help setup dev clean lint lint-py lint-sh lint-org lint-el format format-py format-sh test api-test docker docker-jupyter docker-api tangle tangle-all install-dev-tools

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
	@echo "  ${YELLOW}Linting & Formatting:${RESET}"
	@echo "  ${GREEN}lint${RESET}           Run all linters (Python, Shell, Org, Elisp)"
	@echo "  ${GREEN}lint-py${RESET}        Lint Python files"
	@echo "  ${GREEN}lint-sh${RESET}        Lint shell scripts"
	@echo "  ${GREEN}lint-org${RESET}       Lint Org mode files"
	@echo "  ${GREEN}lint-el${RESET}        Lint Emacs Lisp files"
	@echo "  ${GREEN}format${RESET}         Format all code files"
	@echo "  ${GREEN}format-py${RESET}      Format Python files with black and isort"
	@echo "  ${GREEN}format-sh${RESET}      Format shell scripts with shfmt"
	@echo "  ${YELLOW}Org-mode & Tangling:${RESET}"
	@echo "  ${GREEN}tangle${RESET}         Tangle a specific org file (make tangle FILE=path/to/file.org)"
	@echo "  ${GREEN}tangle-all${RESET}     Tangle all org files in the project"
	@echo "  ${YELLOW}Testing & Docker:${RESET}"
	@echo "  ${GREEN}test${RESET}           Run tests"
	@echo "  ${GREEN}api-test${RESET}       Test API connectivity with Gemini"
	@echo "  ${GREEN}docker${RESET}         Build all Docker containers"
	@echo "  ${GREEN}docker-jupyter${RESET} Run Jupyter notebook server in Docker"
	@echo "  ${GREEN}docker-api${RESET}     Run API service in Docker"
	@echo "  ${YELLOW}Development Tools:${RESET}"
	@echo "  ${GREEN}install-dev-tools${RESET} Install development tools (linters, formatters)"
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

# Install development tools
install-dev-tools:
	@echo "${BLUE}Installing development tools...${RESET}"
	@poetry add --group dev black ruff isort flake8 mypy pytest
	@if ! command -v shellcheck > /dev/null; then \
		echo "${YELLOW}Installing shellcheck...${RESET}"; \
		if command -v apt-get > /dev/null; then \
			sudo apt-get update && sudo apt-get install -y shellcheck; \
		elif command -v brew > /dev/null; then \
			brew install shellcheck; \
		else \
			echo "${YELLOW}Please install shellcheck manually: https://github.com/koalaman/shellcheck${RESET}"; \
		fi; \
	fi
	@if ! command -v shfmt > /dev/null; then \
		echo "${YELLOW}Installing shfmt...${RESET}"; \
		if command -v go > /dev/null; then \
			go install mvdan.cc/sh/v3/cmd/shfmt@latest; \
		elif command -v brew > /dev/null; then \
			brew install shfmt; \
		else \
			echo "${YELLOW}Please install shfmt manually: https://github.com/mvdan/sh${RESET}"; \
		fi; \
	fi
	@echo "${GREEN}Development tools installed!${RESET}"

# Run all linters
lint: lint-py lint-sh lint-org lint-el
	@echo "${GREEN}All linting complete!${RESET}"

# Lint Python files
lint-py:
	@echo "${BLUE}Linting Python files...${RESET}"
	@poetry run ruff check src tests examples
	@poetry run mypy src
	@poetry run flake8 src tests examples
	@echo "${GREEN}Python linting complete!${RESET}"

# Lint shell scripts
lint-sh:
	@echo "${BLUE}Linting shell scripts...${RESET}"
	@find . -name "*.sh" -type f -not -path "./.*" -not -path "*/node_modules/*" -exec shellcheck {} \;
	@echo "${GREEN}Shell script linting complete!${RESET}"

# Lint Org mode files
lint-org:
	@echo "${BLUE}Linting Org files...${RESET}"
	@for file in $$(find . -name "*.org" -type f -not -path "./.*" -not -path "*/node_modules/*"); do \
		echo "  Linting $${file}"; \
		emacs --batch --load=.emacs.d/init.el --eval "(require 'org)" --visit="$${file}" --eval "(org-lint)" --kill || true; \
	done
	@echo "${GREEN}Org linting complete!${RESET}"

# Lint Emacs Lisp files
lint-el:
	@echo "${BLUE}Linting Emacs Lisp files...${RESET}"
	@for file in $$(find . -name "*.el" -type f -not -path "./.*" -not -path "*/node_modules/*"); do \
		echo "  Linting $${file}"; \
		emacs --batch --eval "(setq byte-compile-error-on-warn nil)" --eval "(byte-compile-file \"$${file}\")" || true; \
		if [ -f "$${file}c" ]; then rm "$${file}c"; fi; \
	done
	@echo "${GREEN}Emacs Lisp linting complete!${RESET}"

# Format all code files
format: format-py format-sh
	@echo "${GREEN}All formatting complete!${RESET}"

# Format Python files
format-py:
	@echo "${BLUE}Formatting Python files...${RESET}"
	@poetry run black src tests examples
	@poetry run isort src tests examples
	@echo "${GREEN}Python formatting complete!${RESET}"

# Format shell scripts
format-sh:
	@echo "${BLUE}Formatting shell scripts...${RESET}"
	@if command -v shfmt > /dev/null; then \
		find . -name "*.sh" -type f -not -path "./.*" -not -path "*/node_modules/*" -exec shfmt -w -s -i 4 {} \; ; \
		echo "${GREEN}Shell script formatting complete!${RESET}"; \
	else \
		echo "${YELLOW}shfmt not found. Run 'make install-dev-tools' to install it.${RESET}"; \
	fi

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

# Tangle a specific org file
tangle:
	@if [ -z "$(FILE)" ]; then \
		echo "${YELLOW}Error: No file specified. Usage: make tangle FILE=path/to/file.org${RESET}"; \
		exit 1; \
	fi
	@if [ ! -f "$(FILE)" ]; then \
		echo "${YELLOW}Error: File $(FILE) not found${RESET}"; \
		exit 1; \
	fi
	@echo "${BLUE}Tangling $(FILE)...${RESET}"
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(org-babel-tangle-file \"$(FILE)\")"
	@echo "${GREEN}Tangling complete!${RESET}"

# Tangle all org files
tangle-all:
	@echo "${BLUE}Tangling all Org files...${RESET}"
	@for file in $$(find . -name "*.org" -type f -not -path "./.*" -not -path "*/docs/*" -not -path "*/node_modules/*"); do \
		echo "  Tangling $${file}"; \
		emacs --batch \
			--eval "(require 'org)" \
			--eval "(setq org-confirm-babel-evaluate nil)" \
			--eval "(org-babel-tangle-file \"$${file}\")"; \
	done
	@echo "${GREEN}All Org files tangled!${RESET}"