# Build, Test, and Lint Commands

## Environment Setup
- Setup Python environment with Poetry: `make setup`
- Start Poetry shell for development: `make dev`
- Manual Poetry setup: `poetry install` and `poetry shell`
- Configure API keys in `.env` file

## Testing Commands
- Run all tests: `make test`
- Run with pytest directly: `pytest -xvs tests`
- Run single test: `pytest -xvs tests/path/to/test.py::test_function_name`
- Test API connectivity: `make api-test`

## Linting and Formatting
- Run all linters: `make lint`
- Run specific linters:
  - Python: `make lint-py`
  - Shell: `make lint-sh`
  - Org mode: `make lint-org`
  - Emacs Lisp: `make lint-el`
- Format all code: `make format`
- Format specific types:
  - Python: `make format-py`
  - Shell: `make format-sh`

## Docker Commands
- Build all containers: `docker-compose build`
- Run Jupyter notebook server: `docker-compose up notebook`
- Run API service: `docker-compose up api`
- Run all services: `docker-compose up`

## Org-mode Tangling
- Tangle specific file: `make tangle FILE=path/to/file.org`
- Tangle all org files: `make tangle-all`

## Common Makefile Commands
Run `make` without arguments to see available commands:
- `make help` - Show command help
- `make clean` - Remove build artifacts and cache files
- `make install-dev-tools` - Install development tools
- `make check-tools` - Check if required tools are installed