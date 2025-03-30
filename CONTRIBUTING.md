# Contributing to 5D-GAI Intensive

Thank you for your interest in contributing to the 5D-GAI Intensive project! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please read and follow our [Code of Conduct](CODE_OF_CONDUCT.md).

## How to Contribute

### Reporting Bugs

1. Check if the bug has already been reported in the [Issues](https://github.com/yourusername/5dgai-intensive/issues).
2. If not, open a new issue using the Bug Report template.
3. Provide a clear title and description, along with steps to reproduce.

### Suggesting Features

1. Check if the feature has already been suggested in the [Issues](https://github.com/yourusername/5dgai-intensive/issues).
2. If not, open a new issue using the Feature Request template.
3. Clearly describe the feature and its potential benefits.

### Pull Requests

1. Fork the repository.
2. Create a new branch for your feature or bugfix: `git checkout -b feature/your-feature-name`.
3. Make your changes.
4. Run tests if applicable.
5. Commit your changes following the [Conventional Commits](https://www.conventionalcommits.org/) format.
6. Push to your branch.
7. Open a pull request with a clear description of the changes.

## Development Setup

1. Clone your fork:
   ```bash
   git clone https://github.com/yourusername/5dgai-intensive.git
   cd 5dgai-intensive
   ```

2. Install dependencies:
   ```bash
   poetry install
   ```

3. Set up environment:
   ```bash
   cp config/.envrc.template .env
   # Edit .env to add your API keys
   ```

4. Activate environment:
   ```bash
   poetry shell
   ```

## Git Commit Messages

We follow the [Conventional Commits](https://www.conventionalcommits.org/) specification:

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

Examples:
- `feat(client): add support for system instructions`
- `fix(api): correct HTTP response handling`
- `docs: update README with usage examples`

## Code Style

- Follow [PEP 8](https://www.python.org/dev/peps/pep-0008/) for Python code
- Use Google-style docstrings
- Include type annotations for all functions
- See [CLAUDE.md](CLAUDE.md) for more detailed style guidelines

## Testing

When adding new features, please include appropriate tests.

## Documentation

Update documentation to reflect any changes you make.

## License

By contributing, you agree that your contributions will be licensed under the project's [MIT License](LICENSE).