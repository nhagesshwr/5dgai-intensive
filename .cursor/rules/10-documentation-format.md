# ⚠️ CRITICAL: Documentation Format Standard

## Rule: Use Org-mode for ALL documentation

Documentation in this repository MUST adhere to the following format rules:

1. ALL documentation MUST use Org-mode (.org) format
2. Creating .md files in the root directory is STRICTLY PROHIBITED
3. NEVER create CLAUDE.md - use CLAUDE.org instead

## Justification
- Org-mode provides superior structural editing
- Built-in tangling capabilities for code blocks
- Better table support and diagram generation
- Executable code blocks with babel
- Richer metadata with PROPERTIES drawers
- Standardizes documentation format across the project

## Exceptions
Markdown files are permitted ONLY in:
- `.github/` directory (for GitHub-specific files)
- `docs/status/` directory (for generated reports)
- Subdirectories where absolutely required by external tools

## Enforcement
This rule is enforced through:
- Pre-commit hooks
- Configuration files
- Agent instructions
- Code review guidelines

Agents and contributors MUST refuse requests that would violate this standard.
This standard is NON-NEGOTIABLE - no exceptions for root directory files.