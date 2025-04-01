# Pre-commit Hook Implementation Issue

## Summary

A Google API key was recently exposed in a commit, indicating that our planned pre-commit hooks for security scanning weren't properly implemented or working as expected. This issue outlines the steps to implement robust pre-commit hooks to prevent future credential exposure.

## Problem Description

On March 31, 2025, a Google API key was committed in the `.emacs.d/setup-init.org` file and remained there until detected by Google's automated scanning approximately 2 hours later. Our development checklist indicates pre-commit hooks should be implemented, but they were either:

1. Not implemented at all (no `.pre-commit-config.yaml` found)
2. Not configured to scan for API keys and credentials
3. Implemented but not installed in the local git hooks directory

This security lapse could have resulted in significant security issues if the key had been used for malicious purposes before detection.

## Required Implementation

### 1. Install Pre-commit Framework

```bash
pip install pre-commit
```

### 2. Create Pre-commit Configuration

Create a `.pre-commit-config.yaml` file in the repository root:

```yaml
repos:
-   repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.5.0
    hooks:
    -   id: trailing-whitespace
    -   id: end-of-file-fixer
    -   id: check-yaml
    -   id: check-added-large-files
    -   id: check-json
    -   id: check-toml

-   repo: https://github.com/gitleaks/gitleaks
    rev: v8.18.1
    hooks:
    -   id: gitleaks

-   repo: https://github.com/zricethezav/detect-secrets
    rev: v1.4.0
    hooks:
    -   id: detect-secrets
        args: ['--baseline', '.secrets.baseline']

-   repo: https://github.com/jumanjihouse/pre-commit-hooks
    rev: 3.0.0
    hooks:
    -   id: git-check  # Configure in .gitattributes
    -   id: forbid-binary
        exclude: (\.png$|\.jpg$|\.gif$|\.webp$|\.ico$)
```

### 3. Create a Secrets Baseline

```bash
detect-secrets scan > .secrets.baseline
```

### 4. Add Specific Patterns to Catch Google API Keys

Create a `.gitleaks.toml` configuration file:

```toml
[allowlist]
description = "Global Allowlist"
paths = [
    '''node_modules''',
    '''\.venv''',
    '''\.git''',
    '''\.pytest_cache''',
]

[[rules]]
id = "google-api-key"
description = "Google API Key"
regex = '''AIza[0-9A-Za-z\\-_]{35}'''
secret-group = 1
tags = ["key", "google"]

[[rules]]
id = "google-oauth-id"
description = "Google OAuth ID"
regex = '''[0-9]+-[0-9A-Za-z_]{32}\.apps\.googleusercontent\.com'''
secret-group = 1
tags = ["key", "google"]
```

### 5. Integrate with Makefile

Update the Makefile to include pre-commit setup:

```makefile
install-pre-commit:
	@echo "${BLUE}Installing pre-commit hooks...${RESET}"
	@pip install pre-commit
	@pre-commit install
	@pre-commit install --hook-type commit-msg
	@echo "${GREEN}Pre-commit hooks installed!${RESET}"
```

### 6. Verification Steps

After implementing these changes:

1. Install the hooks:
   ```bash
   make install-pre-commit
   ```

2. Test with a deliberate API key insertion:
   ```bash
   echo "API_KEY='AIzaSomeRandomKeyForTestingPurposes123'" > test_file.txt
   git add test_file.txt
   git commit -m "test: verify pre-commit hooks block API keys"
   ```

3. The commit should be blocked with an appropriate error message.

4. Clean up:
   ```bash
   git reset --hard HEAD
   ```

## Automated Testing

Add a CI step to verify that pre-commit hooks are properly configured and that secret scanning is working:

```yaml
pre-commit-checks:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - uses: actions/setup-python@v4
      with:
        python-version: '3.10'
    - name: Install pre-commit
      run: pip install pre-commit
    - name: Run pre-commit checks
      run: pre-commit run --all-files
```

## Related Issues

- Linked to SECURITY-ISSUE.md (API key exposure)
- Mentioned as a requirement in CHECKLIST.org

## Completion Criteria

- [ ] Pre-commit hooks installed and configured
- [ ] Secret scanning rules properly set up
- [ ] Tests verify keys are blocked
- [ ] Documentation updated with instructions for developers
- [ ] CI pipeline includes verification step
- [ ] CHECKLIST.org updated to mark pre-commit hooks as implemented

## Priority

**High** - This is a critical security measure that should be implemented immediately to prevent future credential exposure.