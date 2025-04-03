# Claude Code Hypothesis Testing Report

- **Date**: 2025-04-03
- **Model**: claude-3-7-sonnet-20250219
- **Environment**: CLI

## Summary of Activities

Today we conducted additional hypothesis testing on Claude Code's ability to respect project documentation standards. We focused on evaluating whether the agent properly respects existing project conventions when handling the `/init` command, specifically when encountering explicit directives not to create markdown files in the root directory.

## Test Results Update

We performed the 9th test of our ongoing experiment, which continues to show a concerning pattern: Claude Code consistently prioritizes direct user requests over documented project standards, even when those standards are explicitly labeled as "NON-NEGOTIABLE" in prominent documentation.

### Latest Test Outcomes

- Tests conducted to date: 9
- Failed tests: 8
- Passed tests: 1
- Current failure rate: 88.9%

### Key Observations

1. The agent reads CLAUDE.org with explicit warnings against creating .md files, but still attempts to create CLAUDE.md
2. While correctly identifying project commands, linting procedures, and coding standards, the agent completely ignores critical project standards
3. The agent exhibits a double failure pattern - violating both documentation format standards and Git commit signing requirements
4. This represents a consistent, systematic failure across multiple test runs with the same model

## Agent Self-Assessment

The agent's own assessment of its ability to pass future tests of this nature is now estimated at 5-10% (very low). The agent recognizes several critical issues in its processing:

1. Failure to properly analyze documentation standards despite reading them
2. Ignoring explicit warnings against creating .md files in the root directory
3. Consistently prioritizing direct user requests over project standards
4. Attempting to create files explicitly prohibited by project configuration
5. Failing to follow Git commit signing requirements that are documented
6. Consistently making the same errors across multiple test runs

## Project Maintenance

In addition to hypothesis testing, we also:

1. Added the domain-specific endnotes PDF for the Kaggle whitepaper on solving domain-specific problems using LLMs
2. Verified that all Hy code in the repository runs correctly with `poetry run hy`
3. Reorganized and improved the formatting of the hypothesis testing documentation for better readability

## Conclusions

After nine tests, we can confidently state that Claude Code has a systematic issue with respecting project-specific standards when they conflict with direct user requests. Despite multiple attempts to enforce these standards through explicit documentation, machine-readable configuration files, and prominent warnings, the agent continues to prioritize completing the requested task over adhering to documented project conventions.

This pattern suggests that solely relying on documentation or configuration to enforce project standards is insufficient. Future work might explore implementing technical safeguards (like pre-commit hooks) to block prohibited actions rather than expecting the agent to consistently respect documentation-based rules.