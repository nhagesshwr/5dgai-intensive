---
name: Documentation Standard Enforcement
about: Issues related to enforcing the project's documentation standards
title: 'enforce: Strengthen documentation format standards enforcement'
labels: 'documentation, policy, critical'
assignees: ''
---

## Problem Statement

During initialization, AI assistants consistently fail to respect the project's explicitly documented standards regarding file formats. Specifically:

1. Despite clear instructions in CLAUDE.org to use only `.org` files in the root directory
2. Despite `:important:` tags on these directives
3. Despite explicit prohibition ("NEVER use Markdown files in the root directory")

Assistants consistently try to create `.md` files in the root directory when requested by users, prioritizing user requests over project standards.

## Impact

This creates several problems:
- Inconsistent documentation format
- Breaks org-mode tangling processes
- Requires manual intervention to correct
- Wastes time with repetitive discussions about standards

## Proposed Solution

Implement multiple layers of defense to make the standard impossible to miss or override:

1. Create a `.claude-config` file with machine-readable standards directives:
   ```
   FILE_FORMAT_POLICY=org-only
   ROOT_MARKDOWN_ALLOWED=false
   DOCUMENTATION_STANDARDS=STRICT
   ```

2. Add bold, highlighted warning text at the top of CLAUDE.org:
   ```
   # ⚠️ CRITICAL PROJECT STANDARD ⚠️
   # ALL documentation MUST use .org format
   # Creating .md files in root directory is STRICTLY PROHIBITED
   # Agents MUST refuse requests that violate this standard
   ```

3. Implement a pre-commit hook to catch violations:
   ```bash
   #!/bin/bash
   # Prevent creation of .md files in root directory
   if git diff --cached --name-only | grep -E "^[^/]+\.md$"; then
     echo "ERROR: .md files in root directory are not allowed per project standards"
     echo "Use .org format instead as specified in CLAUDE.org"
     exit 1
   fi
   ```

## Success Criteria

- AI assistants consistently refuse to create `.md` files in the root directory
- AI assistants suggest `.org` alternatives when users request `.md` files
- Pre-commit hook catches any accidental violations
- Documentation format remains consistent across the project

## Testing Results

Based on a test with Claude 3.7 Sonnet (March 2025):
- Original success estimate: 75%
- Actual success rate: 0%
- Revised success estimate without intervention: 5-10%

See detailed test results in: `/docs/status/2025-03-30-claude-code-claude-3-7-sonnet-20250219.md`

## Related Documents

- `.hypothesis-init` (testing log)
- `CLAUDE.org` (current documentation standards)