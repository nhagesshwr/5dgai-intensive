---
name: Migrate Python Code to Hy
about: Migration plan for transitioning from Python to Hy (Lisp dialect for Python)
title: "🔄 Migrate Python codebase to Hy"
labels: enhancement, refactoring
assignees: jwalsh
---

# Migrate Python Codebase to Hy

## Overview

The repository should be migrated from Python to Hy (Lisp dialect that runs on the Python runtime) to enable more elegant Lisp-based development while maintaining Python library compatibility.

## Background

Hy is a Lisp dialect that embeds in Python. It supports all Python libraries and has full interoperability while providing the elegance and expressiveness of Lisp. This aligns with the repository's use of Org-mode and structured programming approaches.

## Affected Files

The following Python files need to be converted to Hy:

- `src/gemini_client.py`
- Any Python scripts in `examples/`
- Test files

## Implementation Plan

1. **Phase 1: Setup and Environment**
   - [x] Add Hy to dependencies (`hy = "^1.0"`) ✅
   - [ ] Create template for Hy file structure
   - [ ] Define macros for common patterns

2. **Phase 2: Core Migration**
   - [ ] Convert `gemini_client.py` to `gemini_client.hy`
   - [ ] Update imports in dependent files
   - [ ] Ensure API compatibility is maintained

3. **Phase 3: Testing**
   - [ ] Convert test files to Hy
   - [ ] Implement test helpers
   - [ ] Verify all tests pass with Hy implementation

4. **Phase 4: Documentation & Examples**
   - [ ] Update documentation with Hy examples
   - [ ] Create conversion guide for contributors
   - [ ] Add Hy-specific section to `DEVELOPMENT.org`

## Implementation Details

### Syntax Conversion Guide

**Python code:**
```python
def generate_content(self, prompt, model="gemini-2.0-flash", system_instruction=None):
    url = f"{self.base_url}/models/{model}:generateContent?key={self.api_key}"
    payload = {
        "contents": [{
            "parts": [{"text": prompt}]
        }]
    }
    # ...
```

**Hy equivalent:**
```hy
(defn generate-content [self prompt &optional [model "gemini-2.0-flash"] [system-instruction None]]
  (setv url f"{self.base-url}/models/{model}:generateContent?key={self.api-key}")
  (setv payload 
    {"contents" 
      [{"parts" 
        [{"text" prompt}]}]})
  #...)
```

### Import Conversion Guide

**Python:**
```python
import os
import json
import requests
from dotenv import load_dotenv
from typing import Dict, List, Any, Optional
```

**Hy:**
```hy
(import os
        json
        requests
        [dotenv [load-dotenv]]
        [typing [Dict List Any Optional]])
```

## Testing Strategy

1. Unit tests for both Python and Hy implementations
2. Parallel implementations during transition
3. Integration tests using both versions

## Success Criteria

- [ ] All Python functionality replicated in Hy
- [ ] All tests passing with Hy implementation
- [ ] No regressions in API behavior
- [ ] Documentation updated with Hy examples
- [ ] Performance benchmarks show comparable results

## Benefits

- More elegant code structure
- Better alignment with functional programming paradigms
- Lisp-style macros for metaprogramming
- Consistency with Org-mode structured approach
- Maintain Python library compatibility