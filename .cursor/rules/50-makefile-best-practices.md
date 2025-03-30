# Makefile Best Practices

## Core Principles
- Focus on file dependencies to leverage Make's DAG (Directed Acyclic Graph)
- Avoid complex shell scripting inside targets
- Mark targets that don't produce files as `.PHONY`
- Minimize conditionals and loops in recipes

## File Dependencies
Makefiles excel at tracking file-level dependencies:

```makefile
output.pdf: report.tex bibliography.bib
    pdflatex report.tex
```

This allows Make to determine which targets need rebuilding based on file timestamps.

## Pattern Rules
Use pattern rules for similar artifacts:

```makefile
%.py: %.org
    emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$<\")"
```

## Clean Target Implementation
Keep targets focused on their primary operations:

```makefile
# Good
compile: source.c header.h
    $(CC) $(CFLAGS) -o output source.c

# Avoid
compile:
    if [ -f source.c ]; then \
        $(CC) $(CFLAGS) -o output source.c; \
    fi
```

## Target Organization
- Group related targets with clear naming
- Order targets from high-level to low-level
- Document target relationships in comments
- Default target should be help or overview

## Proper DAG for File Processing
For projects with org files tangling to implementation:

```makefile
# Build Python files from Org files
src/%.py: org/%.org
    emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$<\")"

# All source files
SOURCES = $(wildcard org/*.org)
# All derived Python files
PY_FILES = $(patsubst org/%.org,src/%.py,$(SOURCES))

# Build all Python files
build: $(PY_FILES)
```