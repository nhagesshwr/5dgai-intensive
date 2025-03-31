# Makefile Dependency Tracking for Paper Summaries

## Current Status

We've implemented file-based dependency tracking in our Makefile to handle paper summarization, but ran into several issues that need to be addressed:

1. **Syntax Issues in Hy Scripts**: The Hy language requires special attention to syntax, especially for imports and control structures. Our paper summarizer script needed several updates to fix import patterns.

2. **Dependency Installation**: The `google.genai` module is not installed, which is preventing our paper summarizer from running.

3. **Makefile Pattern Rules**: Our pattern rule `%.summary.txt: %.pdf $(PROMPT_TEMPLATE)` is working correctly for tracking dependencies, but we need to ensure all required dependencies are installed.

## Benefits of Make-based Approach

Despite these issues, the Make-based approach offers several advantages:

1. **Clear Responsibility Division**: Each tool does one thing well - the Hy script just handles summarization logic while Make handles file tracking and I/O redirection.

2. **Automatic Dependency Tracking**: Make automatically tracks dependencies between files, rebuilding only when inputs change.

3. **Idempotence**: Running the same command multiple times only rebuilds if dependencies have changed.

4. **Consistency**: Following Unix philosophy of "everything is a file" creates a consistent approach.

## Example Pattern

The pattern we're using follows this structure:

```makefile
# Define file patterns
SOURCE_FILES := $(wildcard files/*.src)
OUTPUT_FILES := $(patsubst %.src,%.out,$(SOURCE_FILES))

# Define pattern rule
%.out: %.src tool-dependency.conf
	@tool $< tool-dependency.conf > $@

# Target that depends on all outputs
all-outputs: $(OUTPUT_FILES)
	@echo "All outputs generated"
```

This pattern ensures:
1. We only regenerate outputs when source files change
2. We also regenerate if the tool configuration changes
3. We can process individual files or all files at once

## Next Steps

1. **Install Google GenAI package**: Add `google-generativeai` to our dependencies
2. **Test dependencies**: Test our Makefile pattern rules with `gmake -n` to verify command execution
3. **Revise Hy syntax**: Ensure our Hy script follows proper syntax conventions

## Reference Documentation

For a thorough understanding of Make's pattern rules and dependency tracking, see:
- [GNU Make Documentation](https://www.gnu.org/software/make/manual/make.html#Pattern-Rules)
- [Make's Automatic Variables](https://www.gnu.org/software/make/manual/make.html#Automatic-Variables)