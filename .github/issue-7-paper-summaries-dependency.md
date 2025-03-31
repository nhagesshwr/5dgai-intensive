# Implement Paper Summaries with File-based Dependency Checking

## Description
Add a feature to generate paper summaries using Gemini API with proper file-based dependency checking in the Makefile. This ensures summaries are only regenerated when either the source PDF or the prompt template changes.

## Implementation Requirements
- Create a pattern rule `%.summary.txt: %.pdf prompts/summarize-paper.md` in the Makefile
- Implement a Hy script that uses Gemini API to summarize PDFs
- Support both individual paper summary generation and batch processing
- Ensure proper dependency checking (idempotent operation)

## Validation Criteria
1. Running `gmake papers/foo.summary.txt` should generate the summary
2. Running the same command again should not regenerate the summary (Make should report "Nothing to be done")
3. If the prompt template in `prompts/summarize-paper.md` is modified, running the command should regenerate the summary
4. If the PDF is updated, running the command should regenerate the summary

## Example Usage
```bash
# Generate a single paper summary
gmake papers/deepmind-2023-gemini-report.summary.txt

# Generate all paper summaries
gmake paper-summaries
```

## Acceptance Tests
1. First run: `gmake papers/deepmind-2023-gemini-report.summary.txt` → Should generate summary
2. Second run (no changes): `gmake papers/deepmind-2023-gemini-report.summary.txt` → Should show "up to date"
3. Modify prompt: `touch prompts/summarize-paper.md` → Next run should regenerate summary
4. Modify paper: `touch papers/deepmind-2023-gemini-report.pdf` → Next run should regenerate summary

This issue implements a typical Make pattern of only rebuilding outputs when inputs have changed, applied to AI-based paper summarization.