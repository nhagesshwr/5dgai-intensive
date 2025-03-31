# Makefile with File-based Dependencies for Org-mode Tangling

## Description
Implemented a file-based dependency system in the Makefile to efficiently manage the tangling of Org-mode files to source code. This system ensures that source files are only regenerated when their Org files change, improving build efficiency.

## Key Features
- Pattern rule `%.py %.hy: %.org` creates automatic dependencies
- Timestamp-based checking ensures efficient builds
- Integration with `scripts/tangle-org.sh` and `scripts/tangle-all.sh`
- Added new `build` target for full dependency-aware build
- Updated documentation in README.org and DEVELOPMENT.org

## Testing
- Verified all Makefile commands work correctly
- Tested dependency tracking with source file generation
- Ensured scripts have proper execution permissions

## Remaining Work
- Consider integrating with Hatch build system (issue #5)
- Add CI integration to validate tangling on commits

Closes #4
