#!/bin/bash
# Setup script for 5D-GAI Intensive course environment
# This script sets up your development environment and project structure

set -euo pipefail

# Directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"

# Echo with color
info() { echo -e "\033[0;34m$1\033[0m"; }
success() { echo -e "\033[0;32m$1\033[0m"; }
warning() { echo -e "\033[0;33m$1\033[0m"; }
error() { echo -e "\033[0;31m$1\033[0m"; }

info "Setting up 5D-GAI Intensive environment..."

# Setup git hooks
if [ -d "$SCRIPT_DIR/.git" ]; then
    info "Setting up git hooks..."
    git config core.hooksPath .githooks
    success "Git hooks configured."
else
    warning "Not a git repository. Skipping git hooks setup."
fi

# Check if Poetry is installed
if ! command -v poetry &>/dev/null; then
    warning "Poetry not found. Installing..."
    curl -sSL https://install.python-poetry.org | python3 -
    export PATH="$HOME/.poetry/bin:$PATH"
fi

# Install dependencies
info "Installing Python dependencies with Poetry..."
poetry install

# Create project directories
info "Creating project structure..."
mkdir -p notebooks/{day1,day2,day3,day4,day5}
mkdir -p papers/{day1,day2,day3,day4,day5}
mkdir -p resources data config

# Create .env file if it doesn't exist
if [ ! -f "$SCRIPT_DIR/.env" ]; then
    info "Creating .env file..."
    cat >"$SCRIPT_DIR/.env" <<EOF
# API Keys - Required for course
AI_STUDIO_API_KEY=""
KAGGLE_USERNAME=""
KAGGLE_KEY=""
OPENAI_API_KEY=""
ANTHROPIC_API_KEY=""
EOF
    success "Created .env file template."
    warning "Please edit .env and add your API keys."
else
    success ".env file already exists."
fi

# Verify documentation standards
info "Checking documentation standards enforcement..."
if [ -f "$SCRIPT_DIR/.claude-config" ]; then
    success "Documentation standards configuration found."
else
    warning "Creating documentation standards configuration..."
    cat >"$SCRIPT_DIR/.claude-config" <<EOF
# Machine-readable configuration for AI assistants
# DO NOT MODIFY WITHOUT TEAM APPROVAL

# Documentation Standards
FILE_FORMAT_POLICY=org-only
ROOT_MARKDOWN_ALLOWED=false
DOCUMENTATION_STANDARDS=STRICT

# Critical Rules (agents MUST follow these)
MUST_REJECT_ROOT_MD_FILES=true
MUST_SUGGEST_ORG_ALTERNATIVES=true
MUST_PRESERVE_EXISTING_CONVENTIONS=true

# Warning Levels
MARKDOWN_WARNING_LEVEL=CRITICAL
CONVENTION_OVERRIDE_WARNING_LEVEL=CRITICAL

# Repository History
POLICY_ESTABLISHED=2025-03-30
POLICY_VIOLATIONS=1
POLICY_ENFORCEMENT_MECHANISM=multiple
EOF
    success "Documentation standards configuration created."
fi

success "Setup complete! Next steps:"
echo "1. Edit .env file and add your API keys"
echo "2. Run 'poetry shell' to activate the environment"
echo "3. Start exploring with 'jupyter notebook'"
