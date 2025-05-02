#\!/bin/bash
# Check for potential API keys in the codebase

echo "=== Running API Key Check ==="
echo "Looking for hardcoded API keys or tokens..."

# Check for potential API keys or tokens in files
grep -r "api[_-]key.*=[\"\']" --include="*.py" --include="*.hy" --include="*.js" --include="*.json" --include="*.sh" . | grep -v "os.getenv\|getenv" && echo "⚠️ Potential hardcoded API keys found" || echo "✅ No hardcoded API keys detected"

# Check for .env files that might be accidentally committed
find . -name ".env" -not -path "*/\.*" && echo "⚠️ .env files found - check that they don't contain real secrets" || echo "✅ No .env files found"

# Check for suspicious patterns
echo "Looking for suspicious patterns that might be API keys..."
grep -r "AIza[0-9A-Za-z-_]{35}" . && echo "⚠️ Potential Google API key found" || echo "✅ No Google API key pattern found"
grep -r "sk-[0-9a-zA-Z]{48}" . && echo "⚠️ Potential OpenAI API key found" || echo "✅ No OpenAI API key pattern found"
grep -r "ya29\.[0-9a-zA-Z_-]{68,}" . && echo "⚠️ Potential Google OAuth token found" || echo "✅ No Google OAuth token pattern found"

echo "=== Check completed ==="
