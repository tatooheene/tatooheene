#!/bin/bash
set -e

echo "🚀 Setting up tatooheene package..."

# Fix stringi ICU compatibility by reinstalling from source
echo "🔧 Ensuring stringi is compiled against correct ICU version..."
Rscript -e "if ('stringi' %in% installed.packages()[,'Package']) { remove.packages('stringi') }; install.packages('stringi', repos='https://cloud.r-project.org', type='source')" > /dev/null 2>&1 || echo "⚠️  stringi installation skipped"

# Configure Git user from environment variables or local git config
if [ -n "$GIT_AUTHOR_NAME" ] && [ -n "$GIT_AUTHOR_EMAIL" ]; then
    echo "⚙️  Configuring Git user from environment..."
    git config --global user.name "$GIT_AUTHOR_NAME"
    git config --global user.email "$GIT_AUTHOR_EMAIL"
else
    echo "⚠️  Git user not configured. Please run:"
    echo "   git config --global user.name 'Your Name'"
    echo "   git config --global user.email 'your.email@example.com'"
fi

# Generate documentation (dependencies already installed in image)
echo "📝 Generating documentation..."
Rscript -e "devtools::document()"

echo "✅ Setup complete! You can now develop the tatooheene package."
echo ""
echo "Quick commands:"
echo "  devtools::check()     - Run R CMD check"
echo "  devtools::test()      - Run tests"
echo "  devtools::document()  - Update documentation"
echo "  devtools::load_all()  - Load package for testing"
