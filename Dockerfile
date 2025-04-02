FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    git \
    emacs \
    wget \
    make \
    && rm -rf /var/lib/apt/lists/*

# Install Poetry
RUN curl -sSL https://install.python-poetry.org | python3 -
ENV PATH="/root/.local/bin:$PATH"

# Copy poetry configuration files
COPY pyproject.toml poetry.lock* ./

# Configure poetry
RUN poetry config virtualenvs.create false

# Install dependencies (Hy-focused)
RUN poetry install --no-interaction --no-ansi

# Copy project files
COPY . .

# Set up Emacs with basic Hy mode support
RUN mkdir -p /root/.emacs.d/
RUN echo '(require (quote package))' > /root/.emacs.d/init.el && \
    echo '(add-to-list (quote package-archives) (quote ("melpa" . "https://melpa.org/packages/")) t)' >> /root/.emacs.d/init.el && \
    echo '(package-initialize)' >> /root/.emacs.d/init.el && \
    echo '(unless (package-installed-p (quote hy-mode)) (package-refresh-contents) (package-install (quote hy-mode)))' >> /root/.emacs.d/init.el && \
    echo '(require (quote hy-mode))' >> /root/.emacs.d/init.el

# Set environment variables
ENV PYTHONPATH=/app
ENV PYTHONUNBUFFERED=1

# Start an Emacs daemon by default
CMD ["emacs", "--daemon", "--no-window-system", "--eval", "(message \"Emacs server ready for Hy development\")"]
