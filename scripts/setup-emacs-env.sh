#!/usr/bin/env bash
# Setup script for Emacs environment for 5D-GAI Intensive
# This script installs necessary Emacs packages and configures the environment

set -euo pipefail

echo "Setting up Emacs environment for 5D-GAI Intensive course..."

# Install required system packages
if command -v apt-get >/dev/null; then
    sudo apt-get update
    sudo apt-get install -y emacs plantuml graphviz
elif command -v brew >/dev/null; then
    brew install emacs plantuml graphviz
else
    echo "Unable to determine package manager. Please install Emacs, PlantUML, and Graphviz manually."
fi

# Create necessary directories
mkdir -p .emacs.d/straight/repos/
mkdir -p images
mkdir -p docs/images

# Install straight.el if not present
if [ ! -f "$HOME/.emacs.d/straight/repos/straight.el/straight.el" ]; then
    mkdir -p "$HOME/.emacs.d/straight/repos/"
    git clone https://github.com/raxod502/straight.el.git "$HOME/.emacs.d/straight/repos/straight.el"
fi

# Generate example PlantUML diagram
cat >docs/example-diagram.org <<'EOF'
#+TITLE: Example Diagram
#+AUTHOR: Jason Walsh
#+EMAIL: j@wal.sh

* Example PlantUML Diagram

#+begin_src plantuml :file images/example-diagram.png
@startuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml

title 5-Day Gen AI Intensive - Architecture

Person(user, "User", "Course participant")

System_Boundary(genai, "5D-GAI Intensive") {
  Container(jupyter, "Jupyter Notebooks", "Python, Jupyter", "Interactive data analysis and experimentation")
  Container(gemini, "Gemini API Client", "Python", "Interfaces with Google's Gemini models")
  Container(data, "Data Pipeline", "Python, pandas", "Processes data for model input")
  ContainerDb(storage, "Data Storage", "Files, SQLite", "Stores data and outputs")
}

System_Ext(google_ai, "Google AI Platform", "Gemini models and API services")

Rel(user, jupyter, "Uses for experiments")
Rel(jupyter, gemini, "Calls")
Rel(jupyter, data, "Processes data with")
Rel(gemini, google_ai, "Makes API calls to")
Rel(data, storage, "Reads/writes")

@enduml
#+end_src

Here is the rendered diagram:

#+CAPTION: Architecture Diagram
#+ATTR_HTML: :width 800px
[[file:images/example-diagram.png]]
EOF

# Create Emacs package installation script
cat >scripts/install-emacs-packages.el <<'EOF'
;; Script to install required Emacs packages for 5D-GAI Intensive

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Define packages to install
(setq my-packages
      '(org
        org-bullets
        ox-hugo
        ox-reveal
        jupyter
        ein
        org-roam
        smartparens
        flycheck
        company
        elpy
        restclient
        plantuml-mode
        yaml-mode
        magit
        projectile))

;; Install packages
(require 'use-package)
(setq use-package-always-ensure t)

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(message "All packages installed successfully!")
EOF

echo "Setup complete!"
echo "To install required Emacs packages, run: emacs --batch -l scripts/install-emacs-packages.el"
echo "To generate example diagram, open docs/example-diagram.org in Emacs and evaluate the PlantUML block"
