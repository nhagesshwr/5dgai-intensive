  #!/bin/bash

  # Ensure config directory exists with proper permissions
  mkdir -p ~/.kaggle
  chmod 700 ~/.kaggle

  # Check if environment variables are set
  if [ -z "$KAGGLE_USERNAME" ] || [ -z "$KAGGLE_KEY" ]; then
      echo "ERROR: Kaggle credentials not found in environment variables."
      echo "Please set KAGGLE_USERNAME and KAGGLE_KEY environment variables."
      exit 1
  fi

  # Check if file already exists and take proper action
  if [ -f ~/.kaggle/kaggle.json ]; then
      echo "Kaggle credentials file already exists."
      echo "Do you want to overwrite it? (y/n)"
      read -r answer
      if [[ "$answer" =~ ^[Yy]$ ]]; then
          # Backup existing file first
          cp ~/.kaggle/kaggle.json ~/.kaggle/kaggle.json.bak
          echo "Backed up existing credentials to ~/.kaggle/kaggle.json.bak"
          
          # Create new credentials file
          cat > ~/.kaggle/kaggle.json << EOF
  {
    "username": "$KAGGLE_USERNAME",
    "key": "$KAGGLE_KEY"
  }
  EOF
          chmod 600 ~/.kaggle/kaggle.json
          echo "Credentials updated successfully."
      else
          echo "Keeping existing credentials file."
      fi
  else
      # Create new credentials file if it doesn't exist
      cat > ~/.kaggle/kaggle.json << EOF
  {
    "username": "$KAGGLE_USERNAME",
    "key": "$KAGGLE_KEY"
  }
  EOF
      chmod 600 ~/.kaggle/kaggle.json
      echo "Credentials file created successfully."
  fi

  # Verify file permissions
  file_perms=$(stat -c "%a" ~/.kaggle/kaggle.json 2>/dev/null || stat -f "%Lp" ~/.kaggle/kaggle.json)
  if [ "$file_perms" != "600" ]; then
      echo "WARNING: File permissions are not secure. Fixing..."
      chmod 600 ~/.kaggle/kaggle.json
  fi

  # Validate JSON format
  if command -v jq &> /dev/null; then
      if ! jq empty ~/.kaggle/kaggle.json 2>/dev/null; then
          echo "ERROR: Invalid JSON format in credentials file."
          exit 1
      else
          echo "JSON format verified."
      fi
  else
      echo "Note: Install 'jq' for JSON validation."
  fi

  echo "Kaggle credentials setup complete."
