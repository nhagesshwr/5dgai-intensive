# Python Code Style Standard

## Type Annotations
- Required for ALL functions and methods
- Use from `typing`: Dict, List, Optional, Union, Any, etc.
- Example:
  ```python
  from typing import Dict, List, Any, Optional
  
  def process_data(input_data: List[Dict[str, Any]], 
                  limit: Optional[int] = None) -> Dict[str, Any]:
      """Process the input data and return results."""
      pass
  ```

## Docstrings
- Use Google-style docstrings with Args/Returns/Raises sections
- Example:
  ```python
  def calculate_score(values: List[float], 
                     weights: Optional[List[float]] = None) -> float:
      """Calculate weighted score from provided values.
      
      Args:
          values: List of float values to score
          weights: Optional weights to apply (defaults to equal weighting)
          
      Returns:
          Float representing the calculated score
          
      Raises:
          ValueError: If values is empty or weights length doesn't match values
      """
      pass
  ```

## Imports
- Group imports with a blank line between groups:
  1. Standard library
  2. Third-party libraries
  3. Local modules
- Example:
  ```python
  # Standard library
  import os
  import json
  from typing import Dict, List, Any
  
  # Third-party libraries
  import numpy as np
  import requests
  
  # Local modules
  from src.utils import format_response
  ```

## Error Handling
- Use specific exceptions and proper context managers
- Example:
  ```python
  try:
      response = requests.get(url, timeout=5)
      response.raise_for_status()
      return response.json()
  except requests.exceptions.HTTPError as err:
      logger.error(f"HTTP error: {err}")
      raise
  ```

## Naming Conventions
- Variables and functions: `snake_case`
- Classes: `PascalCase`
- Constants: `UPPER_CASE`
- Private methods/variables: `_leading_underscore`

## Formatting
- 4-space indentation
- 100 character line limit
- Black and isort for formatting