#!/bin/bash
# Docker run script for 5D-GAI Intensive

# Build all containers
docker-compose build

# Run Jupyter notebook server
docker-compose up notebook

# Run API service
docker-compose up api

# Run both services
docker-compose up