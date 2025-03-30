# Project Structure

```mermaid
C4Context
  title 5-Day Gen AI Intensive - Project Structure

  Enterprise_Boundary(b0, "5-Day Gen AI Intensive") {
    System(notebooks, "Notebooks", "Jupyter notebooks for daily exercises")
    System(papers, "Papers", "Research papers and references")
    System(src, "Source Code", "Python modules and utilities")
    System(api, "API", "API service for model endpoints")
    System(config, "Config", "Configuration files and templates")
    System(podcasts, "Podcasts", "Audio content and notes")
    
    Boundary(infra, "Infrastructure") {
      System_Ext(docker, "Docker", "Containerized environment")
      System_Ext(poetry, "Poetry", "Python dependency management")
    }
    
    Boundary(resources, "Resources") {
      System_Ext(kaggle, "Kaggle", "Competition platform")
      System_Ext(aiStudio, "Google AI Studio", "Model APIs")
      System_Ext(discord, "Discord", "Community discussions")
    }
    
    BiRel(notebooks, src, "Uses")
    Rel(notebooks, papers, "References")
    Rel(src, api, "Implements")
    Rel(src, config, "Uses configuration from")
    Rel(infra, notebooks, "Hosts")
    Rel(infra, api, "Runs")
    Rel(notebooks, resources, "Connects to")
    Rel(api, resources, "Integrates with")
    Rel(podcasts, notebooks, "Provides content for")
  }
```
