# Urology Trials

This repository contains a set of R scripts designed to download, clean, and summarize ongoing clinical trials in **urology** from ClinicalTrials.gov and related sources.  

The goal is to build reproducible pipelines for:  
- Downloading raw trial data  
- Cleaning and harmonizing relevant fields (phase, interventions, outcomes, sponsors, etc.)  
- Generating summary datasets that can be further explored  

The long-term plan is to integrate these summaries into an **R Shiny application** for interactive visualization of the urology clinical trial landscape.  

---

## Current scripts
- **download_trials.R**: Retrieves trial data from ClinicalTrials.gov.  
- **clean_trials.R**: Cleans and standardizes trial fields for analysis.  

## Next steps
- Add exploratory notebooks/analyses  
- Develop R Shiny dashboards for visualization  

