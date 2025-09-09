# Urology Trials

This repository contains R scripts and notebooks to **download, clean, and analyze** clinical trial data in **urology** from ClinicalTrials.gov.  

The aim is to build reproducible pipelines for:  
- Downloading raw trial data.  
- Cleaning and harmonizing relevant fields (phase, interventions, outcomes, sponsors, etc...).  
- Generating summary datasets and exploratory reports.  
- Preparing for integration into an **R Shiny application** for interactive visualization of the urology clinical trial landscape.  

---

## Current components

- **download_trials.R** — Retrieves trial data from ClinicalTrials.gov  
- **clean_trials.R** — Cleans and standardizes trial fields for analysis  
- **exploratory_analysis.Rmd** — R Markdown notebook to generate descriptive statistics and plots (HTML/PDF reports)  

---

## Next steps
- Develop interactive R Shiny dashboards.
