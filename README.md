# Urology Trials

This repository contains R scripts, notebooks, and a Shiny app to **download, clean, explore, and visualize** clinical trial data in **urology** from ClinicalTrials.gov.  

The goal is to provide reproducible pipelines for:  
- Downloading raw trial data  
- Cleaning and harmonizing key fields (phase, interventions, sponsors, dates, etc.)  
- Producing summary datasets and exploratory reports  
- Visualizing results through an interactive **R Shiny dashboard**  

---

## Current components

- **download_trials.R** — Retrieves trial data from ClinicalTrials.gov  
- **clean_trials.R** — Cleans and standardizes trial fields for analysis  
- **exploratory_analysis.Rmd** — R Markdown notebook to generate descriptive statistics and plots (HTML/PDF reports)  
- **app.R** — Shiny dashboard for interactive exploration of urology clinical trials  

---

## Next steps
- Extend the Shiny dashboard with additional views and filters  
- Automate regular data refresh and report generation  