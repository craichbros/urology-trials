# Script: download_trials.R
# ---------------------------------------------------------------
# Script to query the ClinicalTrials.gov API v2 and download data
# for urology cancer trials (Phase II and Phase III only).
#
# Currently implemented: "prostate cancer"
# Easily extendable: kidney cancer, bladder cancer, testicular cancer...
# ---------------------------------------------------------------

library(httr)      
library(jsonlite)  
library(dplyr)     
library(readr)     

# ---------------------------------------------------------------
# 1. Define query parameters
# ---------------------------------------------------------------
condition <- "prostate cancer"    # main condition (expandable later)
page_size <- 1000                 # number of studies per request

# ---------------------------------------------------------------
# 2. Function to fetch one "page" of results
# ---------------------------------------------------------------
fetch_page <- function(expr, page_size = 100, page_token = NULL) {
  base_url <- "https://clinicaltrials.gov/api/v2/studies?"
  
  url <- paste0(
    base_url,
    "query.term=", URLencode(expr, reserved = TRUE),
    "&pageSize=", page_size,
    if (!is.null(page_token)) paste0("&pageToken=", page_token) else ""
  )
  
  response <- GET(url)
  res <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  return(res)
}

# ---------------------------------------------------------------
# 3. Loop over all pages until no more results
# ---------------------------------------------------------------
all_studies <- list()
page_token <- NULL

repeat {
  res <- fetch_page(condition, page_size, page_token)
  if (length(res$studies) == 0) break
  
  all_studies[[length(all_studies) + 1]] <- res$studies
  
  if (is.null(res$nextPageToken)) break
  page_token <- res$nextPageToken
}

df_trials <- bind_rows(all_studies)

# ---------------------------------------------------------------
# 4. Save to data folder with condition + download date
# ---------------------------------------------------------------
if (!dir.exists("data")) dir.create("data")
today <- Sys.Date()
 
saveRDS(df_trials, rds_file)
cat("Saved", nrow(df_trials), "raw rows to", rds_file, "\n")

# ---------------------------------------------------------------
# 5. Preview
# ---------------------------------------------------------------
cat("Saved", nrow(df_trials), "raw rows to", rds_file, "\n")

