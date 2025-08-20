# Script: clean_trials.R
# ---------------------------------------------------------------
# Script to clean and standardize downloaded ClinicalTrials.gov data
# Focus: keep only key fields for analysis and Shiny dashboard
# ---------------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tidyr)

# ---------------------------------------------------------------
# 1. Read the raw data (latest file)
#
# Currently implemented: "prostate cancer"
# Easily extendable: kidney cancer, bladder cancer, testicular cancer...
# ---------------------------------------------------------------
raw_file <- list.files("data", pattern = "raw_trials_prostate.*\\.rds", full.names = TRUE)
raw_file <- raw_file[which.max(file.mtime(raw_file))]
df_raw <- readRDS(raw_file)

# ---------------------------------------------------------------
# 2. Select and rename relevant fields
# ---------------------------------------------------------------

# --- Basic helpers ------------------------------------------------------------

# Safe column getter (exact match)
getcol <- function(df, nm) if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))

# Robust column getter (exact names first, fallback to prefix)
getcol_robust <- function(df, exacts = character(), prefix = NULL) {
  nm <- names(df)
  for (ex in exacts) if (ex %in% nm) return(df[[ex]])
  if (!is.null(prefix)) {
    cand <- nm[startsWith(nm, prefix)]
    if (length(cand)) return(df[[cand[1]]])
  }
  return(rep(NA, nrow(df)))
}

# Collapse lists or character vectors into a single string per row
collapse_any <- function(x) {
  if (!is.list(x)) {
    v <- as.character(x)
    v[!nzchar(v)] <- NA_character_
    return(v)
  }
  purrr::map_chr(x, ~{
    v <- tryCatch(unlist(.x, use.names = FALSE), error=function(e) character())
    v <- v[!is.na(v) & nzchar(as.character(v))]
    if (length(v)==0) NA_character_ else paste(unique(as.character(v)), collapse="; ")
  })
}

# Parse numbers safely
numify <- function(x) {
  if (is.list(x)) {
    sapply(x, function(z) suppressWarnings(readr::parse_number(paste(unlist(z), collapse=" "))))
  } else if (is.character(x)) {
    suppressWarnings(readr::parse_number(x))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

# First non-empty value among multiple vectors
coalesce_nonempty <- function(...) {
  mats <- list(...)
  out <- character(length(mats[[1]])); out[] <- NA_character_
  for (v in mats) {
    pick <- is.na(out) & !is.na(v) & nzchar(v)
    out[pick] <- v[pick]
  }
  out
}

# Parse multiple date formats into Date
parse_date_relaxed <- function(x) {
  x <- as.character(x)
  as.Date(x, tryFormats = c(
    "%Y-%m-%d","%Y/%m/%d","%d/%m/%Y","%m/%d/%Y",
    "%b %d, %Y","%B %d, %Y","%B %Y","%Y"
  ))
}

# Extract NCTId
get_nct <- function(df) {
  cands <- names(df)[grepl("identificationModule\\.nctId$", names(df))]
  out <- rep(NA_character_, nrow(df))
  for (cn in cands) {
    v <- df[[cn]]
    if (is.list(v)) v <- vapply(v, function(z) paste(unlist(z), collapse=" "), character(1))
    v <- toupper(trimws(as.character(v)))
    v2 <- sub(".*\\b(NCT[0-9]{8})\\b.*", "\\1", v)
    v2[!grepl("^NCT[0-9]{8}$", v2)] <- NA
    pick <- is.na(out) & !is.na(v2)
    out[pick] <- v2[pick]
  }
  out
}

# Collect values by prefix (optionally filter by final tokens)
collect_by_prefix <- function(df, prefix, fields = NULL, sep = "; ") {
  nm <- names(df)
  cols <- nm[startsWith(nm, prefix)]
  if (length(cols) == 0) return(rep(NA_character_, nrow(df)))
  
  if (!is.null(fields) && length(fields)) {
    tokenize <- function(nm) {
      tok <- sub("^.*\\.", "", nm)         # after last dot
      tok <- sub("\\.[0-9]+$", "", tok)    # remove .0, .1, ...
      tolower(tok)
    }
    toks <- tokenize(cols)
    fields_low <- tolower(fields)
    keep <- toks %in% fields_low
    if (any(keep)) cols <- cols[keep]
  }
  
  out <- character(nrow(df)); out[] <- NA_character_
  for (i in seq_len(nrow(df))) {
    vals <- c()
    for (c in cols) {
      v <- df[[c]][i]
      if (is.list(v)) v <- unlist(v, use.names = FALSE)
      v <- trimws(as.character(v))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) vals <- c(vals, v)
    }
    vals <- unique(vals)
    if (length(vals)) out[i] <- paste(vals, collapse = sep)
  }
  out
}

# --- Field-specific extractors ------------------------------------------------

# Phase (robust, with observational fallback)
get_phase_robust <- function(df) {
  p1 <- collect_by_prefix(df, "protocolSection.designModule.phases")
  p2 <- collect_by_prefix(df, "protocolSection.designModule.phaseList.phases")
  p3 <- collect_by_prefix(df, "protocolSection.designModule.phase")   
  phase_raw <- coalesce_nonempty(p1, p2, p3)
  
  study_type <- getcol_robust(df,
                              exacts = "protocolSection.designModule.studyType",
                              prefix = "protocolSection.designModule.studyType")
  is_obs <- grepl("observational", study_type, ignore.case = TRUE)
  
  norm <- function(x){
    x <- toupper(trimws(x))
    x <- gsub("\\s+", "_", x)
    x
  }
  
  phase_norm <- ifelse(is.na(phase_raw) | !nzchar(phase_raw), NA_character_, norm(phase_raw))
  phase_norm[is_obs] <- NA_character_
  phase_norm
}

# --- Prepare critical fields before final table -------------------------------

Phase_final <- get_phase_robust(df_raw)

Interventions_final <- collect_by_prefix(
  df_raw,
  "protocolSection.armsInterventionsModule.interventions",
  fields = c("name","interventionType","type","description","label","otherName","otherNames","armGroupLabels","armGroupLabel")
)
if (all(is.na(Interventions_final))) {
  Interventions_final <- collect_by_prefix(df_raw, "protocolSection.armsInterventionsModule.interventions")
}
if (all(is.na(Interventions_final))) {
  Interventions_final <- collect_by_prefix(df_raw, "protocolSection.armsInterventionsModule.armGroups")
}

PrimaryOutcome_final <- collect_by_prefix(
  df_raw,
  "protocolSection.outcomesModule.primaryOutcomes",
  fields = c("measure","description","timeFrame")
)
if (all(is.na(PrimaryOutcome_final))) {
  PrimaryOutcome_final <- collect_by_prefix(df_raw, "protocolSection.outcomesModule.primaryOutcomes")
}

SecondaryOutcome_final <- collect_by_prefix(
  df_raw,
  "protocolSection.outcomesModule.secondaryOutcomes",
  fields = c("measure","description","timeFrame")
)
if (all(is.na(SecondaryOutcome_final))) {
  SecondaryOutcome_final <- collect_by_prefix(df_raw, "protocolSection.outcomesModule.secondaryOutcomes")
}

Start_final <- coalesce_nonempty(
  getcol_robust(df_raw, exacts=c(
    "protocolSection.statusModule.startDateStruct.date",
    "protocolSection.statusModule.startDate"
  ), prefix="protocolSection.statusModule.startDate"),
  NA_character_
)

# --- Build final clean dataset ------------------------------------------------

df_clean <- tibble::tibble(
  NCTId   = get_nct(df_raw),
  
  Title   = coalesce_nonempty(
    getcol_robust(df_raw,
                  exacts="protocolSection.identificationModule.briefTitle",
                  prefix="protocolSection.identificationModule.briefTitle"),
    getcol_robust(df_raw,
                  exacts="protocolSection.identificationModule.officialTitle",
                  prefix="protocolSection.identificationModule.officialTitle")
  ),
  
  Status  = getcol_robust(df_raw,
                          exacts="protocolSection.statusModule.overallStatus",
                          prefix="protocolSection.statusModule.overallStatus"),
  
  StartDate  = coalesce_nonempty(
    Start_final,
    getcol_robust(df_raw,
                  exacts="protocolSection.statusModule.startDate",
                  prefix="protocolSection.statusModule.startDate")
  ),
  PrimaryCompletionDate = coalesce_nonempty(
    getcol_robust(df_raw,
                  exacts="protocolSection.statusModule.primaryCompletionDateStruct.date",
                  prefix="protocolSection.statusModule.primaryCompletionDateStruct.date"),
    getcol_robust(df_raw,
                  exacts="protocolSection.statusModule.primaryCompletionDate",
                  prefix="protocolSection.statusModule.primaryCompletionDate")
  ),
  CompletionDate        = coalesce_nonempty(
    getcol_robust(df_raw,
                  exacts="protocolSection.statusModule.completionDateStruct.date",
                  prefix="protocolSection.statusModule.completionDateStruct.date"),
    getcol_robust(df_raw,
                  exacts="protocolSection.statusModule.completionDate",
                  prefix="protocolSection.statusModule.completionDate")
  ),
  
  Enrollment = getcol_robust(df_raw,
                             exacts="protocolSection.designModule.enrollmentInfo.count",
                             prefix="protocolSection.designModule.enrollmentInfo.count"),
  Sex        = getcol_robust(df_raw,
                             exacts="protocolSection.eligibilityModule.sex",
                             prefix="protocolSection.eligibilityModule.sex"),
  MinimumAge = getcol_robust(df_raw,
                             exacts="protocolSection.eligibilityModule.minimumAge",
                             prefix="protocolSection.eligibilityModule.minimumAge"),
  MaximumAge = getcol_robust(df_raw,
                             exacts="protocolSection.eligibilityModule.maximumAge",
                             prefix="protocolSection.eligibilityModule.maximumAge"),
  
  Phase           = Phase_final,
  Condition       = coalesce_nonempty(
    collapse_any(getcol_robust(df_raw,
                               exacts="protocolSection.conditionsModule.conditions",
                               prefix="protocolSection.conditionsModule.conditions")),
    collapse_any(getcol_robust(df_raw,
                               exacts="protocolSection.conditionsModule.keywords",
                               prefix="protocolSection.conditionsModule.keywords"))
  ),
  Interventions   = Interventions_final,
  Sponsor         = getcol_robust(df_raw,
                                  exacts="protocolSection.sponsorCollaboratorsModule.leadSponsor.name",
                                  prefix="protocolSection.sponsorCollaboratorsModule.leadSponsor.name"),
  Collaborators   = collapse_any(getcol_robust(df_raw,
                                               exacts="protocolSection.sponsorCollaboratorsModule.collaborators",
                                               prefix="protocolSection.sponsorCollaboratorsModule.collaborators")),
  PrimaryOutcome   = PrimaryOutcome_final,
  SecondaryOutcome = SecondaryOutcome_final
) %>%
  mutate(
    StartDate  = parse_date_relaxed(StartDate),
    PrimaryCompletionDate = parse_date_relaxed(PrimaryCompletionDate),
    CompletionDate        = parse_date_relaxed(CompletionDate),
    Enrollment = numify(Enrollment),
    MinimumAge = numify(MinimumAge),
    MaximumAge = numify(MaximumAge),
    IncludePhase23 = grepl("PHASE2", Phase, ignore.case = TRUE) |
      grepl("PHASE3", Phase, ignore.case = TRUE)
  )

# ---------------------------------------------------------------
# 3. Save clean dataset
# ---------------------------------------------------------------
date_str <- sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.(csv|rds)$", "\\1", basename(raw_file))
out_file <- paste0("data/clean_trials_prostate_", date_str, ".csv")
write_csv(df_clean, out_file)

# ---------------------------------------------------------------
# 4. Preview
# ---------------------------------------------------------------
cat("Read from:", raw_file, "\n")
cat("Saved", nrow(df_clean), "cleaned trials to", out_file, "\n")
print(head(df_clean))

