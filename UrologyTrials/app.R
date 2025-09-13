# app.R
# ---------------------------------------------------------------
# App: Urology Trials — Shiny Dashboard
# Purpose: load cleaned ClinicalTrials.gov data and visualize
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# 0) Libraries & global options
# ---------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(forcats)
library(bslib)
library(tibble)

options(shiny.maxRequestSize = 100*1024^2)  # allow uploads up to 100 MB
theme_set(theme_minimal(base_size = 12))

# ---------------------------------------------------------------
# 1) Helpers
# ---------------------------------------------------------------
pretty_phase <- function(x) {
  x <- toupper(trimws(x))
  x[x %in% c("N/A", "NA", "", NA)] <- NA
  x <- gsub("PHASE\\s*([0-4])\\s*[,;:_\\-]+\\s*PHASE\\s*([0-4])", "PHASE\\1/PHASE\\2", x, perl = TRUE)
  x <- gsub("EARLY\\s*PHASE\\s*1|EARLY_PHASE_?1", "Early Phase 1", x)
  x <- gsub("^PHASE\\s*([0-4])$", "Phase \\1", x)
  x <- gsub("^PHASE\\s*([0-4])\\s*/\\s*PHASE\\s*([0-4])$", "Phase \\1/\\2", x)
  x <- tools::toTitleCase(tolower(x))
  x
}

extract_intervention_type <- function(x) {
  t <- sub("^\\s*([^;]+);.*$", "\\1", x)
  t[is.na(x) | !nzchar(trimws(x))] <- NA_character_
  t
}

pretty_label <- function(x) {
  ifelse(is.na(x), NA_character_,
         tools::toTitleCase(gsub("_", " ", tolower(trimws(x)))))
}

status_finished <- c("COMPLETED","APPROVED_FOR_MARKETING")
status_inprog   <- c("RECRUITING","ACTIVE_NOT_RECRUITING","ENROLLING_BY_INVITATION","NOT_YET_RECRUITING")
status_failed   <- c("TERMINATED","WITHDRAWN","SUSPENDED","TEMPORARILY_NOT_AVAILABLE","NO_LONGER_AVAILABLE")

order_phase <- c("Early Phase 1","Phase 1","Phase 1/2","Phase 2","Phase 2/3","Phase 3","Phase 4")

# ---------------------------------------------------------------
# 2) UI
# ---------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cosmo"),
  titlePanel("Urology Trials — Shiny Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("Load a cleaned CSV produced by your cleaning script."),
      fileInput("csv", "CSV file", accept = c(".csv")),
      textInput("path", "or local CSV path", value = "data/clean_trials_prostate_cancer_2025-08-20.csv"),
      actionButton("load", "Load data"),
      tags$hr(),
      checkboxInput("show_tables", "Show tables below plots", FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 br(),
                 fluidRow(
                   column(4, h4("Total"), textOutput("n_total")),
                   column(4, h4("In progress"), textOutput("n_inprog")),
                   column(4, h4("Completed"), textOutput("n_finished"))
                 ),
                 br(),
                 plotOutput("pie_status", height = 350)
        ),
        tabPanel("In-progress",
                 h3("By phase"),
                 plotOutput("plot_inprog_phase", height = 350),
                 conditionalPanel("input.show_tables", tableOutput("tbl_inprog_phase")),
                 tags$hr(),
                 h3("By intervention type"),
                 plotOutput("plot_inprog_interv", height = 350),
                 conditionalPanel("input.show_tables", tableOutput("tbl_inprog_interv"))
        ),
        tabPanel("Completed",
                 h3("By phase"),
                 plotOutput("plot_finished_phase", height = 350),
                 conditionalPanel("input.show_tables", tableOutput("tbl_finished_phase")),
                 tags$hr(),
                 h3("Duration (percent of trials with computable duration)"),
                 plotOutput("plot_duration", height = 350),
                 conditionalPanel("input.show_tables", tableOutput("tbl_duration")),
                 tags$hr(),
                 h3("By intervention type"),
                 plotOutput("plot_finished_interv", height = 350),
                 conditionalPanel("input.show_tables", tableOutput("tbl_finished_interv"))
        ),
        tabPanel("Discontinued",
                 h3("Sub-status"),
                 plotOutput("plot_failed", height = 350),
                 conditionalPanel("input.show_tables", tableOutput("tbl_failed"))
        )
      )
    )
  )
)

# ---------------------------------------------------------------
# 3) Server
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # 3.1) Reactive stores
  df_r <- reactiveVal(NULL)
  condition <- reactiveVal("")
  date_str <- reactiveVal("")
  
  # 3.2) Data loader
  observeEvent(input$load, {
    # prefer uploaded file; otherwise, use typed path
    path <- if (!is.null(input$csv) && nzchar(input$csv$datapath)) {
      input$csv$datapath
    } else {
      input$path
    }
    
    if (!file.exists(path)) {
      showNotification(paste("File not found:", path), type = "error", duration = 5)
      return(NULL)
    }
    
    showNotification("Loading data…", type = "message", duration = 2)
    df <- tryCatch(readr::read_csv(path, show_col_types = FALSE),
                   error = function(e) {
                     showNotification(paste("CSV read error:", e$message), type = "error", duration = 6)
                     return(NULL)
                   })
    req(!is.null(df))
    
    # derive condition/date from file name where possible
    bn <- basename(path)
    m <- stringr::str_match(bn, "^clean_trials_(.+)_(\\d{4}-\\d{2}-\\d{2})\\.csv$")
    cond_slug <- if (!is.na(m[1,2])) m[1,2] else "urology trials"
    condition(stringr::str_replace_all(cond_slug, "_", " ") |> stringr::str_to_title())
    date_str(if (!is.na(m[1,3])) m[1,3] else as.character(Sys.Date()))
    
    # light cleaning (aligned with Rmd)
    df <- df %>%
      mutate(
        Phase = pretty_phase(Phase),
        SimpleIntervention = pretty_label(extract_intervention_type(Interventions))
      )
    
    df_r(df)
    showNotification("Data loaded.", type = "message", duration = 3)
  }, ignoreInit = TRUE)
  
  # 3.3) Subsets & counters
  n_total <- reactive({ nrow(req(df_r())) })
  df_finished <- reactive({ req(df_r()); filter(df_r(), Status %in% status_finished) })
  df_inprog  <- reactive({ req(df_r()); filter(df_r(), Status %in% status_inprog) })
  df_failed  <- reactive({ req(df_r()); filter(df_r(), Status %in% status_failed) })
  
  output$n_total    <- renderText(n_total())
  output$n_inprog   <- renderText(nrow(req(df_inprog())))
  output$n_finished <- renderText(nrow(req(df_finished())))
  
  # -------------------------------------------------------------
  # 3.4) Overview — status composition
  # -------------------------------------------------------------
  output$pie_status <- renderPlot({
    req(df_r())
    tb <- tibble(
      group = c("In progress","Completed","Discontinued"),
      n = c(nrow(df_inprog()), nrow(df_finished()), nrow(df_failed()))
    ) %>% filter(n > 0)
    
    ggplot(tb, aes(x = "", y = n, fill = group)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(
        title = paste0("Status — ", condition()),
        subtitle = paste("As of", date_str()),
        x = NULL, y = NULL
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank()
      )
  })
  
  # -------------------------------------------------------------
  # 3.5) In-progress — by phase
  # -------------------------------------------------------------
  inprog_phase <- reactive({
    req(df_inprog()) %>%
      filter(!is.na(Phase)) %>%
      count(Phase, sort = TRUE) %>%
      mutate(Phase = factor(Phase, levels = order_phase))
  })
  output$plot_inprog_phase <- renderPlot({
    tb <- req(inprog_phase())
    ggplot(tb, aes(x = Phase, y = n)) +
      geom_col() + coord_flip() +
      labs(
        title = paste0("In-progress trials by phase — ", condition()),
        subtitle = paste("As of", date_str()),
        x = "Phase", y = "Count"
      )
  })
  output$tbl_inprog_phase <- renderTable(inprog_phase())
  
  # -------------------------------------------------------------
  # 3.6) In-progress — by intervention type
  # -------------------------------------------------------------
  inprog_interv <- reactive({
    req(df_inprog()) %>%
      filter(!is.na(SimpleIntervention) & nzchar(SimpleIntervention)) %>%
      count(SimpleIntervention, sort = TRUE)
  })
  output$plot_inprog_interv <- renderPlot({
    tb <- req(inprog_interv())
    ggplot(tb, aes(x = fct_reorder(SimpleIntervention, n), y = n)) +
      geom_col() + coord_flip() +
      labs(
        title = paste0("In-progress trials by intervention type — ", condition()),
        subtitle = paste("As of", date_str()),
        x = "Intervention type", y = "Count"
      )
  })
  output$tbl_inprog_interv <- renderTable(inprog_interv())
  
  # -------------------------------------------------------------
  # 3.7) Completed — by phase
  # -------------------------------------------------------------
  finished_phase <- reactive({
    req(df_finished()) %>%
      filter(!is.na(Phase)) %>%
      count(Phase, sort = TRUE) %>%
      mutate(Phase = factor(Phase, levels = order_phase))
  })
  output$plot_finished_phase <- renderPlot({
    tb <- req(finished_phase())
    ggplot(tb, aes(x = Phase, y = n)) +
      geom_col() + coord_flip() +
      labs(
        title = paste0("Completed trials by phase — ", condition()),
        subtitle = paste("As of", date_str()),
        x = "Phase", y = "Count"
      )
  })
  output$tbl_finished_phase <- renderTable(finished_phase())
  
  # -------------------------------------------------------------
  # 3.8) Completed — duration distribution
  # -------------------------------------------------------------
  finished_duration <- reactive({
    d <- req(df_finished()) %>%
      transmute(
        Start = ymd(StartDate, quiet = TRUE),
        PComp = ymd(PrimaryCompletionDate, quiet = TRUE),
        Comp  = ymd(CompletionDate, quiet = TRUE)
      ) %>%
      mutate(
        End = coalesce(Comp, PComp),
        DurationMonths = if_else(
          !is.na(Start) & !is.na(End) & End >= Start,
          time_length(interval(Start, End), "months"),
          as.numeric(NA)
        )
      )
    n_ok <- sum(!is.na(d$DurationMonths))
    d |>
      filter(!is.na(DurationMonths), DurationMonths >= 0) |>
      mutate(
        Bucket = cut(DurationMonths,
                     breaks = c(-Inf, 6, 12, 24, 36, 60, Inf),
                     labels = c("<6","6–12","12–24","24–36","36–60","≥60"),
                     right = FALSE)
      ) |>
      count(Bucket, .drop = FALSE) |>
      mutate(pct_num = if (n_ok > 0) 100 * n / n_ok else NA_real_)
  })
  output$plot_duration <- renderPlot({
    tb <- req(finished_duration())
    ggplot(tb, aes(x = Bucket, y = pct_num)) +
      geom_col() +
      labs(
        title = paste0("Duration of completed trials — ", condition()),
        subtitle = paste("As of", date_str(), "| Percent with computable duration"),
        x = "Months", y = "Percent"
      )
  })
  output$tbl_duration <- renderTable(finished_duration())
  
  # -------------------------------------------------------------
  # 3.9) Completed — by intervention type
  # -------------------------------------------------------------
  finished_interv <- reactive({
    req(df_finished()) %>%
      filter(!is.na(SimpleIntervention) & nzchar(SimpleIntervention)) %>%
      count(SimpleIntervention, sort = TRUE)
  })
  output$plot_finished_interv <- renderPlot({
    tb <- req(finished_interv())
    ggplot(tb, aes(x = fct_reorder(SimpleIntervention, n), y = n)) +
      geom_col() + coord_flip() +
      labs(
        title = paste0("Completed trials by intervention type — ", condition()),
        subtitle = paste("As of", date_str()),
        x = "Intervention type", y = "Count"
      )
  })
  output$tbl_finished_interv <- renderTable(finished_interv())
  
  # -------------------------------------------------------------
  # 3.10) Discontinued — sub-status
  # -------------------------------------------------------------
  failed_tbl <- reactive({
    req(df_failed()) %>%
      filter(!is.na(Status)) %>%
      mutate(
        Status = gsub("_", " ", tolower(Status)),
        Status = paste0(toupper(substr(Status, 1, 1)), substr(Status, 2, nchar(Status)))
      ) %>%
      count(Status, sort = TRUE)
  })
  output$plot_failed <- renderPlot({
    tb <- req(failed_tbl())
    tb$Status <- factor(tb$Status, levels = tb$Status)
    ggplot(tb, aes(x = Status, y = n)) +
      geom_col() + coord_flip() +
      labs(
        title = paste0("Discontinued trials by sub-status — ", condition()),
        subtitle = paste("As of", date_str()),
        x = "Sub-status", y = "Count"
      )
  })
  output$tbl_failed <- renderTable(failed_tbl())
}

# ---------------------------------------------------------------
# 4) App
# ---------------------------------------------------------------
shinyApp(ui, server)
