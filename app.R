# Bayesian Multinomial Shiny App
# R/ directory modules are auto-sourced alphabetically before this file runs:
#   01_constants.R  — DB config, PRECOMPUTED, CONFOUND, COLORS
#   02_utils.R      — theme, format_camel_case, badge helpers
#   03_search.R     — normalize_for_search, elastic_match
#   04_data.R       — load_from_database, process_bib_data, initialize_app
#   05_ui_components.R — ui_css, ui_header, ui_tab_*, ui_footer
#   06_server_corpus.R — server_corpus()
#   07_server_bayesian.R — server_bayesian() -> returns posterior_preds
#   08_server_diagnostics.R — server_diagnostics()
#   09_server_citations.R — server_citations()

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinyFeedback)
library(posterior)
library(plotly)
library(jsonlite)

# UI ====
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),

  ui_css(),
  ui_header(),

  div(class = "container-fluid",
    tabsetPanel(
      id = "tabs",
      type = "pills",

      ui_tab_home(),
      ui_tab_paper(),
      ui_tab_corpus(),
      ui_tab_methodology(),
      ui_tab_bayesian(),
      ui_tab_acknowledgements()
    ),
    ui_footer()
  )
)

# Server ====
server <- function(input, output, session) {

  # Initialize app on startup
  init_data <- initialize_app()

  rv <- reactiveValues(
    raw_data = init_data$raw,
    clean_data = init_data$clean,
    fit_obj = PRECOMPUTED,
    fit_info = if (!is.null(PRECOMPUTED)) PRECOMPUTED$fit_info else NULL,
    fit_counter = 0,
    posterior_preds = NULL,
    contrasts_computed = FALSE,
    contrast_results = list(),
    from_database = TRUE,
    model_summary_visited = FALSE,
    analysis_results_visited = FALSE,
    bayes_current_card = 1,
    bayesian_analysis_visited = if (!is.null(PRECOMPUTED)) TRUE else FALSE,
    selected_posterior_param = NULL,
    modal_citations = NULL
  )

  # Show initialization notification
  if (init_data$initialized) {
    showNotification(
      HTML(paste0(
        "<strong>\u2713 App Initialized</strong><br/>",
        init_data$n_works, " works loaded from database<br/>",
        "<small>set \u00d7 regionality \u00d7 century</small>"
      )),
      type = "message",
      duration = 5
    )
  } else {
    showNotification(
      "Failed to load database. Check that the database file exists.",
      type = "error",
      duration = NULL
    )
  }

  output$data_loaded <- reactive({ !is.null(rv$raw_data) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_fitted <- reactive({ !is.null(rv$fit_obj) })
  outputOptions(output, "model_fitted", suspendWhenHidden = FALSE)

  # Data status badge
  output$data_status_badge <- renderUI({
    req(rv$clean_data)
    n <- nrow(rv$clean_data)
    div(
      span(class = "status-badge status-success",
           icon("check-circle"),
           paste(" Dataset loaded:", n, "works")),
      span(class = "status-badge status-success",
           icon("check-circle"),
           " Variables: set (outcome), regionality (geographic)"),
      span(class = "status-badge status-success",
           icon("check-circle"),
           " Century effect: included")
    )
  })

  # HOME TAB - Animation
  output$home_animation <- renderImage({
    list(
      src = normalizePath("output/animations/islamic_bibliography_map_ALL.gif"),
      contentType = "image/gif",
      width = "100%",
      alt = "Islamic Bibliography Geographic Animation"
    )
  }, deleteFile = FALSE)

  # Dynamic works counts
  output$home_works_count <- renderText({
    if (!is.null(rv$clean_data)) nrow(rv$clean_data) else "172"
  })

  output$corpus_works_count <- renderText({
    if (!is.null(rv$clean_data)) nrow(rv$clean_data) else "172"
  })

  # Delegate to server modules
  server_corpus(input, output, session, rv)
  bayes <- server_bayesian(input, output, session, rv)
  server_diagnostics(input, output, session, rv, bayes$posterior_preds)
  server_citations(input, output, session, rv)
}

shinyApp(ui = ui, server = server)
