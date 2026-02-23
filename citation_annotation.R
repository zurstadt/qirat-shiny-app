# ============================================================================
# Citation Annotation App
# Local Shiny app for parsing citations and linking to works/authors in DB
# Uses citation_parsers.R for all parsing logic
# ============================================================================

library(shiny)
library(DBI)
library(RSQLite)
library(jsonlite)

# Database path
DB_PATH <- "data/iqsa_bibliography.db"

# Source the refined citation parsers
source("../shiny-app/deploy/citation_parsers.R")

# ============================================================================
# RIS Export Function
# ============================================================================

to_ris <- function(parsed, db_row = NULL) {
  csl_to_ris_type <- list(
    "book" = "BOOK",
    "article-journal" = "JOUR",
    "entry-encyclopedia" = "ENCYC",
    "chapter" = "CHAP",
    "thesis" = "THES",
    "webpage" = "ELEC"
  )

  csl <- tryCatch(to_csl_json(parsed), error = function(e) NULL)
  if (is.null(csl)) return(NULL)

  ris_type <- csl_to_ris_type[[csl$type]] %||% "GEN"

  lines <- c(paste0("TY  - ", ris_type))

  # Authors
  if (!is.null(csl$author)) {
    for (a in csl$author) {
      name <- if (!is.null(a$family) && !is.null(a$given)) {
        paste0(a$family, ", ", a$given)
      } else if (!is.null(a$literal)) {
        a$literal
      } else {
        NULL
      }
      if (!is.null(name)) lines <- c(lines, paste0("AU  - ", name))
    }
  }

  # Title (append edition_qualifier if present, e.g. "GAL (Supplement)")
  if (!is.null(csl$title) && !is.na(csl$title)) {
    title_out <- csl$title
    eq <- if (!is.null(db_row)) db_row$edition_qualifier else parsed$edition_qualifier
    if (!is.null(eq) && !is.na(eq) && nchar(trimws(eq)) > 0) {
      title_out <- paste0(title_out, " (", trimws(eq), ")")
    }
    lines <- c(lines, paste0("TI  - ", title_out))
  }

  # Container title (journal, encyclopedia)
  if (!is.null(csl$`container-title`) && !is.na(csl$`container-title`)) {
    lines <- c(lines, paste0("T2  - ", csl$`container-title`))
  }

  # Editors
  if (!is.null(csl$editor)) {
    for (ed in csl$editor) {
      name <- if (!is.null(ed$family) && !is.null(ed$given)) {
        paste0(ed$family, ", ", ed$given)
      } else if (!is.null(ed$literal)) {
        ed$literal
      } else {
        NULL
      }
      if (!is.null(name)) lines <- c(lines, paste0("A2  - ", name))
    }
  }

  # Year
  if (!is.null(csl$issued) && !is.null(csl$issued$`date-parts`)) {
    year <- csl$issued$`date-parts`[[1]][[1]]
    if (!is.null(year) && !is.na(year)) {
      lines <- c(lines, paste0("PY  - ", year))
    }
  }

  # Volume
  if (!is.null(csl$volume) && !is.na(csl$volume)) {
    lines <- c(lines, paste0("VL  - ", csl$volume))
  }

  # Pages
  if (!is.null(csl$page) && !is.na(csl$page)) {
    pages <- strsplit(as.character(csl$page), "[-–]")[[1]]
    sp <- trimws(pages[1])
    lines <- c(lines, paste0("SP  - ", sp))
    if (length(pages) > 1) {
      ep <- trimws(pages[2])
      # Expand abbreviated end page: "539–40" → EP=540, not EP=40
      if (nchar(ep) < nchar(sp)) {
        ep <- paste0(substr(sp, 1, nchar(sp) - nchar(ep)), ep)
      }
      lines <- c(lines, paste0("EP  - ", ep))
    }
  }

  # Publisher
  if (!is.null(csl$publisher) && !is.na(csl$publisher)) {
    lines <- c(lines, paste0("PB  - ", csl$publisher))
  }

  # Place
  if (!is.null(csl$`publisher-place`) && !is.na(csl$`publisher-place`)) {
    lines <- c(lines, paste0("CY  - ", csl$`publisher-place`))
  }

  # ISBN/ISSN
  if (!is.null(csl$ISBN) && !is.na(csl$ISBN)) {
    lines <- c(lines, paste0("SN  - ", csl$ISBN))
  }
  if (!is.null(csl$ISSN) && !is.na(csl$ISSN)) {
    lines <- c(lines, paste0("SN  - ", csl$ISSN))
  }

  # URL — from CSL or extracted from original_text for PUA entries
  url_val <- csl$URL
  if ((is.null(url_val) || is.na(url_val)) && !is.null(db_row)) {
    url_match <- regmatches(db_row$original_text,
                            regexpr("https?://[^\\s,]+", db_row$original_text, perl = TRUE))
    if (length(url_match) > 0 && nchar(url_match) > 0) url_val <- url_match
  }
  if (!is.null(url_val) && !is.na(url_val) && nchar(url_val) > 0) {
    lines <- c(lines, paste0("UR  - ", url_val))
  }

  # Notes: include hijri year, entry number, id_number for PUA, etc.
  notes <- c()
  if (!is.null(parsed$year_hijri) && !is.na(parsed$year_hijri)) {
    notes <- c(notes, paste0("Hijri year: ", parsed$year_hijri))
  }
  entry_num <- parsed$entry_number
  if ((is.null(entry_num) || is.na(entry_num)) && !is.null(db_row)) {
    entry_num <- db_row$entry_number
  }
  if (!is.null(entry_num) && !is.na(entry_num) && nchar(trimws(as.character(entry_num))) > 0) {
    notes <- c(notes, paste0("Entry: №", entry_num))
  }
  if (!is.null(parsed$section) && !is.na(parsed$section)) {
    notes <- c(notes, paste0("Section: ", parsed$section))
  }
  if (length(notes) > 0) {
    lines <- c(lines, paste0("N1  - ", paste(notes, collapse = "; ")))
  }

  lines <- c(lines, "ER  - ")
  paste(lines, collapse = "\n")
}

export_all_ris <- function(db_path, output_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  citations <- dbGetQuery(con, "SELECT * FROM bibliographic_citations")
  if (nrow(citations) == 0) return(invisible(NULL))

  ris_records <- vapply(seq_len(nrow(citations)), function(i) {
    row <- citations[i, ]
    parsed <- tryCatch({
      detected <- detect_citation_type(row$original_text)
      if (detected$form == "long") {
        parse_long_monograph(row$original_text)
      } else if (detected$form == "short") {
        parse_short_citation(row$original_text)
      } else {
        list(
          raw = row$original_text,
          type = row$citation_type %||% "unknown",
          author = row$parsed_author,
          title = row$parsed_title
        )
      }
    }, error = function(e) {
      list(
        raw = row$original_text,
        type = row$citation_type %||% "unknown",
        author = row$parsed_author,
        title = row$parsed_title
      )
    })

    # Override with corrected DB fields (DB values take precedence)
    if (!is.na(row$parsed_author)) parsed$author <- row$parsed_author
    if (!is.na(row$parsed_title)) parsed$title <- row$parsed_title
    if (!is.na(row$parsed_title)) parsed$title_abbrev <- row$parsed_title
    if (!is.na(row$citation_type)) parsed$type <- row$citation_type
    if (!is.na(row$volume_cited)) parsed$volume_cited <- row$volume_cited
    if (!is.na(row$page_cited)) parsed$page_cited <- row$page_cited
    if (!is.na(row$entry_number)) parsed$entry_number <- row$entry_number

    ris <- tryCatch(to_ris(parsed, db_row = row), error = function(e) NULL)
    if (is.null(ris)) "" else ris
  }, character(1))

  ris_records <- ris_records[ris_records != ""]
  if (length(ris_records) > 0) {
    writeLines(paste(ris_records, collapse = "\n\n"), output_path)
  }
  invisible(output_path)
}

# ============================================================================
# Database Helper Functions
# ============================================================================

get_citation_counts <- function(con) {
  work_counts <- dbGetQuery(con, "
    SELECT work_id, COUNT(*) as n
    FROM bibliographic_citations
    WHERE work_id IS NOT NULL
    GROUP BY work_id
  ")
  author_counts <- dbGetQuery(con, "
    SELECT author_id, COUNT(*) as n
    FROM bibliographic_citations
    WHERE author_id IS NOT NULL
    GROUP BY author_id
  ")
  list(works = work_counts, authors = author_counts)
}

save_citation <- function(con, work_id, author_id, original_text, parsed,
                          citation_form, citation_type, link_type, source_type,
                          comment = NA) {
  dbExecute(con, "
    INSERT INTO bibliographic_citations
    (work_id, author_id, original_text, citation_form, citation_type,
     parsed_author, parsed_title, parsed_editor, parsed_place, parsed_publisher,
     parsed_year, volume_cited, page_cited, entry_number, section, notes,
     link_type, source, created_at, comment,
     edition_qualifier, page_german, page_english)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    if (is.null(work_id) || is.na(work_id) || work_id == "") NA else work_id,
    if (is.null(author_id) || is.na(author_id)) NA else as.integer(author_id),
    original_text,
    citation_form,
    citation_type,
    parsed$author %||% parsed$article_author %||% NA,
    parsed$title %||% parsed$article_title %||% parsed$title_abbrev %||% NA,
    parsed$editor %||% NA,
    parsed$place %||% NA,
    parsed$publisher %||% NA,
    parsed$year_gregorian %||% parsed$year_hijri %||% NA,
    parsed$volume_cited %||% parsed$volume %||% NA,
    parsed$page_cited %||% parsed$page %||% NA,
    parsed$entry_number %||% NA,
    parsed$section %||% NA,
    parsed$notes %||% NA,
    link_type,
    source_type,
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    if (is.null(comment) || is.na(comment) || trimws(comment) == "") NA else trimws(comment),
    parsed$edition_qualifier %||% NA,
    parsed$page_german %||% NA,
    parsed$page_english %||% NA
  ))
}

delete_citation <- function(con, citation_id) {
  dbExecute(con, "DELETE FROM bibliographic_citations WHERE citation_id = ?",
            params = list(as.integer(citation_id)))
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        background-color: #f5f5f5;
        padding: 20px;
      }
      .main-container {
        max-width: 1200px;
        margin: 0 auto;
        background: white;
        padding: 30px;
        border-radius: 8px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 2px solid #e0e0e0;
      }
      .entity-card {
        background: #f8f9fa;
        padding: 20px;
        border-radius: 6px;
        margin-bottom: 20px;
        border-left: 4px solid #007bff;
      }
      .entity-card.work-mode { border-left-color: #007bff; }
      .citation-card {
        background: #e8f4e8;
        padding: 15px;
        border-radius: 6px;
        margin-bottom: 10px;
        border-left: 4px solid #28a745;
      }
      .citation-card.short-form {
        background: #fff3cd;
        border-left-color: #ffc107;
      }
      .field-row {
        display: flex;
        margin-bottom: 8px;
      }
      .field-label {
        font-weight: bold;
        color: #495057;
        font-size: 0.85em;
        width: 120px;
        flex-shrink: 0;
      }
      .field-value { flex-grow: 1; }
      .arabic-text {
        font-family: 'Amiri', 'Traditional Arabic', serif;
        font-size: 1.2em;
        direction: rtl;
      }
      .input-section {
        background: #f0f0f0;
        padding: 20px;
        border-radius: 6px;
        margin-bottom: 20px;
      }
      .parsed-preview {
        background: #e9ecef;
        padding: 15px;
        border-radius: 6px;
        margin-top: 15px;
        font-family: monospace;
        white-space: pre-wrap;
        max-height: 300px;
        overflow-y: auto;
      }
      .nav-buttons {
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .badge-long { background: #28a745; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; }
      .badge-short { background: #ffc107; color: black; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; }
      .badge-reference { background: #007bff; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; }
      .badge-edition { background: #17a2b8; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; }
      .badge-study { background: #6f42c1; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; }
      .badge-encyclopedia { background: #fd7e14; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.8em; }
      .stats-bar {
        display: flex;
        gap: 20px;
        margin-bottom: 20px;
        padding: 10px;
        background: #e9ecef;
        border-radius: 6px;
      }
      .stat-item { text-align: center; }
      .stat-value { font-size: 1.5em; font-weight: bold; }
      .stat-label { font-size: 0.85em; color: #666; }
    "))
  ),

  div(class = "main-container",
    div(class = "header",
      h2("Citation Annotation"),
      div(class = "nav-buttons",
        actionButton("prev_entity", icon("arrow-left"), class = "btn btn-secondary"),
        uiOutput("position_display"),
        actionButton("next_entity", icon("arrow-right"), class = "btn btn-secondary")
      )
    ),

    # Stats bar
    uiOutput("stats_bar"),

    # Filters
    fluidRow(
      column(4, selectizeInput("entity_select", "Jump to:",
                               choices = NULL, width = "100%")),
      column(2, selectInput("filter_citations", "Filter:",
                            choices = c("All" = "all",
                                        "With Citations" = "with",
                                        "Without Citations" = "without"),
                            selected = "all")),
      column(2, selectInput("filter_readings", "Readings:",
                            choices = c("All" = "all",
                                        "7" = "7",
                                        "7+1" = "7+1",
                                        "10+" = "10+"),
                            selected = "all")),
      column(2, br(), actionButton("export_ris", "Export RIS",
                                   class = "btn btn-info", icon = icon("download")))
    ),

    hr(),

    # Current entity display
    uiOutput("current_entity_display"),

    # Existing citations
    h4("Citations for This Entity"),
    uiOutput("existing_citations_display"),

    hr(),

    # Add citation section
    h4("Add New Citation"),
    div(class = "input-section",
      tabsetPanel(
        id = "input_tabs",

        tabPanel("Plain Text",
          br(),
          textAreaInput("plain_text_input", "Paste citation text:",
                        rows = 4, width = "100%",
                        placeholder = "Paste a citation in long or short form..."),
          fluidRow(
            column(3, selectInput("force_type", "Force type:",
                                  choices = c("Auto-detect" = "auto",
                                              "Long - Monograph" = "long_monograph",
                                              "Long - Article" = "long_article",
                                              "Long - Dissertation" = "long_dissertation",
                                              "Short - Primary" = "short_primary",
                                              "Short - Secondary" = "short_secondary"))),
            column(3, selectInput("link_type", "Link type:",
                                  choices = c("Reference" = "reference",
                                              "Edition" = "edition",
                                              "Study" = "study",
                                              "Encyclopedia" = "encyclopedia"))),
            column(3, br(), actionButton("parse_text", "Parse", class = "btn btn-info")),
            column(3, br(), actionButton("save_citation", "Save Citation", class = "btn btn-success"))
          ),
          textAreaInput("citation_comment", "Comment (optional):",
                        rows = 3, width = "100%",
                        placeholder = "Scholarly notes, misattributions, cross-references, etc."),
          uiOutput("parse_preview")
        ),

        tabPanel("BibTeX",
          br(),
          fileInput("bib_file", "Upload .bib file:", accept = ".bib"),
          uiOutput("bib_preview"),
          fluidRow(
            column(3, selectInput("bib_link_type", "Link type:",
                                  choices = c("Reference" = "reference",
                                              "Edition" = "edition",
                                              "Study" = "study",
                                              "Encyclopedia" = "encyclopedia"))),
            column(3, br(), actionButton("add_bib_refs", "Add Selected", class = "btn btn-success"))
          )
        ),

        tabPanel("RIS",
          br(),
          fileInput("ris_file", "Upload .ris file:", accept = ".ris"),
          uiOutput("ris_preview"),
          fluidRow(
            column(3, selectInput("ris_link_type", "Link type:",
                                  choices = c("Reference" = "reference",
                                              "Edition" = "edition",
                                              "Study" = "study",
                                              "Encyclopedia" = "encyclopedia"))),
            column(3, br(), actionButton("add_ris_refs", "Add Selected", class = "btn btn-success"))
          )
        ),

        tabPanel("Manual Entry",
          br(),
          fluidRow(
            column(6,
              selectInput("manual_type", "Citation Type:",
                          choices = c("Monograph" = "monograph",
                                      "Article" = "article",
                                      "Dissertation" = "dissertation",
                                      "Book Section" = "book_section",
                                      "Encyclopedia" = "encyclopedia")),
              textInput("manual_author", "Author:"),
              textInput("manual_title", "Title:"),
              textInput("manual_editor", "Editor(s):"),
              selectInput("manual_link_type", "Link type:",
                          choices = c("Reference" = "reference",
                                      "Edition" = "edition",
                                      "Study" = "study",
                                      "Encyclopedia" = "encyclopedia"))
            ),
            column(6,
              textInput("manual_place", "Place:"),
              textInput("manual_publisher", "Publisher:"),
              fluidRow(
                column(6, textInput("manual_year", "Year:")),
                column(6, textInput("manual_volumes", "Volumes:"))
              ),
              fluidRow(
                column(4, textInput("manual_volume", "Vol. Cited:")),
                column(4, textInput("manual_page", "Page(s):")),
                column(4, textInput("manual_entry", "Entry:"))
              ),
              textAreaInput("manual_notes", "Notes:", rows = 2)
            )
          ),
          actionButton("add_manual_ref", "Add Reference", class = "btn btn-success")
        )
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # Database connection (persistent for session)
  db_con <- dbConnect(SQLite(), DB_PATH)
  onStop(function() { dbDisconnect(db_con) })

  rv <- reactiveValues(
    works = NULL,
    entities = NULL,       # works entity list
    filtered_ids = NULL,
    current_index = 1,
    parsed_sequence = NULL,
    bib_entries = NULL,
    ris_entries = NULL,
    citation_refresh = 0   # counter to trigger citation list refresh
  )

  # Load works on startup
  observe({
    rv$works <- dbGetQuery(db_con, "
      SELECT w.work_id, w.title, w.title_arabic, w.type, w.system,
             w.author_id,
             COALESCE(a.author_name_canonical, a.author_name) as author_name,
             a.author_name_arabic as author_arabic, a.death_hijri,
             a.regionality
      FROM works w
      LEFT JOIN authors a ON w.author_id = a.author_id
      WHERE a.in_range = 'T'
        AND w.system IS NOT NULL AND w.system != 'NA'
      ORDER BY CASE WHEN a.death_hijri IS NULL THEN 1 ELSE 0 END,
               CAST(a.death_hijri AS INTEGER), w.work_id
    ")
  })

  # Build entity list from works
  observe({
    req(rv$works)

    counts <- get_citation_counts(db_con)

    entities <- rv$works
    entities$has_citations <- entities$work_id %in% counts$works$work_id
    entities$citation_count <- sapply(entities$work_id, function(wid) {
      row <- counts$works[counts$works$work_id == wid, ]
      if (nrow(row) > 0) row$n[1] else 0L
    })
    entities$entity_id <- entities$work_id
    entities$entity_label <- paste0(
      entities$work_id, " - ",
      substr(entities$title, 1, 50),
      " (", entities$author_name, ", d. ", entities$death_hijri, ")"
    )

    rv$entities <- entities

    choices <- setNames(seq_len(nrow(entities)), entities$entity_label)
    updateSelectizeInput(session, "entity_select", choices = choices, server = TRUE)
  })

  # Filter by citation status and readings
  observe({
    req(rv$entities)
    filter_citations <- input$filter_citations
    filter_readings <- input$filter_readings

    ids <- seq_len(nrow(rv$entities))

    if (filter_citations == "with") {
      ids <- which(rv$entities$has_citations)
    } else if (filter_citations == "without") {
      ids <- which(!rv$entities$has_citations)
    }

    if (!is.null(filter_readings) && filter_readings != "all") {
      reading_match <- which(rv$entities$system == filter_readings)
      ids <- intersect(ids, reading_match)
    }

    rv$filtered_ids <- ids
    rv$current_index <- 1
  })

  # Current entity
  current_entity <- reactive({
    req(rv$entities, length(rv$filtered_ids) > 0)
    idx <- min(rv$current_index, length(rv$filtered_ids))
    row_idx <- rv$filtered_ids[idx]
    rv$entities[row_idx, ]
  })

  # Stats bar
  output$stats_bar <- renderUI({
    req(rv$entities)
    rv$citation_refresh  # depend on refresh counter

    total_citations <- dbGetQuery(db_con,
      "SELECT COUNT(*) as n FROM bibliographic_citations")$n
    persons_with <- dbGetQuery(db_con,
      "SELECT COUNT(DISTINCT author_id) as n FROM bibliographic_citations WHERE author_id IS NOT NULL")$n
    works_with <- dbGetQuery(db_con,
      "SELECT COUNT(DISTINCT work_id) as n FROM bibliographic_citations WHERE work_id IS NOT NULL")$n

    div(class = "stats-bar",
      div(class = "stat-item",
        div(class = "stat-value", total_citations),
        div(class = "stat-label", "Total Citations")),
      div(class = "stat-item",
        div(class = "stat-value", persons_with),
        div(class = "stat-label", "Persons with Citations")),
      div(class = "stat-item",
        div(class = "stat-value", works_with),
        div(class = "stat-label", "Works with Citations"))
    )
  })

  # Position display
  output$position_display <- renderUI({
    req(rv$filtered_ids)
    n <- length(rv$filtered_ids)
    idx <- min(rv$current_index, n)
    span(sprintf("%d / %d", idx, n),
         style = "margin: 0 15px; font-weight: bold;")
  })

  # Navigation
  observeEvent(input$prev_entity, {
    if (rv$current_index > 1) rv$current_index <- rv$current_index - 1
  })

  observeEvent(input$next_entity, {
    if (rv$current_index < length(rv$filtered_ids)) {
      rv$current_index <- rv$current_index + 1
    }
  })

  observeEvent(input$entity_select, {
    req(input$entity_select)
    idx <- as.integer(input$entity_select)
    pos <- which(rv$filtered_ids == idx)
    if (length(pos) > 0) rv$current_index <- pos[1]
  })

  # Current entity display
  output$current_entity_display <- renderUI({
    entity <- current_entity()
    req(entity)

    div(class = "entity-card work-mode",
      h4(entity$work_id),
      div(class = "field-row",
        div(class = "field-label", "Title"),
        div(class = "field-value", entity$title %||% "—")),
      if (!is.na(entity$title_arabic) && entity$title_arabic != "") {
        div(class = "field-row",
          div(class = "field-label", "Arabic"),
          div(class = "field-value arabic-text", entity$title_arabic))
      },
      div(class = "field-row",
        div(class = "field-label", "Author"),
        div(class = "field-value", entity$author_name %||% "—")),
      div(class = "field-row",
        div(class = "field-label", "Type"),
        div(class = "field-value", entity$type %||% "—")),
      div(class = "field-row",
        div(class = "field-label", "System"),
        div(class = "field-value", entity$system %||% "—"))
    )
  })

  # Existing citations display
  output$existing_citations_display <- renderUI({
    entity <- current_entity()
    req(entity)
    rv$citation_refresh  # depend on refresh counter

    citations <- dbGetQuery(db_con,
      "SELECT * FROM bibliographic_citations WHERE work_id = ? ORDER BY created_at DESC",
      params = list(entity$work_id))

    if (nrow(citations) == 0) {
      return(p(em("No citations added yet for this entity.")))
    }

    cards <- lapply(seq_len(nrow(citations)), function(i) {
      cit <- citations[i, ]
      card_class <- if (cit$citation_form == "short") {
        "citation-card short-form"
      } else {
        "citation-card"
      }

      link_badge_class <- switch(cit$link_type %||% "reference",
        "reference" = "badge-reference",
        "edition" = "badge-edition",
        "study" = "badge-study",
        "encyclopedia" = "badge-encyclopedia",
        "badge-reference"
      )

      div(class = card_class,
        div(style = "display: flex; justify-content: space-between; align-items: center;",
          span(
            if (cit$citation_form == "long") span(class = "badge-long", "Long")
            else span(class = "badge-short", "Short"),
            " ",
            span(class = link_badge_class, cit$link_type %||% "reference")
          ),
          actionButton(
            paste0("delete_cit_", cit$citation_id),
            icon("trash"),
            class = "btn btn-sm btn-danger",
            onclick = sprintf(
              "Shiny.setInputValue('delete_citation_id', %d, {priority: 'event'})",
              cit$citation_id
            )
          )
        ),
        p(style = "margin-top: 10px;", strong("Text: "), htmltools::htmlEscape(cit$original_text)),
        if (!is.na(cit$parsed_author)) p(strong("Author: "), htmltools::htmlEscape(cit$parsed_author)),
        if (!is.na(cit$parsed_title)) p(strong("Title: "), htmltools::htmlEscape(cit$parsed_title)),
        if (!is.na(cit$volume_cited)) p(strong("Vol: "), htmltools::htmlEscape(cit$volume_cited),
          if (!is.na(cit$page_cited)) paste0(", pp. ", htmltools::htmlEscape(cit$page_cited)) else ""),
        if (!is.na(cit$notes)) p(strong("Notes: "), htmltools::htmlEscape(cit$notes))
      )
    })

    do.call(tagList, cards)
  })

  # Delete citation
  observeEvent(input$delete_citation_id, {
    delete_citation(db_con, input$delete_citation_id)
    rv$citation_refresh <- rv$citation_refresh + 1
    showNotification("Citation deleted", type = "warning")
  })

  # Parse plain text using parse_citation_sequence() as primary entry point
  # This handles: normalization, prose splitting ("Consult further..."),
  # semicolon splitting, type detection, and routing to correct parser
  observeEvent(input$parse_text, {
    req(input$plain_text_input)
    text <- trimws(input$plain_text_input)

    if (input$force_type != "auto") {
      # Forced type: bypass sequence parser, parse single citation
      if (grepl("^long", input$force_type)) {
        parsed <- parse_long_monograph(text)
        parsed$type <- gsub("long_", "", input$force_type)
      } else {
        parsed <- parse_short_citation(text)
        parsed$type <- input$force_type
      }
      rv$parsed_sequence <- list(
        raw = text,
        type = "forced_single",
        n_references = 1,
        references = list(parsed),
        related_references = list(),
        n_related = 0
      )
    } else {
      # Auto-detect: use full citation sequence parser
      rv$parsed_sequence <- parse_citation_sequence(text)
    }
  })

  # Parse preview — show all extracted references
  output$parse_preview <- renderUI({
    req(rv$parsed_sequence)
    seq <- rv$parsed_sequence

    n_main <- seq$n_references
    n_related <- seq$n_related
    n_total <- n_main + n_related

    if (n_total == 0) {
      return(div(class = "parsed-preview",
        h5("No citations extracted"),
        p("The parser could not identify any citations in this text.")
      ))
    }

    # Build preview cards for each reference
    ref_previews <- list()

    if (n_main > 0) {
      ref_previews <- c(ref_previews, list(h5(paste0(n_main, " main reference(s):"))))
      for (i in seq_len(n_main)) {
        ref <- seq$references[[i]]
        ref_previews <- c(ref_previews, list(
          div(style = "background:#e8f4e8; padding:10px; border-radius:4px; margin:5px 0; border-left:3px solid #28a745;",
            strong(paste0("[", i, "] ")),
            if (!is.null(ref$author) && !is.na(ref$author)) span(htmltools::htmlEscape(ref$author), " — ") else NULL,
            if (!is.null(ref$title_abbrev) && !is.na(ref$title_abbrev)) {
              tagList(
                span(htmltools::htmlEscape(ref$title_abbrev)),
                if (!is.null(ref$edition_qualifier) && !is.na(ref$edition_qualifier))
                  span(paste0(" (", ref$edition_qualifier, ")")) else NULL,
                if (!is.null(ref$article_title) && !is.na(ref$article_title))
                  span(paste0(', "', htmltools::htmlEscape(ref$article_title), '"')) else NULL
              )
            } else if (!is.null(ref$title) && !is.na(ref$title)) {
              span(em(htmltools::htmlEscape(ref$title)))
            } else NULL,
            if (!is.null(ref$volume) && !is.na(ref$volume))
              span(paste0(", vol. ", ref$volume)) else NULL,
            if (!is.null(ref$page_german) && !is.na(ref$page_german)) {
              span(paste0(", p. ", ref$page_german, " (de) / ", ref$page_english %||% "?", " (en)"))
            } else if (!is.null(ref$page) && !is.na(ref$page)) {
              span(paste0(", p. ", ref$page))
            } else NULL,
            if (!is.null(ref$entry_number) && !is.na(ref$entry_number))
              span(paste0(" №", ref$entry_number)) else NULL,
            br(),
            tags$small(style = "color:#666;",
              "Type: ", ref$type %||% "unknown",
              " | Primary: ", if (isTRUE(ref$is_primary)) "yes" else "no"
            )
          )
        ))
      }
    }

    if (n_related > 0) {
      ref_previews <- c(ref_previews, list(
        hr(),
        h5(paste0(n_related, " related reference(s) (from \"Consult further\" / \"See also\"):"))
      ))
      for (i in seq_len(n_related)) {
        ref <- seq$related_references[[i]]
        ref_previews <- c(ref_previews, list(
          div(style = "background:#fff3cd; padding:10px; border-radius:4px; margin:5px 0; border-left:3px solid #ffc107;",
            strong(paste0("[R", i, "] ")),
            if (!is.null(ref$author) && !is.na(ref$author)) span(htmltools::htmlEscape(ref$author), " — ") else NULL,
            if (!is.null(ref$title_abbrev) && !is.na(ref$title_abbrev)) {
              tagList(
                span(htmltools::htmlEscape(ref$title_abbrev)),
                if (!is.null(ref$edition_qualifier) && !is.na(ref$edition_qualifier))
                  span(paste0(" (", ref$edition_qualifier, ")")) else NULL,
                if (!is.null(ref$article_title) && !is.na(ref$article_title))
                  span(paste0(', "', htmltools::htmlEscape(ref$article_title), '"')) else NULL
              )
            } else if (!is.null(ref$title) && !is.na(ref$title)) {
              span(em(htmltools::htmlEscape(ref$title)))
            } else NULL,
            if (!is.null(ref$volume) && !is.na(ref$volume))
              span(paste0(", vol. ", ref$volume)) else NULL,
            if (!is.null(ref$page_german) && !is.na(ref$page_german)) {
              span(paste0(", p. ", ref$page_german, " (de) / ", ref$page_english %||% "?", " (en)"))
            } else if (!is.null(ref$page) && !is.na(ref$page)) {
              span(paste0(", p. ", ref$page))
            } else NULL,
            if (!is.null(ref$entry_number) && !is.na(ref$entry_number))
              span(paste0(" №", ref$entry_number)) else NULL,
            br(),
            tags$small(style = "color:#666;",
              "Type: ", ref$type %||% "unknown",
              " | Primary: ", if (isTRUE(ref$is_primary)) "yes" else "no"
            )
          )
        ))
      }
    }

    # Also show raw parse structure for debugging
    ref_previews <- c(ref_previews, list(
      hr(),
      tags$details(
        tags$summary(style = "cursor:pointer; color:#666;", "Show raw parse output"),
        verbatimTextOutput("parse_json")
      )
    ))

    div(class = "parsed-preview", do.call(tagList, ref_previews))
  })

  output$parse_json <- renderPrint({
    req(rv$parsed_sequence)
    str(rv$parsed_sequence, max.level = 3)
  })

  # Save all parsed citations (main + related) as separate DB records
  observeEvent(input$save_citation, {
    entity <- current_entity()
    req(entity, rv$parsed_sequence)

    work_id <- entity$work_id
    author_id <- entity$author_id

    seq <- rv$parsed_sequence
    all_refs <- c(seq$references, seq$related_references)

    if (length(all_refs) == 0) {
      showNotification("No citations to save", type = "warning")
      return()
    }

    saved <- 0
    for (ref in all_refs) {
      # Expand monograph_equality editions into separate saves
      if (!is.null(ref$editions) && length(ref$editions) > 1) {
        for (ed in ref$editions) {
          ed$author <- ed$author %||% ref$author
          ed$title <- ed$title %||% ref$title
          ed$raw <- ref$raw
          tryCatch({
            save_citation(
              db_con,
              work_id = work_id,
              author_id = author_id,
              original_text = ref$raw %||% trimws(input$plain_text_input),
              parsed = ed,
              citation_form = "long",
              citation_type = "edition",
              link_type = input$link_type,
              source_type = "plain_text",
              comment = input$citation_comment
            )
            saved <- saved + 1
          }, error = function(e) {
            showNotification(paste("Error saving edition:", e$message), type = "error")
          })
        }
        next
      }

      # Determine form from ref type
      citation_form <- if (grepl("^short|^secondary|^serial", ref$type %||% "")) {
        "short"
      } else {
        "long"
      }

      tryCatch({
        save_citation(
          db_con,
          work_id = work_id,
          author_id = author_id,
          original_text = ref$raw %||% trimws(input$plain_text_input),
          parsed = ref,
          citation_form = citation_form,
          citation_type = ref$type %||% "unknown",
          link_type = input$link_type,
          source_type = "plain_text",
          comment = input$citation_comment
        )
        saved <- saved + 1
      }, error = function(e) {
        showNotification(paste("Error saving ref:", e$message), type = "error")
      })
    }

    rv$citation_refresh <- rv$citation_refresh + 1
    updateTextAreaInput(session, "plain_text_input", value = "")
    updateTextAreaInput(session, "citation_comment", value = "")
    rv$parsed_sequence <- NULL
    showNotification(paste(saved, "citation(s) saved"), type = "message")
  })

  # BibTeX import
  observeEvent(input$bib_file, {
    req(input$bib_file)
    content <- paste(readLines(input$bib_file$datapath, warn = FALSE), collapse = "\n")
    # Reuse parser from bibliographic_reference_manager
    entries <- list()
    entry_pattern <- "@(\\w+)\\s*\\{\\s*([^,]+)\\s*,([^@]+)\\}"
    matches <- gregexpr(entry_pattern, content, perl = TRUE)
    entry_texts <- regmatches(content, matches)[[1]]
    for (entry_text in entry_texts) {
      entry <- list(raw = entry_text, source = "bib")
      type_match <- regmatches(entry_text, regexpr("^@(\\w+)", entry_text))
      if (length(type_match) > 0) entry$bib_type <- tolower(gsub("^@", "", type_match))
      field_pattern <- "(\\w+)\\s*=\\s*[{\"]((?:[^{}]|\\{[^{}]*\\})*)[}\"]"
      field_matches <- gregexpr(field_pattern, entry_text, perl = TRUE)
      field_texts <- regmatches(entry_text, field_matches)[[1]]
      for (ft in field_texts) {
        parts <- strsplit(ft, "\\s*=\\s*")[[1]]
        if (length(parts) >= 2) {
          entry[[tolower(trimws(parts[1]))]] <- trimws(gsub("^[{\"']|[}\"']$", "", parts[2]))
        }
      }
      entries <- append(entries, list(entry))
    }
    rv$bib_entries <- entries
  })

  output$bib_preview <- renderUI({
    req(rv$bib_entries)
    if (length(rv$bib_entries) == 0) return(p("No entries found."))
    lapply(seq_along(rv$bib_entries), function(i) {
      entry <- rv$bib_entries[[i]]
      div(style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 4px;",
        checkboxInput(paste0("bib_select_", i),
                      paste0("[", entry$bib_type %||% "?", "] ", entry$title %||% "Untitled"),
                      value = TRUE)
      )
    })
  })

  observeEvent(input$add_bib_refs, {
    entity <- current_entity()
    req(entity, rv$bib_entries)
    work_id <- entity$work_id
    author_id <- entity$author_id

    added <- 0
    for (i in seq_along(rv$bib_entries)) {
      if (isTRUE(input[[paste0("bib_select_", i)]])) {
        entry <- rv$bib_entries[[i]]
        parsed <- list(
          author = entry$author, title = entry$title,
          editor = entry$editor, place = entry$address,
          publisher = entry$publisher,
          year_gregorian = entry$year,
          volume = entry$volume, page = entry$pages
        )
        tryCatch({
          save_citation(db_con, work_id, author_id, entry$raw,
                        parsed, "long", entry$bib_type %||% "unknown",
                        input$bib_link_type, "bib")
          added <- added + 1
        }, error = function(e) NULL)
      }
    }
    rv$citation_refresh <- rv$citation_refresh + 1
    showNotification(paste(added, "BibTeX entries added"), type = "message")
  })

  # RIS import
  observeEvent(input$ris_file, {
    req(input$ris_file)
    content <- paste(readLines(input$ris_file$datapath, warn = FALSE), collapse = "\n")
    entries <- list()
    current_entry <- NULL
    for (line in strsplit(content, "\n")[[1]]) {
      line <- trimws(line)
      if (line == "" || !grepl("^[A-Z][A-Z0-9]\\s+-", line)) next
      tag <- substr(line, 1, 2)
      value <- trimws(sub("^[A-Z][A-Z0-9]\\s+-\\s*", "", line))
      if (tag == "TY") {
        if (!is.null(current_entry)) entries <- append(entries, list(current_entry))
        current_entry <- list(raw = "", source = "ris", ris_type = value)
      } else if (tag == "ER") {
        if (!is.null(current_entry)) {
          entries <- append(entries, list(current_entry))
          current_entry <- NULL
        }
      } else if (!is.null(current_entry)) {
        field_map <- list(
          AU = "author", TI = "title", T1 = "title", PY = "year",
          VL = "volume", SP = "start_page", EP = "end_page",
          PB = "publisher", CY = "place", ED = "editor"
        )
        if (tag %in% names(field_map)) {
          fn <- field_map[[tag]]
          if (fn %in% names(current_entry)) {
            current_entry[[fn]] <- paste(current_entry[[fn]], value, sep = "; ")
          } else {
            current_entry[[fn]] <- value
          }
        }
        current_entry$raw <- paste(current_entry$raw, line, sep = "\n")
      }
    }
    if (!is.null(current_entry)) entries <- append(entries, list(current_entry))
    rv$ris_entries <- entries
  })

  output$ris_preview <- renderUI({
    req(rv$ris_entries)
    if (length(rv$ris_entries) == 0) return(p("No entries found."))
    lapply(seq_along(rv$ris_entries), function(i) {
      entry <- rv$ris_entries[[i]]
      div(style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0; border-radius: 4px;",
        checkboxInput(paste0("ris_select_", i),
                      paste0("[", entry$ris_type %||% "?", "] ", entry$title %||% "Untitled"),
                      value = TRUE)
      )
    })
  })

  observeEvent(input$add_ris_refs, {
    entity <- current_entity()
    req(entity, rv$ris_entries)
    work_id <- entity$work_id
    author_id <- entity$author_id

    added <- 0
    for (i in seq_along(rv$ris_entries)) {
      if (isTRUE(input[[paste0("ris_select_", i)]])) {
        entry <- rv$ris_entries[[i]]
        parsed <- list(
          author = entry$author, title = entry$title,
          editor = entry$editor, place = entry$place,
          publisher = entry$publisher,
          year_gregorian = entry$year,
          volume = entry$volume,
          page = paste0(entry$start_page %||% "", if (!is.null(entry$end_page)) paste0("-", entry$end_page) else "")
        )
        tryCatch({
          save_citation(db_con, work_id, author_id, entry$raw,
                        parsed, "long", entry$ris_type %||% "unknown",
                        input$ris_link_type, "ris")
          added <- added + 1
        }, error = function(e) NULL)
      }
    }
    rv$citation_refresh <- rv$citation_refresh + 1
    showNotification(paste(added, "RIS entries added"), type = "message")
  })

  # Manual entry
  observeEvent(input$add_manual_ref, {
    entity <- current_entity()
    req(entity)
    work_id <- entity$work_id
    author_id <- entity$author_id

    raw_text <- paste(
      input$manual_author, input$manual_title,
      sep = ", "
    )
    parsed <- list(
      author = input$manual_author, title = input$manual_title,
      editor = input$manual_editor, place = input$manual_place,
      publisher = input$manual_publisher,
      year_gregorian = input$manual_year,
      volume_cited = input$manual_volume,
      page_cited = input$manual_page,
      entry_number = input$manual_entry,
      notes = input$manual_notes
    )

    tryCatch({
      save_citation(db_con, work_id, author_id, raw_text,
                    parsed, "long", input$manual_type,
                    input$manual_link_type, "manual")
      rv$citation_refresh <- rv$citation_refresh + 1
      showNotification("Manual citation saved", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Export RIS
  observeEvent(input$export_ris, {
    output_path <- file.path("data", "mashriq-maghrib_bibliography.ris")
    tryCatch({
      export_all_ris(DB_PATH, output_path)
      showNotification(
        paste("RIS exported to", output_path),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Export error:", e$message), type = "error")
    })
  })
}

# ============================================================================
# Run App
# ============================================================================

shinyApp(ui = ui, server = server)
