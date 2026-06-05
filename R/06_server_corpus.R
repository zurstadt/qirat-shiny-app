# Corpus Explorer server logic

server_corpus <- function(input, output, session, rv) {

  # Update work type filter choices based on data
  observe({
    req(rv$raw_data)
    raw_types <- rv$raw_data$type
    raw_types <- raw_types[!is.na(raw_types) & raw_types != ""]
    all_types <- unlist(strsplit(raw_types, "[;,]"))
    all_types <- trimws(all_types)
    all_types <- all_types[all_types != ""]
    unique_types <- sort(unique(all_types))
    updateSelectizeInput(session, "filter_type", choices = unique_types, server = TRUE)
  })

  # Clear filters button
  observeEvent(input$clear_filters, {
    updateTextInput(session, "search_all", value = "")
    updateSelectizeInput(session, "filter_system", selected = character(0))
    updateSelectizeInput(session, "filter_region", selected = character(0))
    updateSelectizeInput(session, "filter_type", selected = character(0))
    updateSliderInput(session, "filter_century", value = c(4, 7))
  })

  # Navigation from Home page links
  observeEvent(input$nav_to, {
    updateTabsetPanel(session, "tabs", selected = input$nav_to)
  })

  # Filtered corpus data (reactive)
  filtered_corpus <- reactive({
    req(rv$raw_data)
    df <- rv$raw_data

    # Apply combined search (title OR author) using elastic search
    if (!is.null(input$search_all) && input$search_all != "") {
      title_matches <- elastic_match_vec(input$search_all, df$title, df$title_arabic)
      author_matches <- elastic_match_vec(input$search_all, df$author_name, df$author_name_arabic)
      df <- df[title_matches | author_matches, ]
    }

    # Filter by reading set (multi-select)
    if (!is.null(input$filter_system) && length(input$filter_system) > 0) {
      df <- df %>% filter(`set` %in% input$filter_system)
    }

    # Filter by region (multi-select)
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      region_filter <- rep(FALSE, nrow(df))
      for (reg in input$filter_region) {
        if (reg == "inter-regional") {
          region_filter <- region_filter | (
            grepl("ma\u0121rib", df$regionality, ignore.case = TRUE) &
            grepl("ma\u0161riq", df$regionality, ignore.case = TRUE)
          )
        } else {
          region_filter <- region_filter | grepl(paste0("^", reg), df$regionality, ignore.case = TRUE)
        }
      }
      df <- df[region_filter, ]
    }

    # Filter by work type (multi-select)
    if (!is.null(input$filter_type) && length(input$filter_type) > 0) {
      type_filter <- sapply(df$type, function(t) {
        if (is.na(t) || t == "") return(FALSE)
        any(sapply(input$filter_type, function(ft) grepl(ft, t, fixed = TRUE)))
      })
      df <- df[type_filter, ]
    }

    if (!is.null(input$filter_century)) {
      df <- df %>% filter(death_century >= input$filter_century[1] &
                         death_century <= input$filter_century[2])
    }

    df
  })

  # Results count
  output$corpus_results_count <- renderUI({
    df <- filtered_corpus()
    total <- nrow(rv$raw_data)
    filtered <- nrow(df)

    div(class = "corpus-search-results",
      if (filtered == total) {
        sprintf("Showing all %d works", total)
      } else {
        sprintf("Showing %d of %d works", filtered, total)
      }
    )
  })

  # Enhanced data table with formatted titles and color badges
  output$enhanced_data_table <- renderDT({
    df <- filtered_corpus()
    req(nrow(df) > 0)

    # Helper to create recycle symbol with hover text
    create_text_reuse_symbol <- function(text_reuse, reused_title, reused_author,
                                         commentary_titles, commentary_types, commentary_authors) {
      symbols <- c()

      if (!is.na(text_reuse) && text_reuse == 1 && !is.na(reused_title)) {
        reused_title_fmt <- format_camel_case(reused_title, "title")
        reused_author_fmt <- format_camel_case(reused_author, "author")
        hover_text <- paste0("Reuses ", reused_title_fmt, " by ", reused_author_fmt)
        symbols <- c(symbols, sprintf(
          '<span class="text-reuse-tooltip" data-tooltip="%s" style="cursor: help; color: #28a745; margin-left: 5px;">&#x267B;</span>',
          htmltools::htmlEscape(hover_text)
        ))
      }

      if (!is.na(commentary_titles) && commentary_titles != "") {
        titles <- strsplit(commentary_titles, "\\|")[[1]]
        types <- if (!is.na(commentary_types) && commentary_types != "") {
          strsplit(commentary_types, "\\|")[[1]]
        } else {
          rep("commentary", length(titles))
        }
        authors <- if (!is.na(commentary_authors) && commentary_authors != "") {
          strsplit(commentary_authors, "\\|")[[1]]
        } else {
          rep("", length(titles))
        }

        commentary_lines <- sapply(seq_along(titles), function(i) {
          title_fmt <- format_camel_case(titles[i], "title")
          author_fmt <- if (!is.na(authors[i]) && authors[i] != "") {
            format_camel_case(authors[i], "author")
          } else {
            "Unknown"
          }
          raw_type <- if (i <= length(types) && !is.na(types[i]) && types[i] != "") types[i] else "unknown"
          type_label <- gsub("_", " ", raw_type)
          paste0("\u2022 ", title_fmt, " (", type_label, ") by ", author_fmt)
        })

        n_works <- length(commentary_lines)
        header <- paste0("Subject of ", n_works, " dependent work", ifelse(n_works > 1, "s", ""), ":")
        hover_text <- paste0(header, "\n", paste(commentary_lines, collapse = "\n"))

        symbols <- c(symbols, sprintf(
          '<span class="text-reuse-tooltip" data-tooltip="%s" style="cursor: help; color: #0066cc; margin-left: 5px; font-size: 1.1em;">&#x267B;</span>',
          htmltools::htmlEscape(hover_text)
        ))
      }

      paste(symbols, collapse = "")
    }

    # Format display data
    display_df <- df %>%
      rowwise() %>%
      mutate(
        reuse_symbol = create_text_reuse_symbol(text_reuse, reused_title, reused_author,
                                                commentary_titles, commentary_types, commentary_authors),
        cite_indicator = "",
        Title = paste0(
          '<a href="#" class="citation-link" onclick="Shiny.setInputValue(\'clicked_work\', \'',
          htmltools::htmlEscape(work_id),
          '\', {priority: \'event\'}); return false;">',
          htmltools::htmlEscape(format_camel_case(title, "title")),
          '</a>',
          reuse_symbol,
          cite_indicator
        )
      ) %>%
      ungroup() %>%
      mutate(
        author_cite_indicator = "",
        Author = paste0(
          '<a href="#" class="citation-link" onclick="Shiny.setInputValue(\'clicked_author\', ',
          as.integer(author_id),
          ', {priority: \'event\'}); return false;">',
          sapply(author_name, function(x) htmltools::htmlEscape(format_camel_case(x, "author"))),
          '</a>',
          author_cite_indicator
        ),
        Set = sapply(`set`, function(x) create_color_badge(x, "set")),
        Type = sapply(type, function(x) {
          if (is.na(x) || x == "") return("")
          if (grepl("[;,]", x)) {
            types <- trimws(strsplit(x, "[;,]")[[1]])
            types <- types[types != ""]
            paste(sapply(types, function(t) create_color_badge(t, "type")), collapse = " ")
          } else {
            create_color_badge(x, "type")
          }
        }),
        Origin = sapply(regionality, function(x) {
          if (is.na(x) || x == "") return("")
          x_lower <- tolower(x)
          if (grepl("ma\u0121rib visits ma\u0161riq", x_lower) || grepl("maghrib visits mashriq", x_lower)) {
            create_color_badge("Ma\u0121rib visits Ma\u0161riq", "region")
          } else if (grepl("ma\u0161riq visits ma\u0121rib", x_lower) || grepl("mashriq visits maghrib", x_lower)) {
            create_color_badge("Ma\u0161riq visits Ma\u0121rib", "region")
          } else if (grepl("^ma\u0121rib", x_lower) || grepl("^maghrib", x_lower)) {
            create_color_badge("Ma\u0121rib", "region")
          } else if (grepl("^ma\u0161riq", x_lower) || grepl("^mashriq", x_lower)) {
            create_color_badge("Ma\u0161riq", "region")
          } else {
            x
          }
        }),
        Century = as.character(death_century)
      ) %>%
      select(Title, Author, Set, Type, Origin, Century)

    datatable(
      display_df,
      escape = FALSE,
      caption = htmltools::tags$caption(style = "caption-side:bottom;text-align:left;font-size:0.85em;color:#888;padding-top:4px;",
        "Click on a title or author name to view citations and details."),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',
        columnDefs = list(
          list(className = 'dt-left', targets = c(0, 1))
        )
      ),
      rownames = FALSE
    )
  })

  # Shared export shaping for the CSV/JSON download handlers
  build_export_df <- function(df) {
    df %>%
      mutate(
        title_formatted = sapply(title, function(x) format_camel_case(x, "title")),
        author_formatted = sapply(author_name, function(x) format_camel_case(x, "author"))
      ) %>%
      select(work_id, title = title_formatted, author = author_formatted,
             reading_set = `set`, type, region = regionality, death_century)
  }

  # 0-row export skeleton with the correct columns (used when filters match nothing)
  empty_export_df <- function() {
    data.frame(work_id = integer(), title = character(), author = character(),
               reading_set = character(), type = character(), region = character(),
               death_century = integer())
  }

  # Download handlers for CSV and JSON
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("iqsa_corpus_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- filtered_corpus()
      if (nrow(df) == 0) {
        showNotification("No works match the current filters — nothing to export.", type = "warning")
        write.csv(empty_export_df(), file, row.names = FALSE)
        return(invisible())
      }
      write.csv(build_export_df(df), file, row.names = FALSE)
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      paste0("iqsa_corpus_", format(Sys.Date(), "%Y%m%d"), ".json")
    },
    content = function(file) {
      df <- filtered_corpus()
      if (nrow(df) == 0) {
        showNotification("No works match the current filters — nothing to export.", type = "warning")
        write(jsonlite::toJSON(empty_export_df(), pretty = TRUE), file)
        return(invisible())
      }
      write(jsonlite::toJSON(build_export_df(df), pretty = TRUE), file)
    }
  )
}
