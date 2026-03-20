server_citations <- function(input, output, session, rv) {

  # ========== Citation Modal Observers ==========

  # Helper: look up digital edition URL from concordance tables
  lookup_digital_url <- function(con, parsed_title, page_cited, entry_number) {
    if (is.null(parsed_title) || is.na(parsed_title)) return(NULL)
    if (is.null(page_cited) || is.na(page_cited)) return(NULL)

    if (parsed_title == "\u0120N") {
      # ĠN concordance: match on page and/or entry_number
      if (!is.null(entry_number) && !is.na(entry_number) && nchar(entry_number) > 0) {
        result <- dbGetQuery(con, "
          SELECT url FROM gn_url_concordance
          WHERE page = ? AND entry_number = ?
          LIMIT 1
        ", params = list(page_cited, entry_number))
      } else {
        result <- dbGetQuery(con, "
          SELECT url FROM gn_url_concordance
          WHERE page = ?
          LIMIT 1
        ", params = list(page_cited))
      }
      if (nrow(result) > 0) return(result$url[1])
    } else if (parsed_title == "MQK") {
      # MQK concordance: match on page and/or entry_number
      if (!is.null(entry_number) && !is.na(entry_number) && nchar(entry_number) > 0) {
        result <- dbGetQuery(con, "
          SELECT url FROM mqk_url_concordance
          WHERE page = ? AND entry_number = ?
          LIMIT 1
        ", params = list(page_cited, entry_number))
      } else {
        result <- dbGetQuery(con, "
          SELECT url FROM mqk_url_concordance
          WHERE page = ?
          LIMIT 1
        ", params = list(page_cited))
      }
      if (nrow(result) > 0) return(result$url[1])
    }
    NULL
  }

  # Work citation modal
  observeEvent(input$clicked_work, {
    work_id <- input$clicked_work
    tryCatch({
      con <- dbConnect(SQLite(), DB_PATH)
      on.exit(dbDisconnect(con))

      citations <- dbGetQuery(con, "
        SELECT * FROM bibliographic_citations
        WHERE work_id = ?
        ORDER BY created_at DESC
      ", params = list(work_id))

      # Get work title for modal header
      work_info <- dbGetQuery(con, "SELECT title FROM works WHERE work_id = ?",
                              params = list(work_id))
      work_title <- if (nrow(work_info) > 0) work_info$title[1] else work_id

      if (nrow(citations) == 0) {
        modal_content <- p(em("No citations have been added for this work yet."),
                          br(), br(),
                          "Use the Citation Annotation app to add citations.")
        modal_footer <- modalButton("Close")
      } else {
        # Store citations for export
        rv$modal_citations <- citations

        # Classify each citation as primary or secondary
        # Schema abbreviations are primary; unrecognized full titles (long names, likely editions) also primary
        all_schema_abbrevs <- if (!is.null(WORK_SCHEMAS)) WORK_SCHEMAS$abbrev else PRIMARY_ABBREVS
        is_primary <- vapply(citations$parsed_title, function(pt) {
          if (is.null(pt) || is.na(pt)) return(FALSE)
          if (pt %in% PRIMARY_ABBREVS) return(TRUE)
          if (!pt %in% all_schema_abbrevs && nchar(pt) > 20) return(TRUE)
          FALSE
        }, logical(1))

        # JS-safe string escaper
        js_escape <- function(s) {
          s <- gsub("\\\\", "\\\\\\\\", s)
          s <- gsub("'", "\\\\'", s)
          s <- gsub("\n", "\\\\n", s)
          s <- gsub("\r", "", s)
          s
        }

        # Group citations by parsed_title (same approach as author modal)
        titles <- unique(citations$parsed_title[!is.na(citations$parsed_title)])
        primary_titles <- titles[vapply(titles, function(tt) {
          any(is_primary[!is.na(citations$parsed_title) & citations$parsed_title == tt])
        }, logical(1))]
        secondary_titles <- setdiff(titles, primary_titles)

        # Build a grouped card for one title (shared logic)
        build_work_title_group <- function(title, rows, border_color) {
          author <- rows$parsed_author[1]
          author_label <- if (!is.na(author) && nchar(author) > 0) paste0(htmltools::htmlEscape(author), ", ") else ""

          # Deduplicate rows with identical volume + page
          dedup_key <- paste0(
            ifelse(is.na(rows$volume_cited), "", rows$volume_cited), "|",
            ifelse(is.na(rows$page_cited), "", rows$page_cited)
          )
          keep_idx <- !duplicated(dedup_key)
          for (dk in unique(dedup_key[duplicated(dedup_key)])) {
            dup_rows <- which(dedup_key == dk)
            has_detail <- vapply(dup_rows, function(r) {
              (!is.na(rows$entry_number[r]) && nchar(rows$entry_number[r]) > 0) +
              (!is.na(rows$section[r]) && nchar(rows$section[r]) > 0) +
              (!is.na(rows$notes[r]) && nchar(rows$notes[r]) > 0)
            }, integer(1))
            best <- dup_rows[which.max(has_detail)]
            keep_idx[dup_rows] <- FALSE
            keep_idx[best] <- TRUE
          }
          rows <- rows[keep_idx, , drop = FALSE]

          # Build page reference lines
          page_lines <- lapply(seq_len(nrow(rows)), function(j) {
            row <- rows[j, ]
            parts <- c()
            if (!is.na(row$volume_cited) && nchar(row$volume_cited) > 0) parts <- c(parts, paste0(row$volume_cited, ":"))
            if (!is.na(row$page_cited) && nchar(row$page_cited) > 0) parts <- c(parts, htmltools::htmlEscape(row$page_cited))
            ref <- paste0(parts, collapse = "")
            if (!is.na(row$entry_number) && nchar(row$entry_number) > 0) ref <- paste0(ref, " \u2116", htmltools::htmlEscape(row$entry_number))
            if (!is.na(row$section) && nchar(row$section) > 0) ref <- paste0(ref, " (", htmltools::htmlEscape(row$section), ")")

            # GdQ/GAL edition pages
            edition_note <- NULL
            if (!is.na(row$page_german) && nchar(row$page_german) > 0 &&
                !is.na(row$page_english) && nchar(row$page_english) > 0) {
              edition_note <- span(style = "color:#888;font-size:0.85em;margin-left:6px;",
                paste0("[Ger. p. ", htmltools::htmlEscape(row$page_german),
                       " / Eng. p. ", htmltools::htmlEscape(row$page_english), "]"))
            } else if (!is.na(row$page_german) && nchar(row$page_german) > 0) {
              edition_note <- span(style = "color:#888;font-size:0.85em;margin-left:6px;",
                paste0("[Ger. p. ", htmltools::htmlEscape(row$page_german), "]"))
            } else if (!is.na(row$page_english) && nchar(row$page_english) > 0) {
              edition_note <- span(style = "color:#888;font-size:0.85em;margin-left:6px;",
                paste0("[Eng. p. ", htmltools::htmlEscape(row$page_english), "]"))
            }

            notes_span <- if (!is.na(row$notes) && nchar(row$notes) > 0) {
              span(style = "color:#888;font-size:0.85em;font-style:italic;margin-left:6px;",
                   paste0("(", htmltools::htmlEscape(row$notes), ")"))
            }

            digital_url <- lookup_digital_url(con, row$parsed_title, row$page_cited, row$entry_number)
            link_span <- if (!is.null(digital_url)) {
              a(href = digital_url, target = "_blank", rel = "noopener noreferrer",
                style = "font-size:0.85em;color:#17a2b8;text-decoration:none;margin-left:4px;",
                "\u2197")
            }

            tagList(span(HTML(ref)), edition_note, notes_span, link_span)
          })

          # For edition entries with no page refs, show truncated original_text
          has_any_ref <- any(vapply(seq_len(nrow(rows)), function(j) {
            !is.na(rows$page_cited[j]) && nchar(rows$page_cited[j]) > 0
          }, logical(1)))

          if (!has_any_ref && nrow(rows) > 0) {
            ref_display <- div(style = "margin-top:4px;color:#444;font-size:0.9em;",
              p(style = "margin:0;", htmltools::htmlEscape(
                substr(rows$original_text[1], 1, 200)
              ), if (nchar(rows$original_text[1]) > 200) "...")
            )
          } else if (length(page_lines) == 1) {
            ref_display <- div(style = "margin-top:4px;color:#444;", page_lines[[1]])
          } else {
            ref_display <- tags$ul(style = "margin-top:4px;margin-bottom:0;padding-left:20px;color:#444;",
              lapply(page_lines, function(pl) tags$li(style = "margin-bottom:2px;", pl))
            )
          }

          # Copyable text for this group
          group_texts <- unique(rows$original_text[!is.na(rows$original_text)])
          group_copy <- js_escape(paste(group_texts, collapse = "\n"))

          div(style = paste0("background:#f8f9fa;padding:12px;border-radius:6px;margin-bottom:10px;border-left:4px solid ", border_color, ";"),
            div(span(style = "font-weight:600;font-size:1.05em;", HTML(paste0(author_label, htmltools::htmlEscape(title)))),
                HTML(paste0('<button class="copy-btn" onclick="copyCitationText(\'', group_copy, '\', this)" title="Copy this citation">\u2398</button>'))),
            ref_display
          )
        }

        # Build primary and secondary sections with copy-section buttons
        sections <- tagList()

        if (length(primary_titles) > 0) {
          primary_cards <- lapply(primary_titles, function(tt) {
            rows <- citations[!is.na(citations$parsed_title) & citations$parsed_title == tt, , drop = FALSE]
            build_work_title_group(tt, rows, "#0072B2")
          })
          primary_text <- js_escape(paste(unique(citations$original_text[is_primary & !is.na(citations$original_text)]), collapse = "\n\n"))
          sections <- tagList(sections,
            h5(style = "color:#0072B2;margin-top:8px;", "Primary Sources",
               span(style = "font-size:0.8em;font-weight:normal;color:#666;margin-left:8px;",
                    paste0("(", length(primary_titles), " works)")),
               HTML(paste0('<button class="copy-section-btn" onclick="copyCitationText(\'', primary_text, '\', this)" title="Copy all primary citations">Copy Section</button>'))),
            do.call(tagList, primary_cards)
          )
        }
        if (length(secondary_titles) > 0) {
          secondary_cards <- lapply(secondary_titles, function(tt) {
            rows <- citations[!is.na(citations$parsed_title) & citations$parsed_title == tt, , drop = FALSE]
            build_work_title_group(tt, rows, "#E69F00")
          })
          secondary_text <- js_escape(paste(unique(citations$original_text[!is_primary & !is.na(citations$original_text)]), collapse = "\n\n"))
          sections <- tagList(sections,
            h5(style = "color:#E69F00;margin-top:16px;", "Secondary Sources",
               span(style = "font-size:0.8em;font-weight:normal;color:#666;margin-left:8px;",
                    paste0("(", length(secondary_titles), " works)")),
               HTML(paste0('<button class="copy-section-btn" onclick="copyCitationText(\'', secondary_text, '\', this)" title="Copy all secondary citations">Copy Section</button>'))),
            do.call(tagList, secondary_cards)
          )
        }

        modal_content <- tagList(
          p(strong(nrow(citations)), " citation(s)"),
          sections
        )

        # Build export footer with download buttons
        modal_footer <- tagList(
          downloadButton("modal_export_ris", "Export RIS", class = "btn-info btn-sm"),
          downloadButton("modal_export_bibtex", "Export BibTeX", class = "btn-info btn-sm"),
          actionButton("modal_copy_all", "Copy All", class = "btn-outline-secondary btn-sm",
                       onclick = paste0(
                         "var texts = ", jsonlite::toJSON(citations$original_text, auto_unbox = FALSE), ";",
                         "navigator.clipboard.writeText(texts.join('\\n\\n')).then(function(){",
                         "Shiny.setInputValue('copy_notify', Math.random());",
                         "});"
                       )),
          modalButton("Close")
        )
      }

      showModal(modalDialog(
        title = htmltools::htmlEscape(paste0("Citations: ", format_camel_case(work_title, "title"))),
        modal_content,
        size = "l",
        easyClose = TRUE,
        footer = modal_footer
      ))
    }, error = function(e) {
      showNotification(paste("Error loading citations:", e$message), type = "error")
    })
  })

  # Copy notification
  observeEvent(input$copy_notify, {
    showNotification("Citations copied to clipboard", type = "message", duration = 3)
  })

  # RIS export from citation modal
  output$modal_export_ris <- downloadHandler(
    filename = function() {
      paste0("citations_", format(Sys.time(), "%Y%m%d"), ".ris")
    },
    content = function(file) {
      cits <- rv$modal_citations
      if (is.null(cits) || nrow(cits) == 0) {
        writeLines("TY  - GEN\nTI  - No citations available\nER  - \n", file)
        return()
      }
      ris_records <- vapply(seq_len(nrow(cits)), function(i) {
        row <- cits[i, ]
        lines <- c("TY  - GEN")
        if (!is.na(row$parsed_author) && nchar(row$parsed_author) > 0) lines <- c(lines, paste0("AU  - ", row$parsed_author))
        if (!is.na(row$parsed_title) && nchar(row$parsed_title) > 0) lines <- c(lines, paste0("TI  - ", row$parsed_title))
        if (!is.na(row$parsed_editor) && nchar(row$parsed_editor) > 0) lines <- c(lines, paste0("A2  - ", row$parsed_editor))
        if (!is.na(row$parsed_year) && nchar(row$parsed_year) > 0) lines <- c(lines, paste0("PY  - ", row$parsed_year))
        if (!is.na(row$volume_cited) && nchar(row$volume_cited) > 0) lines <- c(lines, paste0("VL  - ", row$volume_cited))
        if (!is.na(row$page_cited) && nchar(row$page_cited) > 0) {
          pages <- strsplit(as.character(row$page_cited), "[\u2013-]")[[1]]
          lines <- c(lines, paste0("SP  - ", trimws(pages[1])))
          if (length(pages) > 1) lines <- c(lines, paste0("EP  - ", trimws(pages[2])))
        }
        if (!is.na(row$parsed_publisher) && nchar(row$parsed_publisher) > 0) lines <- c(lines, paste0("PB  - ", row$parsed_publisher))
        if (!is.na(row$parsed_place) && nchar(row$parsed_place) > 0) lines <- c(lines, paste0("CY  - ", row$parsed_place))
        if (!is.na(row$entry_number) && nchar(row$entry_number) > 0) lines <- c(lines, paste0("N1  - Entry: \u2116", row$entry_number))
        lines <- c(lines, "ER  - ")
        paste(lines, collapse = "\n")
      }, character(1))
      writeLines(paste(ris_records, collapse = "\n\n"), file)
    }
  )

  # BibTeX export from citation modal
  output$modal_export_bibtex <- downloadHandler(
    filename = function() {
      paste0("citations_", format(Sys.time(), "%Y%m%d"), ".bib")
    },
    content = function(file) {
      cits <- rv$modal_citations
      if (is.null(cits) || nrow(cits) == 0) {
        writeLines("% No citations available", file)
        return()
      }
      bib_records <- vapply(seq_len(nrow(cits)), function(i) {
        row <- cits[i, ]
        # Generate a cite key from title abbreviation + year
        key_title <- gsub("[^A-Za-z0-9]", "", substr(row$parsed_title %||% "unknown", 1, 15))
        key_year <- if (!is.na(row$parsed_year)) row$parsed_year else "nd"
        cite_key <- paste0(key_title, key_year, "_", i)
        fields <- c()
        if (!is.na(row$parsed_author) && nchar(row$parsed_author) > 0) fields <- c(fields, paste0("  author = {", row$parsed_author, "}"))
        if (!is.na(row$parsed_title) && nchar(row$parsed_title) > 0) fields <- c(fields, paste0("  title = {", row$parsed_title, "}"))
        if (!is.na(row$parsed_editor) && nchar(row$parsed_editor) > 0) fields <- c(fields, paste0("  editor = {", row$parsed_editor, "}"))
        if (!is.na(row$parsed_year) && nchar(row$parsed_year) > 0) fields <- c(fields, paste0("  year = {", row$parsed_year, "}"))
        if (!is.na(row$volume_cited) && nchar(row$volume_cited) > 0) fields <- c(fields, paste0("  volume = {", row$volume_cited, "}"))
        if (!is.na(row$page_cited) && nchar(row$page_cited) > 0) {
          pages <- gsub("\u2013", "--", row$page_cited)
          fields <- c(fields, paste0("  pages = {", pages, "}"))
        }
        if (!is.na(row$parsed_publisher) && nchar(row$parsed_publisher) > 0) fields <- c(fields, paste0("  publisher = {", row$parsed_publisher, "}"))
        if (!is.na(row$parsed_place) && nchar(row$parsed_place) > 0) fields <- c(fields, paste0("  address = {", row$parsed_place, "}"))
        paste0("@book{", cite_key, ",\n", paste(fields, collapse = ",\n"), "\n}")
      }, character(1))
      writeLines(paste(bib_records, collapse = "\n\n"), file)
    }
  )

  # Author citation modal
  observeEvent(input$clicked_author, {
    author_id <- as.integer(input$clicked_author)
    tryCatch({
      con <- dbConnect(SQLite(), DB_PATH)
      on.exit(dbDisconnect(con))

      citations <- dbGetQuery(con, "
        SELECT * FROM bibliographic_citations
        WHERE author_id = ?
        ORDER BY created_at DESC
      ", params = list(author_id))

      # Get author name for modal header
      author_info <- dbGetQuery(con, "
        SELECT COALESCE(author_name_canonical, author_name) as name
        FROM authors WHERE author_id = ?
      ", params = list(author_id))
      author_name_display <- if (nrow(author_info) > 0) author_info$name[1] else paste("Author", author_id)

      if (nrow(citations) == 0) {
        modal_content <- p(em("No citations have been added for this author yet."),
                          br(), br(),
                          "Use the Citation Annotation app to add citations.")
        modal_footer <- modalButton("Close")
      } else {
        # Store for export
        rv$modal_citations <- citations

        # JS-safe string escaper
        js_escape <- function(s) {
          s <- gsub("\\\\", "\\\\\\\\", s)
          s <- gsub("'", "\\\\'", s)
          s <- gsub("\n", "\\\\n", s)
          s <- gsub("\r", "", s)
          s
        }

        # Classify primary/secondary
        # Schema abbreviations are primary; unrecognized full titles (long names, likely editions) also primary
        all_schema_abbrevs <- if (!is.null(WORK_SCHEMAS)) WORK_SCHEMAS$abbrev else PRIMARY_ABBREVS
        is_primary <- vapply(citations$parsed_title, function(pt) {
          if (is.null(pt) || is.na(pt)) return(FALSE)
          if (pt %in% PRIMARY_ABBREVS) return(TRUE)
          # Fallback: if not in schema at all and title is long (likely a real work title), treat as primary
          if (!pt %in% all_schema_abbrevs && nchar(pt) > 20) return(TRUE)
          FALSE
        }, logical(1))

        # Group citations by parsed_title
        titles <- unique(citations$parsed_title[!is.na(citations$parsed_title)])
        primary_titles <- titles[vapply(titles, function(tt) {
          any(is_primary[!is.na(citations$parsed_title) & citations$parsed_title == tt])
        }, logical(1))]
        secondary_titles <- setdiff(titles, primary_titles)

        # Build a grouped card for one title
        build_title_group <- function(title, rows, border_color) {
          # Author name (from first row)
          author <- rows$parsed_author[1]
          author_label <- if (!is.na(author) && nchar(author) > 0) paste0(htmltools::htmlEscape(author), ", ") else ""

          # Deduplicate: merge rows with identical volume_cited + page_cited, keeping the one with more detail
          dedup_key <- paste0(
            ifelse(is.na(rows$volume_cited), "", rows$volume_cited), "|",
            ifelse(is.na(rows$page_cited), "", rows$page_cited)
          )
          keep_idx <- !duplicated(dedup_key)
          # For duplicates, prefer the row with an entry_number or section
          for (dk in unique(dedup_key[duplicated(dedup_key)])) {
            dup_rows <- which(dedup_key == dk)
            has_detail <- vapply(dup_rows, function(r) {
              (!is.na(rows$entry_number[r]) && nchar(rows$entry_number[r]) > 0) +
              (!is.na(rows$section[r]) && nchar(rows$section[r]) > 0) +
              (!is.na(rows$notes[r]) && nchar(rows$notes[r]) > 0)
            }, integer(1))
            best <- dup_rows[which.max(has_detail)]
            keep_idx[dup_rows] <- FALSE
            keep_idx[best] <- TRUE
          }
          rows <- rows[keep_idx, , drop = FALSE]

          # Build page reference lines
          page_lines <- lapply(seq_len(nrow(rows)), function(j) {
            row <- rows[j, ]
            parts <- c()
            if (!is.na(row$volume_cited) && nchar(row$volume_cited) > 0) parts <- c(parts, paste0(row$volume_cited, ":"))
            if (!is.na(row$page_cited) && nchar(row$page_cited) > 0) parts <- c(parts, htmltools::htmlEscape(row$page_cited))
            ref <- paste0(parts, collapse = "")
            if (!is.na(row$entry_number) && nchar(row$entry_number) > 0) ref <- paste0(ref, " \u2116", htmltools::htmlEscape(row$entry_number))
            if (!is.na(row$section) && nchar(row$section) > 0) ref <- paste0(ref, " (", htmltools::htmlEscape(row$section), ")")

            # GdQ / GAL edition pages: show German/English when available
            edition_note <- NULL
            if (!is.na(row$page_german) && nchar(row$page_german) > 0 &&
                !is.na(row$page_english) && nchar(row$page_english) > 0) {
              edition_note <- span(style = "color:#888;font-size:0.85em;margin-left:6px;",
                paste0("[Ger. p. ", htmltools::htmlEscape(row$page_german),
                       " / Eng. p. ", htmltools::htmlEscape(row$page_english), "]"))
            } else if (!is.na(row$page_german) && nchar(row$page_german) > 0) {
              edition_note <- span(style = "color:#888;font-size:0.85em;margin-left:6px;",
                paste0("[Ger. p. ", htmltools::htmlEscape(row$page_german), "]"))
            } else if (!is.na(row$page_english) && nchar(row$page_english) > 0) {
              edition_note <- span(style = "color:#888;font-size:0.85em;margin-left:6px;",
                paste0("[Eng. p. ", htmltools::htmlEscape(row$page_english), "]"))
            }

            # Notes (e.g., "mentioned 3 times")
            notes_span <- if (!is.na(row$notes) && nchar(row$notes) > 0) {
              span(style = "color:#888;font-size:0.85em;font-style:italic;margin-left:6px;",
                   paste0("(", htmltools::htmlEscape(row$notes), ")"))
            }

            # Digital link
            digital_url <- lookup_digital_url(con, row$parsed_title, row$page_cited, row$entry_number)
            link_span <- if (!is.null(digital_url)) {
              a(href = digital_url, target = "_blank", rel = "noopener noreferrer",
                style = "font-size:0.85em;color:#17a2b8;text-decoration:none;margin-left:4px;",
                "\u2197")
            }

            tagList(span(HTML(ref)), edition_note, notes_span, link_span)
          })

          # For cards with no page refs (e.g., edition citations), show original_text of first row
          has_any_ref <- any(vapply(seq_len(nrow(rows)), function(j) {
            !is.na(rows$page_cited[j]) && nchar(rows$page_cited[j]) > 0
          }, logical(1)))

          if (!has_any_ref && nrow(rows) > 0) {
            ref_display <- div(style = "margin-top:4px;color:#444;font-size:0.9em;",
              p(style = "margin:0;", htmltools::htmlEscape(
                substr(rows$original_text[1], 1, 200)
              ), if (nchar(rows$original_text[1]) > 200) "...")
            )
          } else if (length(page_lines) == 1) {
            ref_display <- div(style = "margin-top:4px;color:#444;", page_lines[[1]])
          } else {
            ref_display <- tags$ul(style = "margin-top:4px;margin-bottom:0;padding-left:20px;color:#444;",
              lapply(page_lines, function(pl) tags$li(style = "margin-bottom:2px;", pl))
            )
          }

          # Copyable text for this title group
          group_texts <- unique(rows$original_text[!is.na(rows$original_text)])
          group_copy <- js_escape(paste(group_texts, collapse = "\n"))

          div(style = paste0("background:#f8f9fa;padding:12px;border-radius:6px;margin-bottom:10px;border-left:4px solid ", border_color, ";"),
            div(span(style = "font-weight:600;font-size:1.05em;", HTML(paste0(author_label, htmltools::htmlEscape(title)))),
                HTML(paste0('<button class="copy-btn" onclick="copyCitationText(\'', group_copy, '\', this)" title="Copy this citation">\u2398</button>'))),
            ref_display
          )
        }

        # Build sections
        sections <- tagList()

        if (length(primary_titles) > 0) {
          primary_cards <- lapply(primary_titles, function(tt) {
            rows <- citations[!is.na(citations$parsed_title) & citations$parsed_title == tt, , drop = FALSE]
            build_title_group(tt, rows, "#0072B2")
          })
          primary_all_text <- js_escape(paste(unique(citations$original_text[is_primary & !is.na(citations$original_text)]), collapse = "\n\n"))
          sections <- tagList(sections,
            h5(style = "color:#0072B2;margin-top:8px;", "Primary Sources",
               span(style = "font-size:0.8em;font-weight:normal;color:#666;margin-left:8px;",
                    paste0("(", length(primary_titles), " works)")),
               HTML(paste0('<button class="copy-section-btn" onclick="copyCitationText(\'', primary_all_text, '\', this)" title="Copy all primary citations">Copy Section</button>'))),
            do.call(tagList, primary_cards)
          )
        }

        if (length(secondary_titles) > 0) {
          secondary_cards <- lapply(secondary_titles, function(tt) {
            rows <- citations[!is.na(citations$parsed_title) & citations$parsed_title == tt, , drop = FALSE]
            build_title_group(tt, rows, "#E69F00")
          })
          secondary_all_text <- js_escape(paste(unique(citations$original_text[!is_primary & !is.na(citations$original_text)]), collapse = "\n\n"))
          sections <- tagList(sections,
            h5(style = "color:#E69F00;margin-top:16px;", "Secondary Sources",
               span(style = "font-size:0.8em;font-weight:normal;color:#666;margin-left:8px;",
                    paste0("(", length(secondary_titles), " works)")),
               HTML(paste0('<button class="copy-section-btn" onclick="copyCitationText(\'', secondary_all_text, '\', this)" title="Copy all secondary citations">Copy Section</button>'))),
            do.call(tagList, secondary_cards)
          )
        }

        # Handle citations with no parsed_title
        no_title_idx <- which(is.na(citations$parsed_title))
        if (length(no_title_idx) > 0) {
          other_cards <- lapply(no_title_idx, function(i) {
            cit <- citations[i, ]
            div(style = "background:#f8f9fa;padding:12px;border-radius:6px;margin-bottom:10px;border-left:4px solid #6c757d;",
              p(style = "margin:0;", htmltools::htmlEscape(cit$original_text))
            )
          })
          sections <- tagList(sections,
            h5(style = "color:#6c757d;margin-top:16px;", icon("question-circle"), " Unclassified"),
            do.call(tagList, other_cards)
          )
        }

        modal_content <- tagList(
          p(strong(nrow(citations)), " citation(s) across ", strong(length(titles)), " works"),
          sections
        )

        # Export footer
        modal_footer <- tagList(
          downloadButton("modal_export_ris", "Export RIS", class = "btn-info btn-sm"),
          downloadButton("modal_export_bibtex", "Export BibTeX", class = "btn-info btn-sm"),
          actionButton("modal_copy_all", "Copy All", class = "btn-outline-secondary btn-sm",
                       onclick = paste0(
                         "var texts = ", jsonlite::toJSON(citations$original_text, auto_unbox = FALSE), ";",
                         "navigator.clipboard.writeText(texts.join('\\n\\n')).then(function(){",
                         "Shiny.setInputValue('copy_notify', Math.random());",
                         "});"
                       )),
          modalButton("Close")
        )
      }

      showModal(modalDialog(
        title = htmltools::htmlEscape(paste0("Citations: ", format_camel_case(author_name_display, "author"))),
        modal_content,
        size = "l",
        easyClose = TRUE,
        footer = modal_footer
      ))
    }, error = function(e) {
      showNotification(paste("Error loading citations:", e$message), type = "error")
    })
  })

  # ========== RIS Download ==========

  output$download_ris <- downloadHandler(
    filename = function() {
      "mashriq-maghrib_bibliography.ris"
    },
    content = function(file) {
      ris_path <- file.path("data", "mashriq-maghrib_bibliography.ris")
      if (file.exists(ris_path)) {
        file.copy(ris_path, file)
      } else {
        writeLines("TY  - GEN\nTI  - No citations exported yet. Use the Citation Annotation app to add citations first.\nER  - \n", file)
      }
    }
  )
}
