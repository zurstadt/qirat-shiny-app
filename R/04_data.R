# Data loading and processing functions

# Helper: Load data from database
load_from_database <- function(db_path = DB_PATH) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    query <- "
      SELECT
        w.work_id,
        w.title,
        w.title_arabic,
        w.type,
        w.[set],
        w.extant,
        w.author_id,
        COALESCE(a.author_name_canonical, a.author_name) as author_name,
        a.author_name_arabic,
        a.regionality,
        a.death_century,
        w.text_reuse,
        w.text_reuse_source,
        rw.title as reused_title,
        COALESCE(ra.author_name_canonical, ra.author_name) as reused_author
      FROM works_in_range w
      LEFT JOIN authors a ON w.author_id = a.author_id
      LEFT JOIN works rw ON w.text_reuse_source = rw.work_id
      LEFT JOIN authors ra ON rw.author_id = ra.author_id
      WHERE a.death_century IS NOT NULL
        AND w.[set] IS NOT NULL
        AND w.[set] != ''
        AND w.[set] != 'NA'
    "

    df <- dbGetQuery(con, query)

    # Also get works that are subjects of commentaries
    commentaries_query <- "
      SELECT
        text_reuse_source as work_id,
        GROUP_CONCAT(w.work_id, '|') as commentary_ids,
        GROUP_CONCAT(w.title, '|') as commentary_titles,
        GROUP_CONCAT(w.type, '|') as commentary_types,
        GROUP_CONCAT(COALESCE(a.author_name_canonical, a.author_name), '|') as commentary_authors
      FROM works w
      LEFT JOIN authors a ON w.author_id = a.author_id
      WHERE w.text_reuse_source IS NOT NULL AND w.text_reuse_source <> ''
      GROUP BY text_reuse_source
    "
    commentaries_df <- dbGetQuery(con, commentaries_query)

    # Merge commentary info
    if (nrow(commentaries_df) > 0) {
      df <- merge(df, commentaries_df, by = "work_id", all.x = TRUE)
    } else {
      df$commentary_ids <- NA
      df$commentary_titles <- NA
      df$commentary_types <- NA
      df$commentary_authors <- NA
    }

    # Load citation counts per work and per author
    citation_counts_work <- tryCatch(
      dbGetQuery(con, "
        SELECT work_id, COUNT(*) as citation_count
        FROM bibliographic_citations
        WHERE work_id IS NOT NULL
        GROUP BY work_id
      "),
      error = function(e) data.frame(work_id = character(0), citation_count = integer(0))
    )
    citation_counts_author <- tryCatch(
      dbGetQuery(con, "
        SELECT author_id, COUNT(*) as citation_count
        FROM bibliographic_citations
        WHERE author_id IS NOT NULL
        GROUP BY author_id
      "),
      error = function(e) data.frame(author_id = integer(0), citation_count = integer(0))
    )

    # Merge citation counts
    if (nrow(citation_counts_work) > 0) {
      df <- merge(df, citation_counts_work, by = "work_id", all.x = TRUE)
    } else {
      df$citation_count <- NA
    }
    if (nrow(citation_counts_author) > 0) {
      author_cit <- citation_counts_author
      names(author_cit) <- c("author_id", "author_citation_count")
      df <- merge(df, author_cit, by = "author_id", all.x = TRUE)
    } else {
      df$author_citation_count <- NA
    }

    # Clean up set values (e.g., "7.0" -> "7", keep "7+1" and "10+" as-is)
    df$set <- sapply(df$set, function(x) {
      if (x == "7.0") "7" else x
    })
    # Parse semicolon-delimited titles to show canonical (first) element only
    df$title <- sapply(df$title, parse_first_title)
    df$title_display <- paste0(df$title, " / ", df$title_arabic)
    df$author_display <- paste0(df$author_name, " / ", df$author_name_arabic)
    df
  }, error = function(e) {
    message("Database error: ", e$message)
    NULL
  })
}

# Helper: process bibliography data
process_bib_data <- function(df, outcome_col, geo_col, century_col = NULL) {
  outcome_chr <- as.character(x = df[[outcome_col]])
  geo_chr <- as.character(x = df[[geo_col]])

  if (geo_col == "regionality" || any(grepl(pattern = "ma\u0161riq|ma\u0121rib", x = geo_chr, ignore.case = TRUE))) {
    geo_chr <- ifelse(test = grepl(pattern = "^ma\u0121rib", x = geo_chr, ignore.case = TRUE),
                      yes = "ma\u0121rib",
                      no = ifelse(test = grepl(pattern = "^ma\u0161riq", x = geo_chr, ignore.case = TRUE),
                                  yes = "ma\u0161riq",
                                  no = geo_chr))
    geo_fac <- factor(x = geo_chr, levels = c("ma\u0121rib", "ma\u0161riq"))
  } else {
    geo_fac <- factor(x = geo_chr)
  }

  # Filter valid indices
  valid_idx <- !is.na(outcome_chr) & !is.na(geo_chr) &
               outcome_chr != "" & geo_chr != "" &
               outcome_chr != "NA" & geo_chr != "NA"

  if (!is.null(century_col) && century_col %in% names(df)) {
    century_raw <- df[[century_col]][valid_idx]
    century_numeric <- as.numeric(century_raw)
    century_valid <- !is.na(century_numeric) & century_numeric >= 4 & century_numeric <= 9

    outcome_filtered <- outcome_chr[valid_idx][century_valid]
    geo_filtered <- geo_fac[valid_idx][century_valid]

    result <- data.frame(
      outcome = factor(outcome_filtered, levels = c("7", "7+1", "10+")),
      geo = droplevels(geo_filtered),
      century = century_numeric[century_valid],
      stringsAsFactors = FALSE
    )
    return(result)
  }

  data.frame(
    outcome = factor(outcome_chr[valid_idx], levels = c("7", "7+1", "10+")),
    geo = droplevels(geo_fac[valid_idx]),
    stringsAsFactors = FALSE
  )
}

# Auto-initialization function
initialize_app <- function() {
  df <- load_from_database()

  if (!is.null(df) && nrow(df) > 0) {
    clean <- process_bib_data(
      df,
      outcome_col = "set",
      geo_col = "regionality",
      century_col = "death_century"
    )

    list(
      raw = df,
      clean = clean,
      initialized = TRUE,
      n_works = nrow(clean)
    )
  } else {
    list(
      raw = NULL,
      clean = NULL,
      initialized = FALSE,
      n_works = 0
    )
  }
}
