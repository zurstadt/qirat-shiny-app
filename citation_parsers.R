# Citation Parsing Functions (extracted for testing)
# Updated based on adjudication feedback 2026-01-21
# Version 4: Added GAL Supplement handling, schema-driven CSL export
# Schema source: deploy/data/parser-work-schemas.json

# =============================================================================
# WORK SCHEMA LOADING
# =============================================================================
# Load approved work schemas for type resolution and field configuration.
# These schemas define Zotero/CSL types, fields, and properties for each work.

# Global cache for loaded schemas
.work_schemas_cache <- new.env(parent = emptyenv())

#' Load work schemas from JSON file
#' @param schema_path Path to parser-work-schemas.json (defaults to relative path)
#' @return List of schema objects keyed by abbreviation
load_work_schemas <- function(schema_path = NULL) {
  # Return cached if available

if (!is.null(.work_schemas_cache$schemas)) {
    return(.work_schemas_cache$schemas)
  }

  # Try multiple paths to find the schema file
  if (is.null(schema_path)) {
    possible_paths <- c(
      "data/parser-work-schemas.json",
      "../data/parser-work-schemas.json",
      "deploy/data/parser-work-schemas.json",
      "../deploy/data/parser-work-schemas.json",
      "../../deploy/data/parser-work-schemas.json"
    )
    for (p in possible_paths) {
      if (file.exists(p)) {
        schema_path <- p
        break
      }
    }
  }

  if (is.null(schema_path) || !file.exists(schema_path)) {
    warning("Work schemas file not found. Using default type mappings.")
    return(list())
  }

  tryCatch({
    data <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
    schemas <- list()
    for (s in data$schemas) {
      schemas[[s$abbrev]] <- s
    }
    .work_schemas_cache$schemas <- schemas
    schemas
  }, error = function(e) {
    warning(paste("Error loading work schemas:", e$message))
    list()
  })
}

#' Get schema for a specific work abbreviation
#' @param abbrev Work abbreviation (e.g., "GAL", "EI2", "PUA")
#' @return Schema list or NULL if not found
get_work_schema <- function(abbrev) {
  schemas <- load_work_schemas()
  schemas[[abbrev]]
}

#' Get Zotero type for a work abbreviation
#' @param abbrev Work abbreviation
#' @return Zotero type string (e.g., "book", "encyclopediaArticle", "webpage")
get_zotero_type <- function(abbrev) {
  schema <- get_work_schema(abbrev)
  if (!is.null(schema)) {
    return(schema$zotero_type)
  }
  "book"  # Default
}

#' Get CSL type for a work abbreviation
#' @param abbrev Work abbreviation
#' @return CSL type string (e.g., "book", "entry-encyclopedia", "webpage")
get_csl_type <- function(abbrev) {
  schema <- get_work_schema(abbrev)
  if (!is.null(schema)) {
    return(schema$csl_type)
  }
  "book"  # Default
}

#' Check if a work has multi-edition pagination
#' @param abbrev Work abbreviation
#' @return TRUE if work has edition_pagination configured
has_multi_edition_pagination <- function(abbrev) {
  schema <- get_work_schema(abbrev)
  if (!is.null(schema) && !is.null(schema$properties)) {
    return(isTRUE(schema$properties$multi_edition_pagination))
  }
  FALSE
}

#' Get edition pagination configuration for a work
#' @param abbrev Work abbreviation
#' @return edition_pagination list or NULL
get_edition_pagination <- function(abbrev) {
  schema <- get_work_schema(abbrev)
  if (!is.null(schema) && !is.null(schema$edition_pagination)) {
    ep <- schema$edition_pagination
    # Check if it's an empty object
    if (length(ep) > 0 && !is.null(ep$editions)) {
      return(ep)
    }
  }
  NULL
}

#' Clear the schema cache (useful for testing)
clear_schema_cache <- function() {
  rm(list = ls(.work_schemas_cache), envir = .work_schemas_cache)
}

#' Get abbreviations of all secondary (non-primary) works from schema
#' @return Character vector of abbreviations where is_primary is FALSE
get_secondary_abbreviations <- function() {
  schemas <- load_work_schemas()
  secondary <- c()
  for (abbrev in names(schemas)) {
    if (isTRUE(schemas[[abbrev]]$is_primary == FALSE)) {
      secondary <- c(secondary, abbrev)
    }
  }
  secondary
}

# =============================================================================
# ARTICLE URL LOOKUP
# =============================================================================
# Look up URLs/DOIs for encyclopedia articles from the article_urls database table.
# This enables linking parsed citations to their online sources.

# Global cache for database connection
.url_db_cache <- new.env(parent = emptyenv())

#' Normalize text for URL matching (remove diacritics, lowercase)
#' @param text Text to normalize
#' @return Normalized text string
normalize_for_url_match <- function(text) {
  if (is.null(text) || is.na(text) || text == "") {
    return("")
  }
  if (requireNamespace("stringi", quietly = TRUE)) {
    # Remove diacritics and convert to lowercase
    norm <- stringi::stri_trans_general(text, "Latin-ASCII")
    norm <- tolower(norm)
    return(trimws(norm))
  }
  tolower(trimws(text))
}

#' Look up article URL/DOI from the database
#' @param source_abbrev Encyclopedia abbreviation (e.g., "EI2", "DMBI", "TDVİA")
#' @param article_title Article/entry title
#' @param db_path Path to the SQLite database (defaults to relative paths)
#' @return List with url and url_type, or NULL if not found
lookup_article_url <- function(source_abbrev, article_title, db_path = NULL) {
  if (is.null(source_abbrev) || is.na(source_abbrev) || source_abbrev == "") {
    return(NULL)
  }
  if (is.null(article_title) || is.na(article_title) || article_title == "") {
    return(NULL)
  }

  # Normalize source abbreviation (handle TDVİA vs TDVIA, ĠN vs GN, etc.)
  # The database uses ASCII versions, so normalize special characters
  source_abbrev_norm <- source_abbrev
  if (requireNamespace("stringi", quietly = TRUE)) {
    source_abbrev_norm <- stringi::stri_trans_general(source_abbrev, "Latin-ASCII")
  }

  # Try multiple paths to find the database
  if (is.null(db_path)) {
    possible_paths <- c(
      "data/iqsa_deploy.db",
      "../data/iqsa_deploy.db",
      "deploy/data/iqsa_deploy.db",
      "../deploy/data/iqsa_deploy.db",
      "../../deploy/data/iqsa_deploy.db"
    )
    for (p in possible_paths) {
      if (file.exists(p)) {
        db_path <- p
        break
      }
    }
  }

  if (is.null(db_path) || !file.exists(db_path)) {
    return(NULL)
  }

  # Normalize the article title for matching
  title_norm <- normalize_for_url_match(article_title)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Check if table exists
    tables <- DBI::dbListTables(con)
    if (!"article_urls" %in% tables) {
      return(NULL)
    }

    # Query for matching URL (using normalized source abbreviation)
    result <- DBI::dbGetQuery(con,
      "SELECT url, url_type FROM article_urls
       WHERE source_abbrev = ? AND entry_title_normalized = ?
       LIMIT 1",
      params = list(source_abbrev_norm, title_norm))

    if (nrow(result) > 0) {
      return(list(
        url = result$url[1],
        url_type = result$url_type[1]
      ))
    }

    # Try fuzzy match if exact match fails (prefix match)
    result <- DBI::dbGetQuery(con,
      "SELECT url, url_type FROM article_urls
       WHERE source_abbrev = ? AND entry_title_normalized LIKE ?
       LIMIT 1",
      params = list(source_abbrev_norm, paste0(title_norm, "%")))

    if (nrow(result) > 0) {
      return(list(
        url = result$url[1],
        url_type = result$url_type[1]
      ))
    }

    # Try Levenshtein distance matching (edit distance <= 2)
    # This handles transliteration variants like "al-Sijistani" vs "al-Sidjistānī"
    all_entries <- DBI::dbGetQuery(con,
      "SELECT entry_title_normalized, url, url_type FROM article_urls
       WHERE source_abbrev = ?",
      params = list(source_abbrev_norm))

    if (nrow(all_entries) > 0) {
      # Calculate Levenshtein distances
      distances <- sapply(all_entries$entry_title_normalized, function(db_title) {
        utils::adist(title_norm, db_title)[1,1]
      })

      min_dist <- min(distances)
      # Accept matches with edit distance <= 2 (allows for diacritics and minor spelling variants)
      if (min_dist <= 2) {
        best_idx <- which.min(distances)
        return(list(
          url = all_entries$url[best_idx],
          url_type = all_entries$url_type[best_idx]
        ))
      }
    }

    NULL
  }, error = function(e) {
    NULL
  })
}

#' Extract DOI from full URL
#' @param url Full DOI URL (e.g., "https://doi.org/10.1163/...")
#' @return DOI string without URL prefix
extract_doi_from_url <- function(url) {
  if (is.null(url) || is.na(url)) {
    return(NULL)
  }
  gsub("^https?://doi\\.org/", "", url)
}

# =============================================================================
# INPUT NORMALIZATION
# =============================================================================
# Normalize pasted text to ensure consistent basis for parsing.
# This handles common copy/paste issues from PDFs, word processors, and web sources.
# Rules are modular and can be adjusted as needed.

normalize_input <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return(text)
  }

  # ---------------------------------------------
  # 1. UNICODE NORMALIZATION (NFC - precomposed)
  # ---------------------------------------------
  # Ensures characters like ā are single codepoints, not base + combining mark

  # This is critical for consistent pattern matching
  if (requireNamespace("stringi", quietly = TRUE)) {
    text <- stringi::stri_trans_nfc(text)
  }

  # ---------------------------------------------
  # 2. WHITESPACE NORMALIZATION
  # ---------------------------------------------
  # Replace non-breaking spaces (U+00A0) with regular spaces
  text <- gsub("\u00A0", " ", text, perl = TRUE)
  # Replace other Unicode whitespace variants
  text <- gsub("[\u2000-\u200B\u202F\u205F\u3000]", " ", text, perl = TRUE)
  # Collapse multiple spaces to single space
  text <- gsub("  +", " ", text, perl = TRUE)
  # Remove zero-width characters that can hide in pasted text
  text <- gsub("[\u200B-\u200D\uFEFF]", "", text, perl = TRUE)

  # ---------------------------------------------
  # 3. QUOTE NORMALIZATION
  # ---------------------------------------------
  # Normalize curly/smart quotes to straight quotes
  # Double quotes: " " „ « » → "
  text <- gsub("[\u201C\u201D\u201E\u00AB\u00BB]", '"', text, perl = TRUE)
  # Single quotes: ' ' ‚ › ‹ → ' (but preserve ʿ ayn and ʾ hamza)
  text <- gsub("[\u2018\u2019\u201A\u203A\u2039]", "'", text, perl = TRUE)
  # Backtick/grave accent used as quote → straight quote
  text <- gsub("`", "'", text, perl = TRUE)

  # ---------------------------------------------
  # 4. DASH/HYPHEN NORMALIZATION
  # ---------------------------------------------
  # Normalize various dashes for year ranges and page ranges
  # Em-dash (—), en-dash (–), minus (−), figure dash (‒) → hyphen for ranges
  # But preserve en-dash in formatted year ranges like "1990–1995"
  # Strategy: normalize to en-dash first, then pattern-specific handling

  # First, normalize all dash-like characters to en-dash
  text <- gsub("[\u2014\u2212\u2012]", "\u2013", text, perl = TRUE)  # em-dash, minus, figure-dash → en-dash

  # Hyphen between numbers should become en-dash (proper typography)
  text <- gsub("(\\d)-(\\d)", "\\1\u2013\\2", text, perl = TRUE)

  # ---------------------------------------------
  # 5. ELLIPSIS NORMALIZATION
  # ---------------------------------------------
  # Normalize ellipsis character to three periods
  text <- gsub("\u2026", "...", text, perl = TRUE)

  # ---------------------------------------------
  # 6. TRANSLITERATION CHARACTER PRESERVATION
  # ---------------------------------------------
  # These are INTENTIONALLY preserved - do not normalize:
  # - ʿ (U+02BF) - Arabic ayn
  # - ʾ (U+02BE) - Arabic hamza
  # - Macron vowels: ā ī ū Ā Ī Ū
  # - Dot-below: ḍ ḥ ṣ ṭ ẓ Ḍ Ḥ Ṣ Ṭ Ẓ
  # - Caron/háček: č š ž ǧ Č Š Ž Ǧ
  # - Other: ġ ḫ ṯ ḏ Ġ Ḫ Ṯ Ḏ ẖ

  # However, normalize COMMON SUBSTITUTES that users might type:
  # Straight apostrophe after specific patterns → ayn/hamza
  # (This is heuristic and may need refinement)

  # ---------------------------------------------
  # 7. PUNCTUATION SPACING
  # ---------------------------------------------
  # Ensure space after comma (but not before)
  text <- gsub("\\s+,", ",", text, perl = TRUE)
  # Ensure space after colon in place:publisher patterns (but not in vol:page)
  # This is tricky - only fix obvious cases like "Place:Publisher"
  text <- gsub("([A-Za-z]):([A-Za-z])", "\\1: \\2", text, perl = TRUE)

  # ---------------------------------------------
  # 8. TRIM
  # ---------------------------------------------
  text <- trimws(text)

  text
}

# Get normalization metadata for transparency/debugging
# Returns list of what normalizations were applied
get_normalization_report <- function(original, normalized) {
  report <- list(
    original_length = nchar(original),
    normalized_length = nchar(normalized),
    changes_detected = original != normalized,
    normalizations_applied = c()
  )

  if (original != normalized) {
    # Detect what changed
    if (grepl("[\u00A0\u2000-\u200B\u202F\u205F\u3000]", original, perl = TRUE)) {
      report$normalizations_applied <- c(report$normalizations_applied, "whitespace_variants")
    }
    if (grepl("[\u201C\u201D\u201E\u00AB\u00BB\u2018\u2019\u201A]", original, perl = TRUE)) {
      report$normalizations_applied <- c(report$normalizations_applied, "smart_quotes")
    }
    if (grepl("[\u2014\u2212\u2012]", original, perl = TRUE)) {
      report$normalizations_applied <- c(report$normalizations_applied, "dash_variants")
    }
    if (grepl("[\u200B-\u200D\uFEFF]", original, perl = TRUE)) {
      report$normalizations_applied <- c(report$normalizations_applied, "zero_width_chars")
    }
    if (grepl("  +", original, perl = TRUE)) {
      report$normalizations_applied <- c(report$normalizations_applied, "multiple_spaces")
    }
  }

  report
}

# =============================================================================
# CITATION TYPE DETECTION
# =============================================================================

# Determine citation form (long vs short) and type
detect_citation_type <- function(text) {
  # Guard against NA/NULL input - return unknown type instead of crashing
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return(list(form = "unknown", type = "unknown"))
  }

  # Apply input normalization for consistent analysis
  text <- normalize_input(text)

  if (text == "") {
    return(list(form = "unknown", type = "unknown"))
  }

  # Pre-process to analyze only main citation (before "Consult") for type detection
  # This prevents "Consult the references in..." sections from confusing type detection
  # Enhanced pattern to catch: "Consult...", "See also", "Cf.", "For X, see", "See further"
  consult_preprocess_pattern <- "(?:\\.\\s*|^)(Consult(?:\\s+further)?\\s+(?:the\\s+)?(?:references\\s+in)?|See\\s+(?:also|further)|Cf\\.|For\\s+[^,]+,\\s*see)\\s*"
  has_consult_section <- isTRUE(grepl(consult_preprocess_pattern, text, ignore.case = TRUE, perl = TRUE))

  if (has_consult_section) {
    split_pos <- regexpr(consult_preprocess_pattern, text, ignore.case = TRUE, perl = TRUE)
    if (split_pos[1] != -1) {
      main_text <- trimws(substr(text, 1, split_pos - 1))
      if (nchar(main_text) > 0) {
        text <- main_text  # Analyze main part only for type detection
      }
    }
  }

  # Use isTRUE() to handle potential NA from grepl on malformed input
  has_long_pub_info <- isTRUE(grepl("\\([^)]+:\\s*[^,]+,\\s*(\\d{4}|N\\.D\\.)", text, perl = TRUE))

  # FIRST: Check for equality (multiple editions) - this is a strong structural indicator
  # Must have = signs AND publication info (Place: Publisher, Year)
  # This takes precedence because equality citations may contain GdQ or other short refs
  if (safe_grepl("\\s+=\\s+", text) && has_long_pub_info) {
    return(list(form = "long", type = "monograph_equality"))
  }

  # Check for short equality (same author cross-reference)
  # Pattern: Author, Work1, pages = Work2, pages (NO long-form publication info)
  # Example: Ḥamdān, Adab, 339–50 №220 = MM 100n1
  if (safe_grepl("\\s+=\\s+", text) && !has_long_pub_info) {
    return(list(form = "short", type = "short_equality"))
  }

  # Check for GdQ with dual-page format
  # Pattern: GdQ followed by volume:page_german/page_english with optional footnote
  # Only match if there's no long-form publication info (otherwise it's part of a larger citation)
  if (safe_grepl("GdQ.*\\d+:\\d+/\\d+", text) && !has_long_pub_info) {
    return(list(form = "short", type = "secondary_gdq"))
  }

  # Short form indicators: no publisher info, abbreviated titles
  # Only match if no long-form publication info
  short_form_patterns <- c(
    "^(Ibn al-Ǧazarī|al-Ḏahabī|Ibn Baškuwāl|Ibn Ḫayr|Kâtip Çelebî|Ibn al-Nadīm|Ibn ʿAsākir),\\s+\\w+,\\s+\\d+",
    "^(Ḥamdān|Bergsträßer|Pretzl|Brockelmann|Sezgin|Sālim|Walad Ubbāh),",
    ",\\s+(EI2|EI3|EIr|DMBI|TDVİA|GAL|GdQ|MM|MQK|ĠN|Našr|Adab)\\b"
  )

  for (pattern in short_form_patterns) {
    if (safe_grepl(pattern, text, perl = TRUE) && !has_long_pub_info) {
      if (safe_grepl("(EI2|EI3|EIr|DMBI|TDVİA|GAL|GdQ|MM|Adab)", text)) {
        return(list(form = "short", type = "secondary"))
      } else {
        return(list(form = "short", type = "primary"))
      }
    }
  }

  # Check for multiple articles with idem/eadem
  if (safe_grepl(";\\s*and\\s+(idem|eadem),", text, ignore.case = TRUE)) {
    return(list(form = "long", type = "multiple_articles"))
  }

  # Check for serial short references (multiple refs separated by semicolon)
  # Has semicolon followed by author name pattern, but no long-form publication info (Place: Publisher, Year)
  if (safe_grepl(";\\s*(and\\s+)?[A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū]", text) && !has_long_pub_info) {
    return(list(form = "short", type = "serial_short"))
  }

  # Check for dissertation/thesis (only if NOT an equality citation)
  if (safe_grepl("Ph\\.D\\.\\s*(diss|Dissertation)", text, ignore.case = TRUE)) {
    return(list(form = "long", type = "dissertation"))
  }
  if (safe_grepl("M\\.A\\.\\s*Thesis", text, ignore.case = TRUE)) {
    return(list(form = "long", type = "thesis"))
  }

  # Check for journal article FIRST (before book_section)
  # Pattern: Author, "Title," Journal Volume.Issue (Year):pages
  # or: Author, "Title," Journal Volume (Year):pages
  # More robust detection: extract quoted title first, then check for volume(year):pages pattern
  # This handles complex Arabic titles with nested parentheses
  # Also handles complex journal names with colons, commas, and institution info
  # Example: "ARC: The Journal of the Faculty of Religious Study, McGill University 38 (2011):15–36"
  has_quoted_title <- safe_grepl('"[^"]+"', text)
  if (has_quoted_title) {
    # Extract everything after the quoted title
    after_title <- sub('^.*"[^"]*",?\\s*', "", text, perl = TRUE)
    # Flexible pattern: find digits followed by (year):pages anywhere
    # Handles: "Journal Name 38 (2011):15" or "Journal 38.1 (2011):15"
    # Also handles complex journal names with colons and institution info
    journal_pattern <- '\\d+(?:\\.\\d+)?\\s*\\(\\d{4}(?:/\\d{4})?\\):\\d+'
    journal_match_pos <- regexpr(journal_pattern, after_title, perl = TRUE)

    if (journal_match_pos[1] != -1) {
      # Found journal pattern - check if "in ... ed." appears BEFORE it (book_section)
      # or AFTER it (narrative text with embedded monograph citation)
      text_before_journal <- substr(after_title, 1, journal_match_pos[1] - 1)
      # Use non-greedy match for book title (may contain commas)
      is_book_section <- safe_grepl(',?\\s*in\\s+.+?,\\s*eds?\\.', text_before_journal, ignore.case = TRUE, perl = TRUE)

      if (!is_book_section) {
        return(list(form = "long", type = "journal_article"))
      }
      # If is_book_section is TRUE, fall through to book_section check below
    }
    # If no journal pattern found, fall through to book_section check
  }

  # Check for book section (article in edited volume)
  # Pattern 1: Author, "Title," in Book Title, ed. Editor (Place: Publisher, Year)
  # Pattern 2: Author, "Title," in StudyAuthor, Book Title (Place: Publisher, Year) — no editor
  # Use non-greedy match for book title since it may contain commas
  if (safe_grepl('"[^"]+",?\\s*in\\s+.+?,\\s*eds?\\.', text, ignore.case = TRUE, perl = TRUE)) {
    return(list(form = "long", type = "book_section"))
  }
  # Editor-less book section: "Title," in ... (Place: Publisher, Year)
  if (safe_grepl('"[^"]+",?\\s+in\\s+.+\\([^)]+:\\s*[^,]+,\\s*\\d{4}', text, ignore.case = TRUE, perl = TRUE)) {
    return(list(form = "long", type = "book_section"))
  }

  # Long form with publication info
  if (safe_grepl("\\([^)]+:\\s*[^,]+,\\s*(\\d{4}|N\\.D\\.)", text)) {
    # Check for article (quoted title) - fallback if not matched above
    if (safe_grepl('^[^"]+,\\s*"[^"]+"', text)) {
      return(list(form = "long", type = "article"))
    }
    # (equality check moved earlier in function)
    return(list(form = "long", type = "monograph"))
  }

  # Before returning unknown, use pattern density heuristics
  density <- calculate_pattern_density(text)
  char_count <- nchar(text)

  if (density$vol_page_count > 0 || density$abbrev_count > 0) {
    if (density$equality_count > 0) {
      return(list(form = "short", type = "short_equality",
                  confidence = "heuristic", density = density))
    }
    # Secondary abbreviations indicate secondary source
    secondary_abbrevs <- c("MM", "Adab", "GdQ", "GAL", "EI2", "EI3", "TDVİA", "GAS", "EIr", "DMBI")
    is_secondary <- any(sapply(secondary_abbrevs, function(a) safe_grepl(a, text, fixed = TRUE)))
    return(list(form = "short", type = if (is_secondary) "secondary" else "primary",
                confidence = "heuristic", density = density))
  }

  # Length-based last resort for short citations
  if (char_count < 100 && (density$abbrev_count > 0 || safe_grepl(",\\s*\\d+", text))) {
    return(list(form = "short", type = "primary", confidence = "length_heuristic"))
  }

  return(list(form = "unknown", type = "unknown"))
}

# Normalize common ASCII approximations and typos in abbreviations
# Handles cases where users type ASCII equivalents of transliterated characters
normalize_abbreviations <- function(text) {
  # GN → ĠN (common ASCII approximation for Ġāyat al-nihāyah)
  text <- gsub("\\bGN\\b", "ĠN", text, perl = TRUE)
  text
}

# Calculate pattern density metrics for fallback classification
# Returns counts of various citation-indicative patterns
calculate_pattern_density <- function(text) {
  # Count vol:page patterns (e.g., "1:49", "2:805")
  vol_page_matches <- gregexpr("\\d+:\\d+", text, perl = TRUE)[[1]]
  vol_page_count <- if (vol_page_matches[1] == -1) 0 else length(vol_page_matches)

  # Count equality signs with surrounding spaces
  equality_matches <- gregexpr("\\s+=\\s+", text, perl = TRUE)[[1]]
  equality_count <- if (equality_matches[1] == -1) 0 else length(equality_matches)

  # Count entry numbers (№ followed by alphanumeric)
  entry_matches <- gregexpr("№[A-Za-z0-9.]+", text, perl = TRUE)[[1]]
  entry_count <- if (entry_matches[1] == -1) 0 else length(entry_matches)

  # Count section markers (§)
  section_matches <- gregexpr("§\\s*[^)]+", text, perl = TRUE)[[1]]
  section_count <- if (section_matches[1] == -1) 0 else length(section_matches)

  # Count known abbreviations
  abbrevs <- c("ĠN", "MM", "Našr", "Adab", "MQK", "GdQ", "GAL", "EI2", "EI3",
               "TDVİA", "Fahrasah", "Fihrist", "GAS", "EIr", "DMBI")
  abbrev_count <- sum(sapply(abbrevs, function(a) {
    m <- gregexpr(paste0("\\b", a, "\\b"), text, perl = TRUE)[[1]]
    if (m[1] == -1) 0 else length(m)
  }))

  # Count footnote references (n1, nn2-3, etc.)
  footnote_matches <- gregexpr("nn?\\d+", text, perl = TRUE)[[1]]
  footnote_count <- if (footnote_matches[1] == -1) 0 else length(footnote_matches)

  list(
    vol_page_count = vol_page_count,
    equality_count = equality_count,
    entry_count = entry_count,
    section_count = section_count,
    abbrev_count = abbrev_count,
    footnote_count = footnote_count
  )
}

# Safe wrapper for grepl that returns FALSE instead of NA
# Use this in any conditional statement where text input could be NA
safe_grepl <- function(pattern, x, perl = TRUE, ...) {
  # Default to perl=TRUE for Unicode safety
  if (is.null(x) || length(x) == 0) return(FALSE)
  if (length(x) == 1 && is.na(x)) return(FALSE)
  result <- grepl(pattern, x, perl = perl, ...)
  isTRUE(result)
}

# Detect what field a segment starts with for sequential inheritance
# Returns: "editor_only", "title_and_editor", or "full" (author+title+editor)
detect_leading_field <- function(segment) {
  segment <- trimws(segment)

  # Case 1: Starts with editor marker - only provides editor info
  if (safe_grepl("^eds?\\.\\s+", segment)) {
    return("editor_only")
  }

  # Case 2: Starts with title prefix patterns (Arabic book titles)
  title_prefixes <- c(
    "^K\\.\\s+",                    # K. al-Ġāyah, K. al-Sabʿah
    "^Kitāb\\s+",                   # Kitāb al-...
    "^al-[A-ZĀĪŪĠǦḤḪṢṬẒ]",         # al-Ġāyah (article + capital)
    "^Risālat?\\s+",               # Risālah/Risālat
    "^Šarḥ\\s+",                   # Šarḥ (commentary)
    "^Muḫtaṣar\\s+",               # Muḫtaṣar (abridgment)
    "^Ǧāmiʿ\\s+",                  # Ǧāmiʿ (collection)
    "^Tafsīr\\s+"                  # Tafsīr (exegesis)
  )

  for (prefix in title_prefixes) {
    if (safe_grepl(prefix, segment, ignore.case = FALSE)) {
      return("title_and_editor")
    }
  }

  # Case 3: Has publication info but no leading author pattern
  has_pub_info <- safe_grepl("\\([^)]+:\\s*[^,]+,\\s*(\\d{4}|N\\.D\\.)", segment)
  has_leading_author <- safe_grepl("^\\p{Lu}[\\p{Ll}ʿʾ]+(?:\\s+[\\p{L}ʿʾ-]+)*,\\s+[^e]", segment, perl = TRUE)

  if (has_pub_info && !has_leading_author) {
    return("title_and_editor")
  }

  return("full")
}

# Extract title from a segment that starts with title (no author prefix)
extract_segment_title <- function(segment) {
  segment <- trimws(segment)

  # Pattern: Title, ed. Editor (Place: Publisher, Year)
  title_match <- regmatches(segment, regexpr("^(.+?)\\s*,\\s*eds?\\.", segment, perl = TRUE))
  if (length(title_match) > 0) {
    return(trimws(gsub("\\s*,\\s*eds?\\.$", "", title_match, perl = TRUE)))
  }

  # Fallback: extract up to first opening paren
  title_match2 <- regmatches(segment, regexpr("^([^(]+?)\\s*\\(", segment, perl = TRUE))
  if (length(title_match2) > 0) {
    return(trimws(gsub("\\s*\\($", "", title_match2, perl = TRUE)))
  }

  return(NA)
}

# Smart split on semicolons, respecting parentheses
# Only splits when semicolon is NOT inside parentheses
# Example: "(Beirut; Cairo: Publisher, 2020)" - semicolon inside parens, don't split
# Example: "..., 2020); al-Ḏahabī, MQK..." - semicolon outside parens, DO split
smart_split_semicolon <- function(text) {
  chars <- strsplit(text, "")[[1]]
  depth <- 0
  segments <- c()
  current <- ""

  for (char in chars) {
    if (char == "(") depth <- depth + 1
    if (char == ")") depth <- depth - 1

    if (char == ";" && depth == 0) {
      segments <- c(segments, trimws(current))
      current <- ""
    } else {
      current <- paste0(current, char)
    }
  }

  # Don't forget last segment
  if (nchar(trimws(current)) > 0) {
    segments <- c(segments, trimws(current))
  }

  # Clean up "and " prefix from segments
  segments <- gsub("^and\\s+", "", segments, perl = TRUE)
  segments <- trimws(segments)
  segments[segments != ""]
}

# Split segments on ", citing " (and variants) into separate references
# e.g. "Ḥamdān, Adab, 414 №300n4, citing Abū ʿAmr al-Dānī, Mufradat Yaʿqūb, ..."
# becomes two segments: ["Ḥamdān, Adab, 414 №300n4", "Abū ʿAmr al-Dānī, Mufradat Yaʿqūb, ..."]
split_citing <- function(segments) {
  citing_pattern <- ",\\s+citing\\s+"
  result <- c()
  for (seg in segments) {
    if (grepl(citing_pattern, seg, perl = TRUE)) {
      parts <- strsplit(seg, citing_pattern, perl = TRUE)[[1]]
      result <- c(result, trimws(parts))
    } else {
      result <- c(result, seg)
    }
  }
  result[result != ""]
}

# Split segments on ", and " when it separates two independent citations
# Safe pattern: split after an entry number (№...) or page range followed by ", and "
# e.g. "Pretzl, ..., 29–30 №A.1.14, and al-Asad, al-Fihrist al-šāmil, 1:127 №37"
# becomes: ["Pretzl, ..., 29–30 №A.1.14", "al-Asad, al-Fihrist al-šāmil, 1:127 №37"]
split_comma_and <- function(segments) {
  # Match ", and " after entry number (№...) or after page/range at depth 0
  and_pattern <- "(№[A-Za-z0-9.]+|\\d+(?:[–-]\\d+)?),\\s+and\\s+"
  result <- c()
  for (seg in segments) {
    if (grepl(and_pattern, seg, perl = TRUE)) {
      # Find split position: keep everything up to and including the №/page, split off rest
      split_pos <- regexpr(and_pattern, seg, perl = TRUE)
      if (split_pos > 0) {
        match_text <- regmatches(seg, regexpr(and_pattern, seg, perl = TRUE))
        # The part before ", and " (including the №/page)
        boundary <- sub(",\\s+and\\s+$", "", match_text, perl = TRUE)
        first <- trimws(substr(seg, 1, split_pos - 1 + nchar(boundary)))
        rest <- trimws(substr(seg, split_pos + nchar(match_text), nchar(seg)))
        result <- c(result, first, rest)
      } else {
        result <- c(result, seg)
      }
    } else {
      result <- c(result, seg)
    }
  }
  result[result != ""]
}

# Split segments where a period after closing parenthesis separates two independent citations
# e.g. "...(Alexandria: Publisher, 1422/2001). al-Ḏahabī, Taʾrīḫ al-Islām, ..."
# becomes two segments: the equality citation and the al-Ḏahabī monograph
# Pattern: ")." followed by whitespace then an author name (letter followed by comma within ~60 chars)
split_period_boundary <- function(segments) {
  # Match "). " followed by a Unicode letter (start of new author), with a comma nearby
  boundary_pattern <- "\\)\\.\\s+(?=[A-Za-zʾʿḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪūÁáÉéÍíÓóÚú][^.]{1,60},)"
  result <- c()
  for (seg in segments) {
    if (grepl(boundary_pattern, seg, perl = TRUE)) {
      # Find split position: right after "). "
      split_pos <- regexpr(boundary_pattern, seg, perl = TRUE)
      if (split_pos > 0) {
        # Split at the "). " boundary: first part gets ")", second part starts with author
        first <- trimws(substr(seg, 1, split_pos))  # includes the ")"
        rest <- trimws(substr(seg, split_pos + 1, nchar(seg)))
        rest <- sub("^\\.\\s*", "", rest, perl = TRUE)  # strip leading ". "
        result <- c(result, first, rest)
      } else {
        result <- c(result, seg)
      }
    } else {
      result <- c(result, seg)
    }
  }
  result[result != ""]
}

# Expand parsed reference into flat list
# Handles expansion of page_refs and equality sub-references
# Works for BOTH short AND long citations with multiple pages
expand_parsed_reference <- function(parsed, start_ref_num) {
  refs <- list()
  ref_num <- start_ref_num

  if (!is.null(parsed$type) && parsed$type == "short_equality") {
    # Expand each sub-reference from equality
    # Preserve raw field from parent for sub-refs that don't have it
    parent_raw <- parsed$raw
    for (subref in parsed$references) {
      # Ensure raw field is set - use parent raw or construct minimal raw
      if (is.null(subref$raw) || is.na(subref$raw) || subref$raw == "") {
        subref$raw <- if (!is.null(parent_raw) && !is.na(parent_raw)) {
          parent_raw
        } else {
          # Construct minimal raw from available fields
          paste0(subref$author %||% "", ", ", subref$title_abbrev %||% "", ", ", subref$page %||% "")
        }
      }
      if (length(subref$page_refs) > 1) {
        for (pr in subref$page_refs) {
          expanded <- subref
          expanded$page <- pr$page
          expanded$footnote <- pr$footnote
          if (!is.null(pr$page_german)) expanded$page_german <- pr$page_german
          if (!is.null(pr$page_english)) expanded$page_english <- pr$page_english
          expanded$page_refs <- list(pr)
          expanded$reference_number <- ref_num
          refs[[length(refs) + 1]] <- expanded
          ref_num <- ref_num + 1
        }
      } else {
        subref$reference_number <- ref_num
        refs[[length(refs) + 1]] <- subref
        ref_num <- ref_num + 1
      }
    }
  } else if (!is.null(parsed$page_refs) && length(parsed$page_refs) > 1) {
    # Expand multiple page_refs (works for BOTH short and long citations)
    for (pr in parsed$page_refs) {
      expanded <- parsed
      expanded$volume <- if (!is.null(pr$volume)) pr$volume else expanded$volume
      expanded$page <- pr$page
      # Propagate GdQ dual-page fields from each page_ref
      if (!is.null(pr$page_german)) expanded$page_german <- pr$page_german
      if (!is.null(pr$page_english)) expanded$page_english <- pr$page_english
      # Handle both page_cited (long) and page (short) field names
      if (!is.null(expanded$page_cited)) {
        expanded$page_cited <- pr$page
      }
      expanded$footnote <- if (!is.null(pr$footnote)) pr$footnote else NA
      expanded$page_refs <- list(pr)
      expanded$reference_number <- ref_num
      # Ensure raw field is preserved
      if (is.null(expanded$raw) || is.na(expanded$raw) || expanded$raw == "") {
        expanded$raw <- paste0(expanded$author %||% "", ", ", expanded$title_abbrev %||% expanded$title %||% "", ", ", expanded$page %||% "")
      }
      refs[[length(refs) + 1]] <- expanded
      ref_num <- ref_num + 1
    }
  } else {
    # Ensure raw field is set for single reference
    if (is.null(parsed$raw) || is.na(parsed$raw) || parsed$raw == "") {
      parsed$raw <- paste0(parsed$author %||% "", ", ", parsed$title_abbrev %||% parsed$title %||% "", ", ", parsed$page %||% parsed$page_cited %||% "")
    }
    parsed$reference_number <- ref_num
    refs[[length(refs) + 1]] <- parsed
  }

  refs
}

# Parse multiple articles with idem/eadem
# Returns list of article objects
parse_multiple_articles <- function(text) {
  text <- trimws(text)

  # Split on "; and idem," or "; and eadem,"
  parts <- strsplit(text, ";\\s*and\\s+(idem|eadem),", perl = TRUE)[[1]]

  # Extract the first author from the first citation
  first_author <- NA
  if (grepl("^([^,]+),", parts[1], perl = TRUE)) {
    first_author <- trimws(sub("^([^,]+),.*", "\\1", parts[1], perl = TRUE))
  }

  articles <- list()

  for (i in seq_along(parts)) {
    part <- trimws(parts[i])

    if (i == 1) {
      # First part has the author
      article <- parse_journal_article(part)
    } else {
      # Subsequent parts use idem/eadem - prepend the author
      reconstructed <- paste0(first_author, ", ", part)
      article <- parse_journal_article(reconstructed)
      article$idem_reference <- TRUE
    }

    article$article_number <- i
    articles[[i]] <- article
  }

  list(
    raw = text,
    type = "multiple_articles",
    shared_author = first_author,
    n_articles = length(articles),
    articles = articles
  )
}

# Parse journal article
# Schema: Author, "Title," Journal Volume.Issue (Year):pages
parse_journal_article <- function(text) {
  result <- list(
    raw = text,
    type = "journal_article",
    author = NA,
    author_type = "article_author",  # Context indicator for author field
    article_title = NA,
    journal_name = NA,
    volume = NA,
    issue = NA,
    year = NA,
    year_shamsi = NA,  # Persian solar calendar year (for consistency with other parsers)
    page_start = NA,
    page_end = NA,
    page_specific = NA,  # "at X" specific page
    section = NA,
    idem_reference = FALSE
  )

  # Extract author (before first comma)
  if (grepl("^([^,]+),", text, perl = TRUE)) {
    result$author <- trimws(sub("^([^,]+),.*", "\\1", text, perl = TRUE))
  }

  # Detect idem/eadem reference (same author as previous citation)
  if (grepl("^(idem|eadem|id\\.|ead\\.)", text, ignore.case = TRUE, perl = TRUE)) {
    result$idem_reference <- TRUE
  }

  # Extract article title (in quotes)
  title_match <- regmatches(text, regexpr('"([^"]+)"', text, perl = TRUE))
  if (length(title_match) > 0) {
    result$article_title <- trimws(gsub(',?$', "", gsub('^"|"$', "", title_match, perl = TRUE), perl = TRUE))
  }

  # Extract journal name and volume/issue/year/pages
  # Pattern after quoted title: Journal Volume.Issue (Year):pages
  # or: Journal Volume (Year):pages
  after_title <- sub('^[^"]*"[^"]*",?\\s*', "", text, perl = TRUE)

  # Try pattern: Journal Volume.Issue (Year):pages
  journal_match <- regmatches(after_title, regexpr("^([^\\d]+)\\s+(\\d+)(?:\\.(\\d+))?\\s*\\((\\d{4}(?:/\\d{4})?)\\):(\\d+)(?:–(\\d+))?", after_title, perl = TRUE))
  if (length(journal_match) > 0) {
    # Parse the components
    jm <- regmatches(after_title, regexec("^([^\\d]+)\\s+(\\d+)(?:\\.(\\d+))?\\s*\\((\\d{4}(?:/\\d{4})?)\\):(\\d+)(?:–(\\d+))?", after_title, perl = TRUE))[[1]]
    if (length(jm) >= 5) {
      result$journal_name <- trimws(jm[2])
      result$volume <- jm[3]
      result$issue <- if (jm[4] != "") jm[4] else NA
      result$year <- jm[5]
      result$page_start <- jm[6]
      result$page_end <- if (length(jm) >= 7 && jm[7] != "") jm[7] else NA
    }
  }

  # Extract "at X" specific page reference
  at_match <- regmatches(text, regexpr(",?\\s*at\\s+(\\d+(?:–\\d+)?)", text, perl = TRUE))
  if (length(at_match) > 0) {
    result$page_specific <- trimws(gsub("^,?\\s*at\\s+", "", at_match, perl = TRUE))
  }

  # Extract section
  section_match <- regmatches(text, regexpr("\\(§\\s*([^)]+)\\)", text, perl = TRUE))
  if (length(section_match) > 0) {
    result$section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_match, perl = TRUE))
  }

  result
}

# Parse book section (article in edited volume)
# Schema 1: Author, "Title," in Book Title, ed. Editor (Place: Publisher, Year), pages
# Schema 2: Author, "Title," in StudyAuthor, Book Title (Place: Publisher, Year), pages — no editor
parse_book_section <- function(text) {
  result <- list(
    raw = text,
    type = "book_section",
    author = NA,
    author_type = "chapter_author",  # Context indicator for author field
    article_title = NA,
    book_author = NA,
    book_title = NA,
    book_editor = NA,
    volumes = 1,
    series = NA,
    place = NA,
    publisher = NA,
    year_hijri = NA,
    year_gregorian = NA,
    year_shamsi = NA,  # For consistency with other parsers
    page_start = NA,
    page_end = NA,
    page_specific = NA,
    section = NA
  )

  # Extract author (before first comma)
  if (grepl("^([^,]+),", text, perl = TRUE)) {
    result$author <- trimws(sub("^([^,]+),.*", "\\1", text, perl = TRUE))
  }

  # Extract article title (in quotes)
  title_match <- regmatches(text, regexpr('"([^"]+)"', text, perl = TRUE))
  if (length(title_match) > 0) {
    result$article_title <- trimws(gsub(',?$', "", gsub('^"|"$', "", title_match, perl = TRUE), perl = TRUE))
  }

  # Extract book title and optionally book author
  # Pattern 1: "in BookTitle, ed. Editor" — edited volume
  book_match <- regmatches(text, regexpr(",?\\s*in\\s+([^,]+(?:,\\s*[^,]+)*?),\\s*eds?\\.", text, ignore.case = TRUE, perl = TRUE))
  if (length(book_match) > 0) {
    result$book_title <- trimws(gsub("^,?\\s*in\\s+|,\\s*eds?\\.$", "", book_match, ignore.case = TRUE, perl = TRUE))
  }

  # Pattern 2: "in StudyAuthor, BookTitle (Place:" — no editor, author's own book
  if (is.na(result$book_title)) {
    # Extract everything between "in " and the publication parenthetical
    in_to_pub <- regmatches(text, regexpr(",?\\s+in\\s+(.+?)\\s*\\([^)]+:\\s*[^,]+,\\s*\\d{4}", text, ignore.case = TRUE, perl = TRUE))
    if (length(in_to_pub) > 0) {
      inner <- trimws(gsub("^,?\\s+in\\s+|\\s*\\([^)]+$", "", in_to_pub, ignore.case = TRUE, perl = TRUE))
      # First comma-delimited part is the study author, rest is the book title
      first_comma <- regexpr(",\\s*", inner, perl = TRUE)
      if (first_comma > 0) {
        result$book_author <- trimws(substr(inner, 1, first_comma - 1))
        result$book_title <- trimws(sub("^,\\s*", "", substr(inner, first_comma, nchar(inner)), perl = TRUE))
      } else {
        # No comma — entire thing is the book title (no separate author)
        result$book_title <- inner
      }
    }
  }

  # Extract editor(s)
  ed_match <- regmatches(text, regexpr("eds?\\.\\s+([^(]+?)\\s*(?:,\\s*\\d+\\s*vols?\\.|\\()", text, perl = TRUE))
  if (length(ed_match) > 0) {
    editor_str <- trimws(gsub("^eds?\\.\\s*|\\s*,\\s*\\d+\\s*vols?\\.?$|\\s*\\($", "", ed_match, perl = TRUE))
    # Check if a series name is embedded at the end of the editor string
    # Pattern: ", Series Name" where Series Name contains "Handbook", "Series", "Reihe", etc.
    series_in_ed <- regmatches(editor_str, regexpr(",\\s*(Handbook\\s+.+|Silsilat[^,]+|[^,]+\\s+(?:Series|Reihe|Collection|Bibliotheca))$", editor_str, perl = TRUE))
    if (length(series_in_ed) > 0 && nchar(series_in_ed) > 0) {
      result$series <- trimws(gsub("^,\\s*", "", series_in_ed, perl = TRUE))
      editor_str <- trimws(sub(",\\s*(Handbook\\s+.+|Silsilat[^,]+|[^,]+\\s+(?:Series|Reihe|Collection|Bibliotheca))$", "", editor_str, perl = TRUE))
    }
    result$book_editor <- editor_str
  }

  # Extract volumes
  vol_match <- regmatches(text, regexpr("(\\d+)\\s*vols?\\.", text, perl = TRUE))
  if (length(vol_match) > 0) {
    result$volumes <- as.integer(gsub("\\s*vols?\\.?", "", vol_match, perl = TRUE))
  }

  # Extract publication info
  pub_match <- regmatches(text, regexpr("\\([^)]*:[^)]*(?:\\d{4}|N\\.D\\.)[^)]*\\)", text, perl = TRUE))
  if (length(pub_match) > 0) {
    pub_inner <- gsub("^\\(|\\)$", "", pub_match[length(pub_match)], perl = TRUE)

    # Check for multi-publisher (contains "and" between Place: Publisher pairs)
    if (grepl("\\s+and\\s+", pub_inner, perl = TRUE)) {
      parsed_mp <- parse_multiple_publishers(pub_inner)
      if (length(parsed_mp$places) > 0) {
        result$place <- paste(parsed_mp$places, collapse = "; ")
        result$places <- parsed_mp$places
      }
      if (length(parsed_mp$publishers) > 0) {
        result$publisher <- paste(parsed_mp$publishers, collapse = "; ")
        result$publishers <- parsed_mp$publishers
      }
      # Extract year: use per-part years if available, else from end of string
      yr <- parsed_mp$years[!is.na(parsed_mp$years)]
      year_str <- if (length(yr) > 0) yr[1] else {
        ym <- regmatches(pub_inner, regexpr("\\d{4}(?:/\\d{4})?\\s*$", pub_inner, perl = TRUE))
        if (length(ym) > 0) trimws(ym) else NA
      }
      if (!is.na(year_str)) {
        if (grepl("/", year_str, perl = TRUE)) {
          years <- strsplit(year_str, "/", perl = TRUE)[[1]]
          result$year_hijri <- years[1]
          result$year_gregorian <- years[2]
        } else if (grepl("^1[89]\\d{2}|^20\\d{2}", year_str, perl = TRUE)) {
          result$year_gregorian <- year_str
        }
      }
    } else {
      # Single publisher
      parts <- strsplit(pub_inner, ":\\s*", perl = TRUE)[[1]]
      if (length(parts) >= 2) {
        result$place <- trimws(parts[1])
        rest <- paste(parts[-1], collapse = ": ")
        pub_year <- strsplit(rest, ",\\s*(?=\\d|N\\.D\\.)", perl = TRUE)[[1]]
        if (length(pub_year) >= 1) {
          result$publisher <- trimws(pub_year[1])
        }
        if (length(pub_year) >= 2) {
          year_str <- trimws(pub_year[2])
          if (grepl("/", year_str, perl = TRUE)) {
            years <- strsplit(year_str, "/", perl = TRUE)[[1]]
            result$year_hijri <- years[1]
            result$year_gregorian <- years[2]
          } else if (grepl("^1[89]\\d{2}|^20\\d{2}", year_str, perl = TRUE)) {
            result$year_gregorian <- year_str
          }
        }
      }
    }
  }

  # Extract page range after closing parenthesis
  page_match <- regmatches(text, regexpr("\\),\\s*(\\d+)(?:[–-](\\d+))?\\.?", text, perl = TRUE))
  if (length(page_match) > 0) {
    pm <- regmatches(text, regexec("\\),\\s*(\\d+)(?:[–-](\\d+))?\\.?", text, perl = TRUE))[[1]]
    if (length(pm) >= 2) {
      result$page_start <- pm[2]
      if (length(pm) >= 3 && pm[3] != "") {
        result$page_end <- pm[3]
        result$page <- paste0(pm[2], "\u2013", pm[3])
      } else {
        result$page <- pm[2]
      }
    }
  }

  # Extract "at X" specific page
  at_match <- regmatches(text, regexpr(",?\\s*at\\s+(\\d+(?:–\\d+)?)", text, perl = TRUE))
  if (length(at_match) > 0) {
    result$page_specific <- trimws(gsub("^,?\\s*at\\s+", "", at_match, perl = TRUE))
  }

  # Extract section
  section_match <- regmatches(text, regexpr("\\(§\\s*([^)]+)\\)", text, perl = TRUE))
  if (length(section_match) > 0) {
    result$section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_match, perl = TRUE))
  }

  result
}

# Extract manuscript author from "li-Author" attribution in dissertation titles
# Common patterns:
#   "... li-Abī ʿAlī al-Ahwāzī: Dirāsah..."  → Abū ʿAlī al-Ahwāzī
#   "... li-l-imām Abī al-Qāsim Manṣūr..."    → Abū al-Qāsim Manṣūr...
#   "... li-Ibn Muǧāhid..."                     → Ibn Muǧāhid...
extract_manuscript_author <- function(title) {
  if (is.na(title) || is.null(title)) return(NA)

  # Match "li-" followed by the author name, ending at:
  #   - colon + dirāsah/taḥqīq (": Dirāsah", ": Taḥqīq")
  #   - accusative forms without colon ("dirāsatan", "taḥqīqan", "ǧamʿan")
  #   - death date parenthetical ("(tuwuffiya", "(t.", "(386")
  #   - end of string
  li_pattern <- paste0(
    "\\bli-(?:l-)?(.+?)",
    "(?:",
      ":\\s*[Dd]irāsa|",                    # : Dirāsah
      ":\\s*[Tt]aḥqīq|",                    # : Taḥqīq
      "\\s+[Dd]irāsat|",                    # dirāsatan (accusative, space-separated)
      "\\s+[Tt]aḥqīq|",                     # taḥqīqan (accusative, space-separated)
      "\\s+ǧamʿ|",                          # ǧamʿan (accusative, space-separated)
      "\\s+al-mutawaf|",                     # al-mutawaffī/al-mutafawwī (death note)
      "\\s*\\(tuwuf+iy+a|",                   # (tuwuffiya / tuwufiyya ...
      "\\s*\\(t\\.|",                        # (t. ...
      "\\s*\\(\\d{3,4}|",                    # (386 ...
      "$",                                   # end of string
    ")"
  )
  li_match <- regmatches(title, regexec(li_pattern, title, perl = TRUE))[[1]]
  if (length(li_match) < 2 || nchar(li_match[2]) == 0) return(NA)

  author_raw <- trimws(li_match[2])

  # Strip honorific prefixes (al-imām, al-šayḫ, al-ḥāfiẓ, al-ustāḏ, al-muqriʾ, etc.)
  # Loop to handle stacked honorifics (e.g., "imām al-muqriʾ")
  honorific_pattern <- "^(?:al-)?(?:imām|šayḫ|ḥāfiẓ|ustāḏ|qāḍī|muqriʾ|ʿallāmah?)\\s+"
  while (grepl(honorific_pattern, author_raw, ignore.case = TRUE, perl = TRUE)) {
    author_raw <- sub(honorific_pattern, "", author_raw, ignore.case = TRUE, perl = TRUE)
  }

  # Convert genitive kunyah to nominative: Abī → Abū (use \\s instead of \\b for Unicode safety)
  author_raw <- sub("^Abī(?=\\s)", "Abū", author_raw, perl = TRUE)
  # Also handle: ʾAbī → ʾAbū (with hamza)
  author_raw <- sub("^ʾAbī(?=\\s)", "ʾAbū", author_raw, perl = TRUE)

  # Strip trailing punctuation
  author_raw <- trimws(gsub("[,;.]+$", "", author_raw, perl = TRUE))

  if (nchar(author_raw) > 0) author_raw else NA
}

# Parse dissertation or thesis
parse_dissertation <- function(text, type = "dissertation") {
  result <- list(
    raw = text,
    type = type,
    author = NA,  # Standardized author field — manuscript author if extractable, else student
    author_type = "student_author",  # Context indicator for author field
    student_author = NA,
    manuscript_author = NA,  # The actual author of the work being edited
    title = NA,
    supervisor = NA,
    institution = NA,
    place = NA,
    year_hijri = NA,
    year_gregorian = NA,
    year_shamsi = NA,  # For consistency with other parsers
    volumes = 1,  # Default to 1
    page_cited = NA,
    section = NA,  # For consistency with other parsers
    notes = NA
  )

  # Extract student author (before first comma or before quoted title)
  if (grepl('^([^,]+),\\s*"', text, perl = TRUE)) {
    result$student_author <- trimws(sub('^([^,]+),\\s*".*', "\\1", text, perl = TRUE))
    result$author <- result$student_author  # Standardized author field
  } else if (grepl("^([^,]+),", text, perl = TRUE)) {
    result$student_author <- trimws(sub("^([^,]+),.*", "\\1", text, perl = TRUE))
    result$author <- result$student_author  # Standardized author field
  }

  # Extract title (quoted or before thesis/diss indicator)
  # Use regexec to capture the group directly, handling trailing comma/punctuation
  title_match <- regmatches(text, regexec('"([^"]+)"', text, perl = TRUE))[[1]]
  if (length(title_match) > 1) {
    # Get captured group directly (index 2), then strip trailing comma if present
    result$title <- trimws(gsub(",$", "", title_match[2], perl = TRUE))
  }

  # Extract manuscript author from "li-Author" attribution in the title
  # The dissertation title often contains the work's original author, e.g.:
  #   "al-Mūǧaz fī al-qirāʾāt li-Abī ʿAlī al-Ahwāzī: Dirāsah wa-taḥqīq"
  if (!is.na(result$title)) {
    result$manuscript_author <- extract_manuscript_author(result$title)
    # If we found a manuscript author, use it as the standardized author
    if (!is.na(result$manuscript_author)) {
      result$author <- result$manuscript_author
      result$author_type <- "manuscript_author"
    }
  }

  # Extract supervisor (after "supervised by ")
  sup_match <- regmatches(text, regexpr("supervised by\\s+([^(,]+)", text, ignore.case = TRUE, perl = TRUE))
  if (length(sup_match) > 0) {
    result$supervisor <- trimws(gsub("^supervised by\\s*", "", sup_match, ignore.case = TRUE, perl = TRUE))
  }

  # Extract institution and place from publication info
  pub_match <- regmatches(text, regexpr("\\([^:]+:\\s*[^,]+,\\s*(?:\\d{4}|N\\.D\\.)[^)]*\\)", text, perl = TRUE))
  if (length(pub_match) > 0) {
    pub_inner <- gsub("^\\(|\\)$", "", pub_match[length(pub_match)], perl = TRUE)
    parts <- strsplit(pub_inner, ":\\s*", perl = TRUE)[[1]]
    if (length(parts) >= 2) {
      result$place <- trimws(parts[1])
      rest <- paste(parts[-1], collapse = ": ")
      pub_year <- strsplit(rest, ",\\s*(?=\\d|N\\.D\\.)", perl = TRUE)[[1]]
      if (length(pub_year) >= 1) {
        result$institution <- trimws(pub_year[1])
      }
      if (length(pub_year) >= 2) {
        year_str <- trimws(pub_year[2])
        if (grepl("/", year_str, perl = TRUE)) {
          years <- strsplit(year_str, "/", perl = TRUE)[[1]]
          result$year_hijri <- years[1]
          result$year_gregorian <- years[2]
        } else if (grepl("^1[89]\\d{2}|^20\\d{2}", year_str, perl = TRUE)) {
          result$year_gregorian <- year_str
        }
      }
    }
  }

  # Extract volumes (if specified, otherwise keep default of 1)
  vol_match <- regmatches(text, regexpr("(\\d+)\\s*vols?\\.", text, perl = TRUE))
  if (length(vol_match) > 0) {
    result$volumes <- as.integer(gsub("\\s*vols?\\.?", "", vol_match, perl = TRUE))
  }

  # Extract section
  section_match <- regmatches(text, regexpr("\\(§\\s*([^)]+)\\)", text, perl = TRUE))
  if (length(section_match) > 0) {
    result$section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_match, perl = TRUE))
  }

  result
}

# Parse long form monograph citation
parse_long_monograph <- function(text) {
  # Normalize missing comma before title (e.g., "Author K. Title" -> "Author, K. Title")
  # Prioritize definite title markers (K., Kitāb) over al- which can appear in author names
  first_comma <- regexpr(",", text, perl = TRUE)

  # First, try definite title markers: K. or Kitāb
  definite_title_pattern <- "\\s+(K\\.|Kitāb\\s)"
  definite_match <- regexpr(definite_title_pattern, text, perl = TRUE)
  if (definite_match > 0 && (first_comma < 0 || definite_match < first_comma)) {
    text <- sub(definite_title_pattern, ", \\1", text, perl = TRUE)
  } else {
    # Fallback: al- with uppercase letter (but not if there's already a comma)
    al_title_pattern <- "\\s+(al-[A-ZĀĪŪĠǦḤḪṢṬẒ])"
    al_match <- regexpr(al_title_pattern, text, perl = TRUE)
    if (al_match > 0 && first_comma < 0) {
      text <- sub(al_title_pattern, ", \\1", text, perl = TRUE)
    }
  }

  result <- list(
    raw = text,
    type = "monograph",
    author = NA,
    author_type = "book_author",  # Context indicator for author field
    title = NA,
    editor = NA,
    reviewer = NA,  # For "and reviewed by [Name]"
    volumes = 1,  # Default to 1 volume
    has_index = FALSE,
    series = NA,
    series_number = NA,
    place = NA,
    places = NA,
    publisher = NA,
    publishers = NA,
    year_hijri = NA,
    year_shamsi = NA,
    year_gregorian = NA,
    printing = NA,  # New: for "second printing", etc.
    reprints = list(),  # New: list of reprint objects with bibliographic details
    volume_cited = NA,
    page_cited = NA,
    page_refs = list(),  # For multiple page citations like ), 100, 150, 200
    entry_number = NA,
    section = NA,
    notes = NA,
    comments = NA  # For user-added commentary
  )

  # Extract author (before first comma)
  if (grepl("^([^,]+),", text, perl = TRUE)) {
    result$author <- trimws(sub("^([^,]+),.*", "\\1", text, perl = TRUE))
  }

  # Extract title - multiple strategies (with perl=TRUE for Unicode)
  title_match <- regmatches(text, regexpr(",\\s*([^,]+(?:,\\s*[^,]+)*?)\\s*,\\s*eds?\\.", text, perl = TRUE))
  if (length(title_match) > 0) {
    result$title <- trimws(gsub("^,\\s*|\\s*,\\s*eds?\\.$", "", title_match, perl = TRUE))
  } else {
    title_match2 <- regmatches(text, regexpr(",\\s*([^,]+)\\s*,\\s*\\d+\\s*vols?\\.", text, perl = TRUE))
    if (length(title_match2) > 0) {
      result$title <- trimws(gsub("^,\\s*|\\s*,\\s*\\d+\\s*vols?\\.$", "", title_match2, perl = TRUE))
    } else {
      title_match3 <- regmatches(text, regexpr(",\\s*([^(]+?)\\s*\\(", text, perl = TRUE))
      if (length(title_match3) > 0) {
        candidate <- trimws(gsub("^,\\s*|\\s*\\($", "", title_match3, perl = TRUE))
        if (!grepl("^eds?\\.", candidate, perl = TRUE) && !grepl("\\d+\\s*vols?\\.", candidate, perl = TRUE)) {
          result$title <- candidate
        }
      }
    }
  }

  # Clean up title - remove volume info if it slipped through
  if (!is.na(result$title)) {
    result$title <- trimws(gsub(",?\\s*\\d+\\s*vols?\\.?\\s*$", "", result$title, perl = TRUE))
  }

  # Extract editor - match everything between "ed." and either "N vols.", "and reviewed by", or opening paren
  ed_match <- regmatches(text, regexpr("eds?\\.\\s+([^(]+?)\\s*(?:,\\s*\\d+\\s*vols?|\\s+and\\s+reviewed\\s+by|\\()", text, ignore.case = TRUE, perl = TRUE))
  if (length(ed_match) > 0) {
    editor_str <- gsub("^eds?\\.\\s*", "", ed_match, ignore.case = TRUE, perl = TRUE)
    # Remove "and reviewed by ..." portion FIRST (everything from that phrase to end)
    editor_str <- gsub("\\s+and\\s+reviewed\\s+by\\s*.*$", "", editor_str, ignore.case = TRUE, perl = TRUE)
    # Then remove volume info from end (with or without trailing period, with or without comma)
    editor_str <- gsub(",?\\s*\\d+\\s*vols?\\.?\\s*$", "", editor_str, perl = TRUE)
    # Remove opening paren from end
    editor_str <- gsub("\\s*\\($", "", editor_str, perl = TRUE)
    result$editor <- trimws(editor_str)
  }

  # Extract reviewer (after "and reviewed by") - stop at volume info or opening paren
  rev_match <- regmatches(text, regexpr("and\\s+reviewed\\s+by\\s+([^(]+?)\\s*(?:,\\s*\\d+\\s*vols?|\\()", text, ignore.case = TRUE, perl = TRUE))
  if (length(rev_match) > 0) {
    reviewer_str <- gsub("^and\\s+reviewed\\s+by\\s*", "", rev_match, ignore.case = TRUE, perl = TRUE)
    reviewer_str <- gsub(",?\\s*\\d+\\s*vols?\\.?\\s*$", "", reviewer_str, perl = TRUE)
    reviewer_str <- gsub("\\s*\\($", "", reviewer_str, perl = TRUE)
    result$reviewer <- trimws(reviewer_str)
  }

  # Extract volumes (if specified, otherwise keep default of 1)
  vol_match <- regmatches(text, regexpr("(\\d+)\\s*vols?\\.", text, perl = TRUE))
  if (length(vol_match) > 0) {
    result$volumes <- as.integer(gsub("\\s*vols?\\.?", "", vol_match, perl = TRUE))
  }

  # Check for index
  result$has_index <- grepl("\\+\\s*Index|Indices", text, ignore.case = TRUE, perl = TRUE)

  # Extract series name and number
  # Pattern: after "vols." or "vols. + Index," and before "(Place: Publisher"
  # Example: "3 vols. + Index, Silsilat al-Tarāǧim al-Andalusiyyah 7 (Tunis: ..."
  # Series often has a number at the end like "Silsilat... 7"
  series_pattern <- "vols?\\.(?:\\s*\\+\\s*Index)?\\s*,\\s*([^(]+?)\\s+(\\d+)\\s*\\("
  series_match <- regmatches(text, regexec(series_pattern, text, perl = TRUE))[[1]]
  if (length(series_match) >= 3 && series_match[1] != "") {
    result$series <- trimws(series_match[2])
    result$series_number <- series_match[3]
  } else {
    # Try without number (series without explicit number)
    series_pattern2 <- "vols?\\.(?:\\s*\\+\\s*Index)?\\s*,\\s*([^(]+?)\\s*\\("
    series_match2 <- regmatches(text, regexec(series_pattern2, text, perl = TRUE))[[1]]
    if (length(series_match2) >= 2 && series_match2[1] != "") {
      # Check if it looks like a series (not just stray text)
      candidate <- trimws(series_match2[2])
      # Series typically start with Arabic words like "Silsilat", "Maǧmūʿat", etc.
      # or contain common series indicators
      if (grepl("^(Silsilat|Maǧmūʿat|Series|Collection)|silsilah|maǧmūʿah", candidate, ignore.case = TRUE, perl = TRUE)) {
        result$series <- candidate
      }
    }
  }

  # Extract printing info (e.g., "second printing 1427/2006")
  # Use simpler pattern without en-dash to avoid encoding issues
  print_match <- regmatches(text, regexpr("(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|\\d+(?:st|nd|rd|th))\\s+printing\\s+[0-9/]+", text, ignore.case = TRUE, perl = TRUE))
  if (length(print_match) > 0) {
    # Parse into structured form with ordinal, years, and inheritance markers
    result$printing <- parse_printing_info(trimws(print_match))
  }

  # Extract reprint info with bibliographic details and strip from text for pub extraction
  # Pattern: "reprint Place: Publisher, Year" or "reprint Title, ed. Editor (Place: Publisher, Year), pages"
  text_for_pub <- text
  reprint_match <- regmatches(text, regexpr(",?\\s*reprints?\\s+([^=]+?)(?:=|$)", text, ignore.case = TRUE, perl = TRUE))
  if (length(reprint_match) > 0) {
    reprint_str <- gsub("^,?\\s*reprints?\\s*|\\s*=\\s*$|\\s*$", "", reprint_match, ignore.case = TRUE, perl = TRUE)
    result$reprints <- parse_reprint_details(reprint_str)
    text_for_pub <- sub(",?\\s*reprints?\\s+([^=]+?)(?:=|$)", "", text, ignore.case = TRUE, perl = TRUE)
  }

  # Extract publication info - find the FIRST (Place: Publisher, Year) pattern
  # (First because reprints come after)
  # Use Unicode-aware pattern - match any word character at start of place name
  # IMPORTANT: Skip parentheses that start with "(=" which are alternate titles
  # Uses negative lookahead (?!\s*=) to reject "(=" patterns at regex level
  pub_matches <- gregexpr("\\((?!\\s*=)[^()]+:\\s*[^()]+,\\s*(?:\\d{4}|N\\.D\\.)[^()]*\\)", text_for_pub, perl = TRUE)
  if (pub_matches[[1]][1] != -1) {
    all_matches <- regmatches(text_for_pub, pub_matches)[[1]]
    # Filter out alternate title parentheses that start with "(=" (with optional space after =)
    all_matches <- all_matches[!grepl("^\\(\\s*=\\s*", all_matches, perl = TRUE)]
    if (length(all_matches) == 0) {
      return(result)
    }
    pub_match <- all_matches[1]  # Take FIRST match for original publication

    pub_inner <- gsub("^\\(|\\)$", "", pub_match, perl = TRUE)

    # Check for multiple places/publishers - "and" with spaces before/after a word
    # Handles patterns like "Beirut and Tunis:" or "Place1: Pub1 and Place2: Pub2"
    if (grepl("\\s+and\\s+\\S+", pub_inner, perl = TRUE) && grepl(":", pub_inner, perl = TRUE)) {
      parsed_multi <- parse_multiple_publishers(pub_inner)
      result$places <- parsed_multi$places
      result$publishers <- parsed_multi$publishers
      result$place <- paste(parsed_multi$places, collapse = "; ")
      result$publisher <- paste(parsed_multi$publishers, collapse = "; ")

      year_match <- regmatches(pub_inner, regexpr(",\\s*((?:\\d{4}(?:–\\d{4})?/?)+|N\\.D\\.)\\s*$", pub_inner, perl = TRUE))
      if (length(year_match) > 0) {
        year_str <- trimws(gsub("^,\\s*", "", year_match, perl = TRUE))
        result <- parse_year_into_result(year_str, result)
      }
    } else {
      parts <- strsplit(pub_inner, ":\\s*", perl = TRUE)[[1]]
      if (length(parts) >= 2) {
        result$place <- trimws(parts[1])
        rest <- paste(parts[-1], collapse = ": ")
        pub_year <- strsplit(rest, ",\\s*(?=\\d|N\\.D\\.)", perl = TRUE)[[1]]
        if (length(pub_year) >= 1) {
          result$publisher <- trimws(pub_year[1])
        }
        if (length(pub_year) >= 2) {
          year_str <- trimws(pub_year[2])
          result <- parse_year_into_result(year_str, result)
        }
      }
    }
  }

  # Extract volume:page citation
  cite_match <- regmatches(text, regexpr("\\),\\s*(\\d+):(\\d+(?:–\\d+)?)", text, perl = TRUE))
  if (length(cite_match) > 0) {
    cite_parts <- strsplit(gsub("^\\),\\s*", "", cite_match, perl = TRUE), ":", perl = TRUE)[[1]]
    result$volume_cited <- cite_parts[1]
    result$page_cited <- cite_parts[2]
  } else {
    # Look for page numbers after closing paren: ), 100, 150, 200 or ), 2504
    # Pattern supports multiple comma-separated pages
    page_after_pub <- regmatches(text, regexec("\\),\\s*(\\d+(?:[–-]\\d+)?(?:\\s*,\\s*\\d+(?:[–-]\\d+)?)*)", text, perl = TRUE))
    if (length(page_after_pub[[1]]) >= 2 && page_after_pub[[1]][1] != "") {
      pages_str <- page_after_pub[[1]][2]
      # Split on comma to get individual pages
      pages <- strsplit(pages_str, "[,\\s]+", perl = TRUE)[[1]]
      pages <- pages[pages != "" & grepl("\\d", pages, perl = TRUE)]

      if (length(pages) > 1) {
        # Multiple pages - create page_refs list with split page ranges
        result$page_refs <- lapply(pages, function(p) {
          range_parts <- strsplit(trimws(p), "[–-]", perl = TRUE)[[1]]
          list(
            volume = NA,
            page_start = trimws(range_parts[1]),
            page_end = if (length(range_parts) > 1) trimws(range_parts[2]) else NA,
            page = trimws(p),  # Keep original for backwards compatibility
            footnote = NA
          )
        })
      }
      result$page_cited <- trimws(pages[1])  # First page for backwards compatibility
    }
  }

  # Extract entry number
  # Extract entry number (may be alphanumeric like №A.1.15 or №2831)
  entry_match <- regmatches(text, regexpr("№([A-Za-z0-9.]+)", text, perl = TRUE))
  if (length(entry_match) > 0) {
    result$entry_number <- gsub("№", "", entry_match, perl = TRUE)
  }

  # Extract section
  section_match <- regmatches(text, regexpr("\\(§\\s*([^)]+)\\)", text, perl = TRUE))
  if (length(section_match) > 0) {
    result$section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_match, perl = TRUE))
  }

  # =========================================================================
  # NOTES EXTRACTION
  # =========================================================================
  # Extract contextual notes that appear AFTER the core bibliographic data.
  # Based on sequence analysis, these patterns typically appear at 55-76%
  # position in the citation (after page/entry number).
  #
  # Patterns captured:
  #   - "noting that [explanation]"
  #   - "where [author] suggests/notes/states [explanation]"
  #   - "giving the title as '[title]'"
  #   - "reporting that [explanation]"
  #   - "mentioned X times"
  #
  # These provide valuable scholarly context beyond the bibliographic core.

  # Pattern 1: "noting that...", "where X suggests...", "giving the title...",
  #            "reporting that...", "described as..."
  notes_pattern <- ",\\s*(noting that|where\\s+[^,]+\\s+(?:suggests|notes|states)|giving the title|reporting that|described as)\\s+[^;=]+"
  notes_match <- regmatches(text, regexpr(notes_pattern, text, ignore.case = TRUE, perl = TRUE))
  if (length(notes_match) > 0 && notes_match != "") {
    result$notes <- trimws(gsub("^,\\s*", "", notes_match, perl = TRUE))
  }

  # Pattern 2: "mentioned X times" (common in Ibn al-Jazari Nashr index citations)
  # Only set if notes not already captured
  if (is.na(result$notes)) {
    mentions_pattern <- ",?\\s*mentioned\\s+\\d+\\s+times?"
    mentions_match <- regmatches(text, regexpr(mentions_pattern, text, ignore.case = TRUE, perl = TRUE))
    if (length(mentions_match) > 0 && mentions_match != "") {
      result$notes <- trimws(gsub("^,\\s*", "", mentions_match, perl = TRUE))
    }
  }

  result
}

# Helper to parse year string into hijri/gregorian components
parse_reprint_year <- function(year_str) {
  result <- list(year = year_str, year_hijri = NA, year_gregorian = NA)
  if (is.na(year_str) || year_str == "") return(result)
  if (grepl("/", year_str, perl = TRUE)) {
    years <- strsplit(year_str, "/", perl = TRUE)[[1]]
    if (grepl("^1[234]\\d{2}", years[1], perl = TRUE)) result$year_hijri <- years[1]
    if (length(years) > 1) result$year_gregorian <- gsub("[^0-9–-]", "", years[2], perl = TRUE)
  } else if (grepl("^1[89]\\d{2}|^20\\d{2}", year_str, perl = TRUE)) {
    result$year_gregorian <- year_str
  } else if (grepl("^1[234]\\d{2}", year_str, perl = TRUE)) {
    result$year_hijri <- year_str
  }
  result
}

# Convert ordinal text to number
# Input: "first", "second", "1st", "2nd", etc.
# Returns: numeric value or NA
ordinal_to_number <- function(ordinal_text) {
  ordinal_text <- tolower(ordinal_text)

  # Word ordinals
  word_map <- c(
    "first" = 1, "second" = 2, "third" = 3, "fourth" = 4, "fifth" = 5,
    "sixth" = 6, "seventh" = 7, "eighth" = 8, "ninth" = 9, "tenth" = 10
  )

  if (ordinal_text %in% names(word_map)) {
    return(word_map[[ordinal_text]])
  }

  # Numeric ordinals (1st, 2nd, 3rd, 4th, etc.)
  num_match <- regmatches(ordinal_text, regexpr("^\\d+", ordinal_text, perl = TRUE))
  if (length(num_match) > 0) {
    return(as.integer(num_match))
  }

  NA
}

# Parse printing info into structured form
# Input: "second printing 1411/1990" or "3rd printing 1427"
# Returns: list with ordinal, years, and inheritance markers
# Printings ALWAYS inherit place and publisher from base publication (ontological distinction from reprints)
parse_printing_info <- function(printing_str) {
  if (is.null(printing_str) || is.na(printing_str) || printing_str == "") {
    return(NULL)
  }

  result <- list(
    raw = printing_str,
    ordinal = NA,
    ordinal_text = NA,
    year_hijri = NA,
    year_gregorian = NA,
    # Printings ALWAYS inherit place and publisher from base publication
    # This is the key ontological distinction from reprints
    inherits_place = TRUE,
    inherits_publisher = TRUE
  )

  # Extract ordinal: "first", "second", "1st", "2nd", etc.
  ordinal_pattern <- "(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|\\d+(?:st|nd|rd|th))"
  ordinal_match <- regmatches(printing_str, regexpr(ordinal_pattern, printing_str, ignore.case = TRUE, perl = TRUE))

  if (length(ordinal_match) > 0) {
    result$ordinal_text <- tolower(ordinal_match)
    result$ordinal <- ordinal_to_number(result$ordinal_text)
  }

  # Extract years (hijri/gregorian) - reuse parse_reprint_year logic
  year_match <- regmatches(printing_str, regexpr("\\d{3,4}(?:/\\d{4})?", printing_str, perl = TRUE))
  if (length(year_match) > 0) {
    year_parsed <- parse_reprint_year(year_match)
    result$year_hijri <- year_parsed$year_hijri
    result$year_gregorian <- year_parsed$year_gregorian
  }

  result
}

# Parse reprint details from string like "Beirut: Publisher1 and Jeddah: Publisher2, 1408/1988"
# Also handles rich reprints: "Title, ed. Editor (Place: Publisher, Year), pages"
# Reprints have EXPLICIT new values (no inheritance) - ontological distinction from printings
parse_reprint_details <- function(reprint_str) {
  reprints <- list()

  # Rich reprint: has its own title, editor, and parenthesized publication info
  # Pattern: "Title, ed. Editor (Place: Publisher, Year), pages"
  rich_pattern <- "^(.+?),\\s*eds?\\.\\s*(.+?)\\s*\\(([^:]+):\\s*([^,)]+),\\s*(\\d{4}(?:/\\d{4})?)\\)(?:,\\s*(\\d+[–-]\\d+|\\d+))?$"
  rich_match <- regmatches(reprint_str, regexec(rich_pattern, reprint_str, perl = TRUE))[[1]]
  if (length(rich_match) > 1) {
    year_str <- rich_match[6]
    year_parsed <- parse_reprint_year(year_str)
    reprint <- list(
      title = trimws(rich_match[2]),
      editor = trimws(rich_match[3]),
      place = trimws(rich_match[4]),
      publisher = trimws(rich_match[5]),
      year = year_str,
      year_hijri = year_parsed$year_hijri,
      year_gregorian = year_parsed$year_gregorian,
      page_cited = if (nchar(rich_match[7]) > 0) trimws(rich_match[7]) else NA,
      inherits_place = FALSE,
      inherits_publisher = FALSE
    )
    reprints[[1]] <- reprint
    return(reprints)
  }

  # Check for multiple places/publishers with "and"
  if (grepl("\\s+and\\s+", reprint_str, perl = TRUE)) {
    parsed <- parse_multiple_publishers(reprint_str)
    # Fallback year from end of full string (used if per-part years unavailable)
    fallback_match <- regmatches(reprint_str, regexpr(",?\\s*(\\d{4}(?:/\\d{4})?)\\s*$", reprint_str, perl = TRUE))
    fallback_year <- if (length(fallback_match) > 0) trimws(gsub("^,?\\s*", "", fallback_match, perl = TRUE)) else NA

    for (i in seq_along(parsed$places)) {
      year_str <- if (i <= length(parsed$years) && !is.na(parsed$years[i])) parsed$years[i] else fallback_year
      year_parsed <- parse_reprint_year(year_str)
      reprint <- list(
        title = NA,
        editor = NA,
        place = parsed$places[i],
        publisher = if (i <= length(parsed$publishers)) parsed$publishers[i] else NA,
        year = year_str,
        year_hijri = year_parsed$year_hijri,
        year_gregorian = year_parsed$year_gregorian,
        page_cited = NA,
        inherits_place = FALSE,
        inherits_publisher = FALSE
      )
      reprints[[length(reprints) + 1]] <- reprint
    }
  } else {
    # Single reprint: Place: Publisher, Year
    parts <- strsplit(reprint_str, ":\\s*", perl = TRUE)[[1]]
    if (length(parts) >= 2) {
      reprint <- list(
        title = NA,
        editor = NA,
        place = trimws(parts[1]),
        publisher = NA,
        year = NA,
        year_hijri = NA,
        year_gregorian = NA,
        page_cited = NA,
        # Reprints do NOT inherit - all fields are explicit
        # This is the key ontological distinction from printings
        inherits_place = FALSE,
        inherits_publisher = FALSE
      )
      rest <- paste(parts[-1], collapse = ": ")
      pub_year <- strsplit(rest, ",\\s*", perl = TRUE)[[1]]
      if (length(pub_year) >= 1) {
        reprint$publisher <- trimws(pub_year[1])
      }
      if (length(pub_year) >= 2) {
        year_str <- trimws(pub_year[2])
        reprint$year <- year_str
        year_parsed <- parse_reprint_year(year_str)
        reprint$year_hijri <- year_parsed$year_hijri
        reprint$year_gregorian <- year_parsed$year_gregorian
      }
      reprints[[1]] <- reprint
    }
  }

  reprints
}

# Helper function to parse multiple publishers
# Handles two patterns:
# 1. "Place1 and Place2: Publisher" - multiple places, one publisher
# 2. "Place1: Publisher1 and Place2: Publisher2" - multiple places, multiple publishers
# 3. "Place1: Publisher1 and Publisher2 and Place2: Publisher3" - mixed
parse_multiple_publishers <- function(pub_inner) {
  places <- c()
  publishers <- c()

  pub_no_year <- gsub(",\\s*((?:\\d{4}(?:–\\d{4})?/?)+|N\\.D\\.)\\s*$", "", pub_inner, perl = TRUE)

  # Count colons to determine pattern
  n_colons <- length(gregexpr(":", pub_no_year, perl = TRUE)[[1]])

  if (n_colons == 1) {
    # Pattern: "Place1 and Place2: Publisher" or "Place1 and Place2 and Place3: Publisher"
    # Split on ":" first
    parts <- strsplit(pub_no_year, ":\\s*", perl = TRUE)[[1]]
    places_str <- trimws(parts[1])
    publisher <- if (length(parts) > 1) trimws(parts[2]) else NA

    # Split places on "and"
    place_parts <- strsplit(places_str, "\\s+and\\s+", perl = TRUE)[[1]]
    places <- sapply(place_parts, trimws, USE.NAMES = FALSE)
    # All places share the same publisher
    publishers <- rep(publisher, length(places))
  } else {
    # Pattern: "Place1: Publisher1 and Place2: Publisher2"
    # or "Place1: Publisher1 and Takrīt Publisher2" (without colon after second place)
    parts <- strsplit(pub_no_year, "\\s+and\\s+", perl = TRUE)[[1]]

    for (part in parts) {
      part <- trimws(part)
      if (grepl(":", part, perl = TRUE)) {
        split_part <- strsplit(part, ":\\s*", perl = TRUE)[[1]]
        places <- c(places, trimws(split_part[1]))
        if (length(split_part) > 1) {
          publishers <- c(publishers, trimws(split_part[2]))
        }
      } else {
        # Part without colon - e.g., "Takrīt Maktabat al-Amīr"
        # First word is likely place, rest is publisher
        words <- strsplit(part, "\\s+", perl = TRUE)[[1]]
        if (length(words) >= 2) {
          places <- c(places, words[1])
          publishers <- c(publishers, paste(words[-1], collapse = " "))
        } else {
          places <- c(places, part)
        }
      }
    }
  }

  # Extract per-part years by stripping from each publisher
  years <- rep(NA_character_, length(publishers))
  for (i in seq_along(publishers)) {
    yr_match <- regmatches(publishers[i], regexpr(",?\\s*(\\d{4}(?:/\\d{4})?)\\s*$", publishers[i], perl = TRUE))
    if (length(yr_match) > 0 && nchar(yr_match) > 0) {
      years[i] <- trimws(gsub("^,?\\s*", "", yr_match, perl = TRUE))
      publishers[i] <- trimws(sub(",?\\s*\\d{4}(?:/\\d{4})?\\s*$", "", publishers[i], perl = TRUE))
    }
  }

  list(places = places, publishers = publishers, years = years)
}

# Helper function to parse year string into result
parse_year_into_result <- function(year_str, result) {
  if (grepl("N\\.D\\.", year_str, ignore.case = TRUE, perl = TRUE)) {
    result$year_gregorian <- "N.D."
    return(result)
  }

  if (grepl("/", year_str, perl = TRUE)) {
    years <- strsplit(year_str, "/", perl = TRUE)[[1]]
    if (grepl("Š", years[1], perl = TRUE)) {
      result$year_shamsi <- gsub("Š", "", years[1], perl = TRUE)
    } else {
      result$year_hijri <- years[1]
    }
    result$year_gregorian <- years[2]
  } else if (grepl("^1[89]\\d{2}", year_str, perl = TRUE) || grepl("^20\\d{2}", year_str, perl = TRUE)) {
    result$year_gregorian <- year_str
  } else if (grepl("^1[234]\\d{2}", year_str, perl = TRUE)) {
    if (grepl("Š", year_str, perl = TRUE)) {
      result$year_shamsi <- gsub("Š", "", year_str, perl = TRUE)
    } else {
      result$year_hijri <- year_str
    }
  }

  result
}

# Parse equality citation (multiple editions of the same work)
parse_equality_citation <- function(text) {
  text <- trimws(text)

  # Check for "Consult further..." or "See also..." section at the end
  # This introduces related references (mostly secondary sources)
  related_references <- list()
  # Enhanced pattern: "Consult...", "See also/further", "Cf.", "For X, see"
  related_ref_pattern <- "(?:\\.\\s*|^)(Consult(?:\\s+further)?\\s+(?:the\\s+)?(?:references\\s+in)?|See\\s+(?:also|further)|Cf\\.|For\\s+[^,]+,\\s*see)\\s*"
  consult_match <- regmatches(text, regexpr(related_ref_pattern, text, ignore.case = TRUE, perl = TRUE))
  if (length(consult_match) > 0) {
    # Split the text - with guard against split_pos == -1
    split_pos <- regexpr(related_ref_pattern, text, ignore.case = TRUE, perl = TRUE)
    if (split_pos[1] != -1) {
      main_text <- trimws(substr(text, 1, split_pos - 1))
      refs_text <- trimws(substr(text, split_pos + attr(split_pos, "match.length"), nchar(text)))
    } else {
      main_text <- text
      refs_text <- ""
    }

    # Remove trailing period from refs_text if present
    refs_text <- gsub("\\.$", "", refs_text, perl = TRUE)

    # Parse the refs as serial short references
    if (nchar(refs_text) > 0) {
      ref_parts <- strsplit(refs_text, ";\\s*(?:and\\s+)?", perl = TRUE)[[1]]
      ref_parts <- trimws(ref_parts)
      ref_parts <- ref_parts[ref_parts != ""]

      related_references <- lapply(seq_along(ref_parts), function(i) {
        ref <- parse_short_citation(ref_parts[i])
        ref$reference_number <- i
        # Ibn al-Jazari, Nashr is primary; others in "Consult further" are secondary
        if (!grepl("Ibn al-Ǧazarī.*Našr|Našr.*Ibn al-Ǧazarī", ref_parts[i], perl = TRUE)) {
          ref$is_primary <- FALSE
        }
        ref
      })
    }

    text <- main_text
  }

  n_equals <- length(gregexpr("\\s+=\\s+", text, perl = TRUE)[[1]])
  if (n_equals < 1 || gregexpr("\\s+=\\s+", text, perl = TRUE)[[1]][1] == -1) {
    result <- list(editions = list(parse_long_monograph(text)), n_editions = 1)
    if (length(related_references) > 0) {
      result$related_references <- related_references
      result$n_related <- length(related_references)
    }
    return(result)
  }

  # === SEQUENTIAL FIELD INHERITANCE ===
  # Track previous segment's values for inheritance chain
  # Instead of extracting shared author/title once, we track and inherit sequentially
  segments <- strsplit(text, "\\s+=\\s+", perl = TRUE)[[1]]
  editions <- list()

  previous_author <- NA
  previous_title <- NA

  for (i in seq_along(segments)) {
    segment <- trimws(segments[i])

    # Normalize missing comma before title (e.g., "Author K. Title" -> "Author, K. Title")
    # Prioritize definite title markers (K., Kitāb) over al- which can appear in author names
    first_comma <- regexpr(",", segment, perl = TRUE)

    # First, try definite title markers: K. or Kitāb
    definite_title_pattern <- "\\s+(K\\.|Kitāb\\s)"
    definite_match <- regexpr(definite_title_pattern, segment, perl = TRUE)
    if (definite_match > 0 && (first_comma < 0 || definite_match < first_comma)) {
      segment <- sub(definite_title_pattern, ", \\1", segment, perl = TRUE)
    } else {
      # Fallback: al- with uppercase letter (but not if there's already a comma)
      al_title_pattern <- "\\s+(al-[A-ZĀĪŪĠǦḤḪṢṬẒ])"
      al_match <- regexpr(al_title_pattern, segment, perl = TRUE)
      if (al_match > 0 && first_comma < 0) {
        segment <- sub(al_title_pattern, ", \\1", segment, perl = TRUE)
      }
    }

    # Determine edition type
    edition_type <- "monograph_edition"  # Default

    # Check for journal article FIRST (most specific pattern)
    # Pattern 1 (quoted): Author, "Title," JournalName Vol.Issue (Year):Pages
    # Pattern 2 (unquoted): Author, Title, ed. Editor, JournalName (Place) Vol (Year):Pages
    journal_article_pattern <- '"[^"]+"[^"]*\\d+(?:\\.\\d+)?\\s*\\([0-9/]+\\):\\d+'
    # Strip reprint portion before checking for unquoted journal pattern
    segment_no_reprint <- sub(",?\\s*reprints?\\s+.*$", "", segment, ignore.case = TRUE, perl = TRUE)
    unquoted_journal_pattern <- "\\)\\s+\\d+(?:\\.\\d+)?\\s*\\([0-9/]+\\):\\d+"
    if (grepl(journal_article_pattern, segment, perl = TRUE)) {
      edition_type <- "journal_article"
    } else if (grepl(unquoted_journal_pattern, segment_no_reprint, perl = TRUE)) {
      edition_type <- "journal_article_unquoted"
    } else if (grepl("Ph\\.D\\.\\s*(diss|Dissertation)", segment, ignore.case = TRUE, perl = TRUE)) {
      edition_type <- "dissertation"
    } else if (grepl("M\\.A\\.\\s*Thesis", segment, ignore.case = TRUE, perl = TRUE)) {
      edition_type <- "thesis"
    } else if (i > 1 && !grepl("^eds?\\.\\s+", segment, perl = TRUE) && grepl(",\\s*[^,]+,", segment, perl = TRUE) &&
               !grepl("^(K\\.|Kitāb\\s|al-[A-ZĀĪŪĠǦḤḪṢṬẒ]|Risālat?\\s|Šarḥ\\s|Muḫtaṣar\\s|Ǧāmiʿ\\s|Tafsīr\\s)", segment, perl = TRUE) &&
               !grepl(",\\s*eds?\\.\\s+", segment, perl = TRUE)) {
      # Subsequent segment that doesn't start with "ed." and has author pattern,
      # but NOT a title-leading segment (starts with title prefix) or regular edition (has ", ed.")
      # This is likely a monograph_section (work embedded in larger study)
      edition_type <- "monograph_section"
    }

    # Detect leading field type for sequential inheritance
    leading_field <- detect_leading_field(segment)

    edition <- list(
      edition_number = i,
      edition_type = edition_type,
      editor = NA,
      study_author = NA,  # For monograph_section
      study_title = NA,   # For monograph_section
      student_author = NA,      # For dissertation/thesis
      dissertation_title = NA,  # For dissertation/thesis
      supervisor = NA,          # For dissertation/thesis
      institution = NA,         # For dissertation/thesis
      # Journal article fields (for edition_type == "journal_article")
      article_author = NA,
      article_title = NA,
      journal_name = NA,
      journal_volume = NA,
      journal_issue = NA,
      journal_pages = NA,
      journal_page_start = NA,
      journal_page_end = NA,
      volumes = 1,        # Default to 1
      series = NA,
      place = NA,
      places = NA,        # For multiple places
      publisher = NA,
      publishers = NA,    # For multiple publishers
      year_hijri = NA,
      year_shamsi = NA,
      year_gregorian = NA,
      printing = NA,       # Will be structured object if present
      reprints = list(),   # List of reprint objects with inheritance markers
      volume_cited = NA,   # For vol:page citations like 2:26-686
      page_cited = NA,     # For sections within larger work or page citations
      notes = NA,
      author = NA,
      title = NA,
      # Explicit inheritance markers for transparency and model explicability
      inherited_author = FALSE,
      inherited_title = FALSE,
      inherited_place = FALSE,      # Place always explicit in editions
      inherited_publisher = FALSE   # Publisher always explicit in editions
    )

    # Apply sequential inheritance based on leading field
    if (leading_field == "editor_only") {
      # Starts with "ed." - inherit both author and title from previous
      edition$author <- previous_author
      edition$title <- previous_title
      edition$inherited_author <- TRUE
      edition$inherited_title <- TRUE

    } else if (leading_field == "title_and_editor") {
      # Starts with title prefix (K., al-, Kitāb, etc.) - inherit author, extract new title
      edition$author <- previous_author
      edition$inherited_author <- TRUE
      edition$title <- extract_segment_title(segment)

    } else {
      # "full" - extract both author and title from segment
      if (grepl("^([^,]+),", segment, perl = TRUE)) {
        edition$author <- trimws(sub("^([^,]+),.*", "\\1", segment, perl = TRUE))
      }

      # Multi-strategy title extraction (matching parse_long_monograph logic)

      # Strategy 1: , Title, ed. (existing pattern, with perl=TRUE for Unicode)
      title_match <- regmatches(segment, regexpr(",\\s*([^,]+(?:,\\s*[^,]+)*?)\\s*,\\s*eds?\\.", segment, perl = TRUE))
      if (length(title_match) > 0) {
        edition$title <- trimws(gsub("^,\\s*|\\s*,\\s*eds?\\.$", "", title_match, perl = TRUE))
      }

      # Strategy 2: , Title, N vols. (NEW)
      if (is.na(edition$title)) {
        title_match2 <- regmatches(segment, regexpr(",\\s*([^,]+)\\s*,\\s*\\d+\\s*vols?\\.", segment, perl = TRUE))
        if (length(title_match2) > 0) {
          edition$title <- trimws(gsub("^,\\s*|\\s*,\\s*\\d+\\s*vols?\\.$", "", title_match2, perl = TRUE))
        }
      }

      # Strategy 3: , Title ( - title before publication info (NEW)
      if (is.na(edition$title)) {
        title_match3 <- regmatches(segment, regexpr(",\\s*([^(]+?)\\s*\\(", segment, perl = TRUE))
        if (length(title_match3) > 0) {
          candidate <- trimws(gsub("^,\\s*|\\s*\\($", "", title_match3, perl = TRUE))
          if (!grepl("^eds?\\.", candidate, perl = TRUE) && !grepl("\\d+\\s*vols?\\.", candidate, perl = TRUE)) {
            edition$title <- candidate
          }
        }
      }

      # Strategy 4: "Quoted Title" for dissertations (NEW)
      if (is.na(edition$title) && edition_type %in% c("dissertation", "thesis")) {
        title_match4 <- regmatches(segment, regexec('"([^"]+)"', segment, perl = TRUE))[[1]]
        if (length(title_match4) > 1) {
          edition$title <- trimws(gsub(",$", "", title_match4[2], perl = TRUE))
        }
      }
    }

    # Clean up title - remove volume info if it slipped through
    if (!is.na(edition$title)) {
      edition$title <- trimws(gsub(",?\\s*\\d+\\s*vols?\\.?\\s*$", "", edition$title, perl = TRUE))
    }

    # Clean up title - extract series name if it slipped into title
    if (!is.na(edition$title)) {
      series_in_title <- regmatches(edition$title, regexpr(",\\s*(Silsilat[^,]+|[^,]+\\s+(?:Series|Reihe|Collection|Bibliotheca))\\s*$", edition$title, perl = TRUE))
      if (length(series_in_title) > 0 && nchar(series_in_title) > 0) {
        edition$series <- trimws(gsub("^,\\s*", "", series_in_title, perl = TRUE))
        edition$title <- trimws(sub(",\\s*(Silsilat[^,]+|[^,]+\\s+(?:Series|Reihe|Collection|Bibliotheca))\\s*$", "", edition$title, perl = TRUE))
      }
    }

    # NOTE: Tracking update (previous_author, previous_title) moved to end of loop
    # after edition-type-specific parsing, so manuscript_author is available

    if (edition_type == "monograph_section") {
      # Parse as embedded work within larger study
      # Pattern: StudyAuthor, StudyTitle, N vols. (Place: Publisher, Year), pages
      if (grepl("^([^,]+),\\s*([^,]+(?:,\\s*[^,]+)*),\\s*\\d+\\s*vols?\\.", segment, perl = TRUE)) {
        study_parts <- strsplit(segment, ",\\s*", perl = TRUE)[[1]]
        edition$study_author <- trimws(study_parts[1])
        # Reconstruct study title (everything before volume info)
        title_parts <- c()
        for (j in 2:length(study_parts)) {
          if (grepl("\\d+\\s*vols?\\.", study_parts[j], perl = TRUE)) break
          title_parts <- c(title_parts, study_parts[j])
        }
        edition$study_title <- paste(title_parts, collapse = ", ")
      } else if (grepl("^([^,]+),", segment, perl = TRUE)) {
        pre_paren <- regmatches(segment, regexpr("^[^(]+", segment, perl = TRUE))
        if (length(pre_paren) > 0) {
          parts <- strsplit(pre_paren, ",\\s*", perl = TRUE)[[1]]
          if (length(parts) >= 2) {
            edition$study_author <- trimws(parts[1])
            title_parts <- trimws(parts[-1])
            # Check if last part is a series name (ends with "Series", "Reihe", "Collection", etc.)
            last_part <- title_parts[length(title_parts)]
            if (grepl("\\bSeries$|\\bReihe$|\\bCollection$|\\bBibliotheca$|\\bSilsilat\\b", last_part, perl = TRUE)) {
              edition$series <- last_part
              title_parts <- title_parts[-length(title_parts)]
            }
            edition$study_title <- paste(title_parts, collapse = ", ")
          }
        }
      }
    } else if (edition_type == "journal_article") {
      # Parse: Author, "Title," JournalName Vol.Issue (Year):Pages

      # Extract article author (before first comma before quoted title)
      author_match <- regmatches(segment, regexec("^([^,]+),\\s*\"", segment, perl = TRUE))[[1]]
      if (length(author_match) >= 2) {
        edition$article_author <- trimws(author_match[2])
      }

      # Extract article title (in quotes) - remove trailing comma if present
      title_match <- regmatches(segment, regexec('"([^"]+)"', segment, perl = TRUE))[[1]]
      if (length(title_match) >= 2) {
        edition$article_title <- trimws(gsub(",\\s*$", "", title_match[2], perl = TRUE))
      }

      # Extract journal info after the quoted title
      after_title <- sub('^[^"]*"[^"]*",?\\s*', "", segment, perl = TRUE)

      # Pattern: JournalName (OptionalLoc) Vol.Issue (Year):Pages
      journal_pattern <- "^(.+?)\\s+(\\d+)(?:\\.(\\d+))?\\s*\\(([0-9/]+)\\):(\\d+)(?:[–-](\\d+))?"
      journal_match <- regmatches(after_title, regexec(journal_pattern, after_title, perl = TRUE))[[1]]

      if (length(journal_match) >= 6) {
        edition$journal_name <- trimws(journal_match[2])
        edition$journal_volume <- journal_match[3]
        edition$journal_issue <- if (journal_match[4] != "") journal_match[4] else NA

        # Parse year (may be hijri/gregorian like 1429/2008)
        year_str <- journal_match[5]
        if (grepl("/", year_str, perl = TRUE)) {
          years <- strsplit(year_str, "/", perl = TRUE)[[1]]
          edition$year_hijri <- years[1]
          edition$year_gregorian <- years[2]
        } else {
          edition$year_gregorian <- year_str
        }

        edition$journal_page_start <- journal_match[6]
        edition$journal_page_end <- if (length(journal_match) >= 7 && journal_match[7] != "") journal_match[7] else NA
        # NOTE: journal_pages is a derived/computed field for backwards compatibility
        # Prefer using journal_page_start and journal_page_end directly
        edition$journal_pages <- paste0(journal_match[6],
          if (!is.na(edition$journal_page_end)) paste0("–", edition$journal_page_end) else "")
      }
    } else if (edition_type == "journal_article_unquoted") {
      # Parse unquoted journal article: Author, Title, ed. Editor, JournalName (Place) Vol.Issue (Year):Pages
      # Strip reprint portion first so it doesn't interfere with journal parsing
      seg_for_journal <- sub(",?\\s*reprints?\\s+.*$", "", segment, ignore.case = TRUE, perl = TRUE)

      # Extract journal info using the unquoted pattern: JournalName (Place) Vol.Issue (Year):Pages
      unquoted_journal_rx <- ",\\s*([^,]+?)\\s*\\(([^)]+)\\)\\s+(\\d+)(?:\\.(\\d+))?\\s*\\(([0-9/]+)\\):(\\d+)(?:[–-](\\d+))?"
      jmatch <- regmatches(seg_for_journal, regexec(unquoted_journal_rx, seg_for_journal, perl = TRUE))[[1]]

      if (length(jmatch) >= 7) {
        edition$journal_name <- paste0(trimws(jmatch[2]), " (", trimws(jmatch[3]), ")")
        edition$journal_volume <- jmatch[4]
        edition$journal_issue <- if (jmatch[5] != "") jmatch[5] else NA

        year_str <- jmatch[6]
        if (grepl("/", year_str, perl = TRUE)) {
          years <- strsplit(year_str, "/", perl = TRUE)[[1]]
          edition$year_hijri <- years[1]
          edition$year_gregorian <- years[2]
        } else {
          edition$year_gregorian <- year_str
        }

        edition$journal_page_start <- jmatch[7]
        edition$journal_page_end <- if (length(jmatch) >= 8 && jmatch[8] != "") jmatch[8] else NA
        edition$journal_pages <- paste0(jmatch[7],
          if (!is.na(edition$journal_page_end)) paste0("–", edition$journal_page_end) else "")

        # Extract text before the journal name match for author/title/editor
        journal_pos <- regexpr(unquoted_journal_rx, seg_for_journal, perl = TRUE)
        before_journal <- trimws(substr(seg_for_journal, 1, journal_pos - 1))

        # Extract editor: ", ed. EditorName" at end of before_journal
        ed_rx <- ",\\s*eds?\\.\\s+(.+)$"
        ed_match <- regmatches(before_journal, regexec(ed_rx, before_journal, perl = TRUE))[[1]]
        if (length(ed_match) >= 2) {
          edition$editor <- trimws(ed_match[2])
          before_journal <- trimws(sub(ed_rx, "", before_journal, perl = TRUE))
        }

        # Remaining before_journal is "Author, Title"
        # Split on first comma for author, rest is title
        first_comma <- regexpr(",\\s*", before_journal, perl = TRUE)
        if (first_comma > 0) {
          edition$article_author <- trimws(substr(before_journal, 1, first_comma - 1))
          edition$article_title <- trimws(sub("^,\\s*", "",
            substr(before_journal, first_comma, nchar(before_journal)), perl = TRUE))
        } else {
          edition$article_author <- before_journal
        }
      }

      # Normalize edition_type for downstream consistency
      edition$edition_type <- "journal_article"

    } else if (edition_type %in% c("dissertation", "thesis")) {
      # Use specialized parser for dissertation/thesis
      parsed <- parse_dissertation(segment, type = edition_type)
      edition$student_author <- parsed$student_author
      edition$manuscript_author <- parsed$manuscript_author
      # The "author" of the edition is the manuscript author (the work's original author).
      # Priority: 1) this edition's own manuscript_author (from "li-Author" in title)
      #           2) inherited author from previous edition (already set by inheritance logic)
      #           3) student_author as last resort
      if (!is.na(parsed$manuscript_author)) {
        edition$author <- parsed$manuscript_author
      } else if (is.na(edition$author) || edition$inherited_author) {
        # Keep inherited author if already set; only fall back to student if truly empty
        if (is.na(edition$author)) {
          edition$author <- parsed$student_author
        }
      }
      edition$dissertation_title <- parsed$title
      edition$title <- parsed$title  # Also populate main title field
      edition$supervisor <- parsed$supervisor
      edition$institution <- parsed$institution
      edition$place <- parsed$place
      edition$publisher <- parsed$institution  # Institution doubles as publisher for dissertations
      edition$year_hijri <- parsed$year_hijri
      edition$year_gregorian <- parsed$year_gregorian
      edition$volumes <- parsed$volumes
    } else {
      # Regular edition - extract editor
      if (i == 1) {
        ed_match <- regmatches(segment, regexpr("eds?\\.\\s+([^(]+?)\\s*(?:,\\s*[^(]+)?\\(", segment, perl = TRUE))
        if (length(ed_match) > 0) {
          editor_str <- gsub("^eds?\\.\\s*|\\s*\\($", "", ed_match, perl = TRUE)
          editor_str <- gsub(",\\s*\\d+\\s*vols?\\.?\\s*$", "", editor_str, perl = TRUE)
          editor_str <- gsub(",\\s*[^,]+$", "", editor_str, perl = TRUE)
          edition$editor <- trimws(editor_str)
        }
      } else if (grepl("^eds?\\.\\s+", segment, perl = TRUE)) {
        ed_match <- regmatches(segment, regexpr("^eds?\\.\\s+([^(]+?)\\s*(?:,\\s*[^(]+)?\\(", segment, perl = TRUE))
        if (length(ed_match) > 0) {
          editor_str <- gsub("^eds?\\.\\s*|\\s*\\($", "", ed_match, perl = TRUE)
          editor_str <- gsub(",\\s*\\d+\\s*vols?\\.?\\s*$", "", editor_str, perl = TRUE)
          edition$editor <- trimws(editor_str)
        }
      } else if (grepl(",\\s*eds?\\.\\s+", segment, perl = TRUE)) {
        # Title-leading segment with editor (e.g., "K. al-Ġāyah..., ed. Editor (Place: Publisher, Year)")
        # Extract editor from between ", ed." and opening parenthesis
        ed_match <- regmatches(segment, regexpr(",\\s*eds?\\.\\s+([^(]+?)\\s*(?:,\\s*[^(]+)?\\(", segment, perl = TRUE))
        if (length(ed_match) > 0) {
          editor_str <- gsub("^,\\s*eds?\\.\\s*|\\s*\\($", "", ed_match, perl = TRUE)
          editor_str <- gsub(",\\s*\\d+\\s*vols?\\.?\\s*$", "", editor_str, perl = TRUE)
          edition$editor <- trimws(editor_str)
        }
      }
    }

    # Extract volumes
    vol_match <- regmatches(segment, regexpr("(\\d+)\\s*vols?\\.", segment, perl = TRUE))
    if (length(vol_match) > 0) {
      edition$volumes <- as.integer(gsub("\\s*vols?\\.?", "", vol_match, perl = TRUE))
    }

    # Extract series (Arabic: Silsilat..., English: ...Series/Reihe/Collection)
    series_match <- regmatches(segment, regexpr(",\\s*(Silsilat[^(]+|[^,]+\\s+(?:Series|Reihe|Collection|Bibliotheca))\\s*\\(", segment, perl = TRUE))
    if (length(series_match) > 0) {
      edition$series <- trimws(gsub("^,\\s*|\\s*\\($", "", series_match, perl = TRUE))
    }

    # Extract printing - parse into structured form with inheritance markers
    print_match <- regmatches(segment, regexpr("(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|\\d+(?:st|nd|rd|th))\\s+printing\\s+[0-9/]+", segment, ignore.case = TRUE, perl = TRUE))
    if (length(print_match) > 0) {
      edition$printing <- parse_printing_info(trimws(print_match))
    }

    # Extract reprint info and strip it before publication info extraction
    # so the reprint's (Place: Publisher, Year) doesn't get mistaken for the base edition's
    segment_for_pub <- segment
    reprint_match <- regmatches(segment, regexpr(",?\\s*reprints?\\s+([^=]+?)(?:=|$)", segment, ignore.case = TRUE, perl = TRUE))
    if (length(reprint_match) > 0) {
      reprint_str <- gsub("^,?\\s*reprints?\\s*|\\s*=\\s*$|\\s*$", "", reprint_match, ignore.case = TRUE, perl = TRUE)
      edition$reprints <- parse_reprint_details(reprint_str)
      segment_for_pub <- sub(",?\\s*reprints?\\s+([^=]+?)(?:=|$)", "", segment, ignore.case = TRUE, perl = TRUE)
    }

    # Extract publication info (Unicode-aware) from segment with reprint stripped
    pub_match <- regmatches(segment_for_pub, regexpr("\\([^()]+:\\s*[^()]+,\\s*(?:\\d{4}|N\\.D\\.)[^()]*\\)", segment_for_pub, perl = TRUE))
    if (length(pub_match) > 0) {
      pub_inner <- gsub("^\\(|\\)$", "", pub_match[length(pub_match)], perl = TRUE)

      # Check for multiple publishers (with "and" pattern)
      if (grepl("\\s+and\\s+", pub_inner, perl = TRUE)) {
        parsed_multi <- parse_multiple_publishers(pub_inner)
        edition$places <- parsed_multi$places
        edition$publishers <- parsed_multi$publishers
        edition$place <- paste(parsed_multi$places, collapse = "; ")
        edition$publisher <- paste(parsed_multi$publishers, collapse = "; ")

        # Extract year from end
        year_match <- regmatches(pub_inner, regexpr(",\\s*((?:\\d{4}(?:–\\d{4})?/?)+|N\\.D\\.)\\s*$", pub_inner, perl = TRUE))
        if (length(year_match) > 0) {
          year_str <- trimws(gsub("^,\\s*", "", year_match, perl = TRUE))
          if (grepl("N\\.D\\.", year_str, ignore.case = TRUE, perl = TRUE)) {
            edition$year_gregorian <- "N.D."
          } else if (grepl("/", year_str, perl = TRUE)) {
            years <- strsplit(year_str, "/", perl = TRUE)[[1]]
            if (grepl("^1[234]\\d{2}", years[1], perl = TRUE)) {
              edition$year_hijri <- years[1]
            }
            if (length(years) > 1) {
              edition$year_gregorian <- gsub("[^0-9–-]", "", years[2], perl = TRUE)
            }
          } else if (grepl("^1[89]\\d{2}|^20\\d{2}", year_str, perl = TRUE)) {
            edition$year_gregorian <- gsub("[^0-9–-]", "", year_str, perl = TRUE)
          }
        }
      } else {
        # Single publisher
        parts <- strsplit(pub_inner, ":\\s*", perl = TRUE)[[1]]
        if (length(parts) >= 2) {
          edition$place <- trimws(parts[1])
          rest <- paste(parts[-1], collapse = ": ")
          pub_year <- strsplit(rest, ",\\s*(?=\\d|N\\.D\\.)", perl = TRUE)[[1]]
          if (length(pub_year) >= 1) {
            edition$publisher <- trimws(pub_year[1])
          }
          if (length(pub_year) >= 2) {
            year_str <- trimws(pub_year[2])
            year_str <- gsub("\\).*$", "", year_str, perl = TRUE)
            if (grepl("N\\.D\\.", year_str, ignore.case = TRUE, perl = TRUE)) {
              edition$year_gregorian <- "N.D."
            } else if (grepl("/", year_str, perl = TRUE)) {
              years <- strsplit(year_str, "/", perl = TRUE)[[1]]
              if (grepl("^1[234]\\d{2}", years[1], perl = TRUE)) {
                edition$year_hijri <- years[1]
              }
              if (length(years) > 1) {
                edition$year_gregorian <- gsub("[^0-9–-]", "", years[2], perl = TRUE)
              }
            } else if (grepl("^1[89]\\d{2}|^20\\d{2}", year_str, perl = TRUE)) {
              edition$year_gregorian <- gsub("[^0-9–-]", "", year_str, perl = TRUE)
            } else if (grepl("^1[234]\\d{2}", year_str, perl = TRUE)) {
              edition$year_hijri <- gsub("[^0-9–-]", "", year_str, perl = TRUE)
            }
          }
        }
      }
    }

    # Extract volume:page citation (e.g., 2:26–686 after closing paren)
    cite_match <- regmatches(segment, regexpr("\\),\\s*(\\d+):(\\d+(?:–\\d+)?)", segment, perl = TRUE))
    if (length(cite_match) > 0) {
      cite_parts <- strsplit(gsub("^\\),\\s*", "", cite_match, perl = TRUE), ":", perl = TRUE)[[1]]
      edition$volume_cited <- cite_parts[1]
      edition$page_cited <- cite_parts[2]
    }

    # Update tracking for next iteration (MUST be after edition-type-specific parsing
    # so that manuscript_author from dissertation branch is available)
    if (edition_type %in% c("dissertation", "thesis")) {
      if (!is.null(edition$manuscript_author) && !is.na(edition$manuscript_author)) {
        previous_author <- edition$manuscript_author
      }
    } else if (!is.na(edition$author)) {
      previous_author <- edition$author
    }
    if (!is.na(edition$title)) previous_title <- edition$title

    editions[[i]] <- edition
  }

  # Check if titles are consistent across editions
  unique_titles <- unique(na.omit(sapply(editions, function(e) e$title)))
  has_variant_titles <- length(unique_titles) > 1
  # Set shared title from first edition if titles are consistent, otherwise NA
  shared_title <- if (!has_variant_titles && length(unique_titles) == 1) unique_titles[1] else editions[[1]]$title

  result <- list(
    raw = text,
    type = "monograph_equality",
    author = editions[[1]]$author,  # Author from first edition
    title = shared_title,  # Set shared title if consistent, otherwise from first edition
    n_editions = length(editions),
    editions = editions,
    has_variant_titles = has_variant_titles,
    comments = NA  # For user-added commentary
  )

  # Add related references if found (from "Consult further..." section)
  if (length(related_references) > 0) {
    result$related_references <- related_references
    result$n_related <- length(related_references)
  }

  result
}

# Parse short form citation
parse_short_citation <- function(text) {
  # Guard against NA/NULL input - return empty result instead of crashing
  if (is.null(text) || length(text) == 0 || is.na(text) || text == "") {
    return(list(
      raw = NA,
      type = "short",
      author = NA,
      author_type = "short_citation_author",
      title_abbrev = NA,
      volume = NA,
      page = NA,
      page_german = NA,
      page_english = NA,
      page_codera = NA,
      page_maruf = NA,
      footnote = NA,
      page_refs = list(),
      entry_number = NA,
      entry_number_codera = NA,
      entry_number_maruf = NA,
      section = NA,
      notes = NA,
      notes_codera = NA,
      notes_maruf = NA,
      nashr_mentions = NA,
      is_primary = TRUE
    ))
  }

  # Normalize common abbreviation typos (e.g., GN → ĠN)
  text <- normalize_abbreviations(text)

  # Extract and strip section markers early to prevent (§ Q Yūsuf 12:11)
  # from being misread as volume:page by downstream patterns
  extracted_section <- NA
  section_pre_match <- regmatches(text, regexpr("\\(§\\s*([^)]+)\\)", text, perl = TRUE))
  if (length(section_pre_match) > 0) {
    extracted_section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_pre_match, perl = TRUE))
    text <- trimws(sub("\\(§\\s*[^)]+\\)", "", text, perl = TRUE))
  }

  result <- list(
    raw = text,
    type = "short",
    author = NA,
    author_type = "short_citation_author",  # Context indicator for author field
    title_abbrev = NA,
    volume = NA,
    page = NA,
    page_german = NA,      # For GdQ German original page
    page_english = NA,     # For GdQ English translation page
    page_codera = NA,      # For Ibn Khayr Fahrasah Codera edition
    page_maruf = NA,       # For Ibn Khayr Fahrasah Ma'ruf edition
    footnote = NA,         # For footnote notation (number only, no n/nn prefix)
    page_refs = list(),    # For multiple vol:page refs like "2:565, and 3:1209"
    entry_number = NA,
    entry_number_codera = NA,  # For Ibn Khayr - Codera ed. has no entry numbers
    entry_number_maruf = NA,   # For Ibn Khayr - only Ma'ruf ed. has entry numbers
    section = extracted_section,
    notes = NA,
    notes_codera = NA,     # For Ibn Khayr Fahrasah Codera edition notes
    notes_maruf = NA,      # For Ibn Khayr Fahrasah Ma'ruf edition notes
    nashr_mentions = NA,   # For "mentioned X times" in Ibn al-Jazari Nashr citations
    is_primary = TRUE
  )

  # Secondary source indicators derived from schema (is_primary == FALSE)
  secondary_indicators <- get_secondary_abbreviations()
  for (ind in secondary_indicators) {
    if (safe_grepl(ind, text, fixed = TRUE)) {
      result$is_primary <- FALSE
      break
    }
  }

  # Special handling for PUA citations
  # Format: "PUA (ed. Editor) id1234, URL:https://..." or "PUA (ed. Editor) id1234"
  pua_pattern <- "^PUA\\s*\\(ed\\.\\s*([^)]+)\\)\\s*id(\\d+)(?:\\s*,\\s*URL:\\s*(\\S+))?"
  pua_match <- regmatches(text, regexec(pua_pattern, text, perl = TRUE))[[1]]
  if (length(pua_match) >= 3 && pua_match[1] != "") {
    result$title_abbrev <- "PUA"
    result$author <- trimws(pua_match[2])
    result$author_type <- "editor"
    result$entry_number <- pua_match[3]
    if (length(pua_match) >= 4 && pua_match[4] != "") {
      result$url <- pua_match[4]
    }
    result$is_primary <- FALSE
    return(result)
  }

  # Extract author (before first comma)
  if (safe_grepl("^([^,]+),", text)) {
    result$author <- trimws(sub("^([^,]+),.*", "\\1", text, perl = TRUE))
  }

  # Extract title - handle quoted titles specially
  if (safe_grepl('"[^"]+"', text)) {
    # Quoted title: could be encyclopedia article pattern: Author, "Article," Encyclopedia
    title_match <- regmatches(text, regexpr('"[^"]+"', text, perl = TRUE))
    if (length(title_match) > 0) {
      # Check if text after the quoted title contains a known encyclopedia abbreviation
      after_quote <- trimws(sub(paste0('.*', gsub('([\\[\\](){}.*+?^$|\\\\])', '\\\\\\1', title_match, perl = TRUE), ',?\\s*'), '', text, perl = TRUE))
      # Strip leading "in" (e.g., 'Editors, "Article," in BA 6:481')
      after_quote <- sub("^in\\s+", "", after_quote, perl = TRUE)
      known_encyclopedias <- c("BA", "DMBI", "EI1", "EI2", "EI3", "EIr", "PUA", "TDVİA",
                                "GAL", "GAS", "GdQ", "MM", "Adab")
      matched_enc <- NULL
      for (enc in known_encyclopedias) {
        if (safe_grepl(paste0("^", enc, "\\b"), after_quote) || safe_grepl(paste0("^", enc, "$"), after_quote)) {
          matched_enc <- enc
          break
        }
      }
      if (!is.null(matched_enc)) {
        # Encyclopedia article: quoted string is article_title, abbreviation is title_abbrev
        result$article_title <- trimws(gsub(',\\s*$', '', gsub('^"|"$', '', title_match, perl = TRUE), perl = TRUE))
        result$title_abbrev <- matched_enc
      } else {
        # No known encyclopedia follows — treat quoted string as title_abbrev
        result$title_abbrev <- title_match
      }
    }
  } else {
    # Split on commas to get parts
    parts <- strsplit(text, ",\\s*", perl = TRUE)[[1]]
    if (length(parts) >= 2) {
      title_part <- trimws(parts[2])
      # Check if title_part starts with letters followed by space and digit (no comma between title and page)
      # Pattern: "Našr 3210" or "MM 135n3" - title then space then digits
      # NOTE: [[:alpha:]] with perl=TRUE does NOT match Unicode letters like Ġ, š, ǧ
      # so we must use perl=FALSE for POSIX class Unicode matching
      if (grepl("^[[:alpha:]-]+\\s+\\d", title_part, perl = FALSE)) {
        # No comma between title and page: extract just the title (letters before first space+digit)
        title_only <- regmatches(title_part, regexpr("^[[:alpha:]-]+", title_part, perl = FALSE))
        if (length(title_only) > 0) {
          result$title_abbrev <- trimws(title_only)
        }
      } else {
        # Standard: title is the whole second comma-separated element
        result$title_abbrev <- title_part
      }
    }
  }

  # Check for GAL Supplement pattern in standalone citations
  # Handles: "Brockelmann, GAL, Supplement 1:721/747 №4d" or "GAL, Supplement 1:350"
  # The comma-split puts "Supplement ..." in a later part when title_abbrev is already "GAL"
  is_gal_supplement <- (!is.na(result$title_abbrev) &&
                         (result$title_abbrev == "GAL" || result$title_abbrev == "Supplement") &&
                         safe_grepl("GAL", text, fixed = TRUE) &&
                         safe_grepl("Supplement", text, fixed = TRUE))
  if (is_gal_supplement) {
    result$title_abbrev <- "GAL"
    # Look for "Supplement" in remaining text after GAL
    supp_text <- sub("^.*GAL\\s*,\\s*", "", text, perl = TRUE)
    # Dual page pattern: Supplement vol:page_german/page_english [№entry]
    supp_dual <- regmatches(supp_text, regexec(
      "^Supplement\\s+(\\d+):(\\d+(?:[–-]\\d+)?)/([\\d–-]+)(?:\\s*№([A-Za-z0-9.]+))?",
      supp_text, perl = TRUE))[[1]]
    if (length(supp_dual) >= 4 && supp_dual[1] != "") {
      result$edition_qualifier <- "Supplement"
      result$volume <- supp_dual[2]
      result$page_german <- supp_dual[3]
      result$page_english <- supp_dual[4]
      result$page <- supp_dual[3]
      if (length(supp_dual) >= 5 && supp_dual[5] != "") {
        result$entry_number <- supp_dual[5]
      }
    } else {
      # Simple pattern: Supplement vol:page [№entry]
      supp_simple <- regmatches(supp_text, regexec(
        "^Supplement\\s+(\\d+):(\\d+(?:[–-]\\d+)?)(?:\\s*№([A-Za-z0-9.]+))?",
        supp_text, perl = TRUE))[[1]]
      if (length(supp_simple) >= 3 && supp_simple[1] != "") {
        result$edition_qualifier <- "Supplement"
        result$volume <- supp_simple[2]
        result$page <- supp_simple[3]
        if (length(supp_simple) >= 4 && supp_simple[4] != "") {
          result$entry_number <- supp_simple[4]
        }
      }
    }
  }

  # Check for Ibn Khayr Fahrasah dual-edition format: page/page where page/page represents [Codera]/[Ma'ruf] editions
  # Pattern: "Ibn Ḫayr, Fahrasah, 25/51 №6" or "Ibn Ḫayr, Fahrasah, 42–43, giving... / 72 №69, giving..."
  is_ibn_khayr <- !is.na(result$author) && !is.na(result$title_abbrev) &&
    safe_grepl("Ibn Ḫayr", result$author) && safe_grepl("Fahrasah", result$title_abbrev)

  if (is_ibn_khayr) {
    # Complex pattern: page, notes / page №entry, notes
    # Example: "42–43, giving the title as '...' / 72 №69, giving the title as '...'"
    complex_pattern <- ",\\s*(\\d+(?:[–-]\\d+)?)(,\\s*[^/]+)?\\s*/\\s*([\\d–-]+)(?:\\s*№([A-Za-z0-9.]+))?(,\\s*.+)?$"
    complex_match <- regmatches(text, regexec(complex_pattern, text, perl = TRUE))[[1]]

    if (length(complex_match) >= 4 && complex_match[1] != "") {
      result$page_codera <- complex_match[2]
      result$page_maruf <- complex_match[4]
      result$page <- complex_match[2]  # Set primary page to Codera for backwards compatibility
      if (length(complex_match) >= 3 && complex_match[3] != "") {
        result$notes_codera <- trimws(gsub("^,\\s*", "", complex_match[3], perl = TRUE))
      }
      if (length(complex_match) >= 5 && complex_match[5] != "") {
        # Entry number belongs to Ma'ruf edition only (Codera has no entry numbers)
        result$entry_number_maruf <- complex_match[5]
        result$entry_number_codera <- NA
        result$entry_number <- complex_match[5]  # Keep for backwards compatibility
      }
      if (length(complex_match) >= 6 && complex_match[6] != "") {
        result$notes_maruf <- trimws(gsub("^,\\s*", "", complex_match[6], perl = TRUE))
      }
    } else {
      # Simple pattern: page/page №entry
      # Example: "25/51 №6" or "34/74 №39"
      # NOTE: Entry numbers (№) only exist in the Ma'ruf edition, not Codera
      simple_pattern <- ",\\s*(\\d+(?:[–-]\\d+)?)/([\\d–-]+)(?:\\s*№([A-Za-z0-9.]+))?"
      simple_match <- regmatches(text, regexec(simple_pattern, text, perl = TRUE))[[1]]
      if (length(simple_match) >= 3 && simple_match[1] != "") {
        result$page_codera <- simple_match[2]
        result$page_maruf <- simple_match[3]
        result$page <- simple_match[2]  # Set primary page to Codera for backwards compatibility
        if (length(simple_match) >= 4 && simple_match[4] != "") {
          # Entry number belongs to Ma'ruf edition only
          result$entry_number_maruf <- simple_match[4]
          result$entry_number_codera <- NA
          result$entry_number <- simple_match[4]  # Keep for backwards compatibility
        }
      }
    }
  }

  # Check for GdQ dual-page format: volume:page_german/page_english[nN or nnN-N]
  # Pattern: 3:158/505n2 or 3:208/548-49n35 or 3:223/560nn111–12 or 3:230/566
  # Use gregexpr to find ALL GdQ patterns (handles ", and " separated refs)
  gdq_pattern <- "(\\d+):(\\d+(?:[–-]\\d+)?)/([\\d–-]+)(nn?[\\d–-]+)?"
  gdq_all <- gregexpr(gdq_pattern, text, perl = TRUE)
  if (gdq_all[[1]][1] != -1) {
    gdq_match_strings <- regmatches(text, gdq_all)[[1]]
    gdq_page_refs <- list()

    for (ms in gdq_match_strings) {
      gdq_match <- regmatches(ms, regexec(gdq_pattern, ms, perl = TRUE))[[1]]
      if (length(gdq_match) >= 4 && gdq_match[1] != "") {
        fn_val <- if (length(gdq_match) >= 5 && gdq_match[5] != "") gsub("^nn?", "", gdq_match[5], perl = TRUE) else NA
        pr <- list(
          volume = gdq_match[2],
          page = gdq_match[3],  # German page for backwards compatibility
          page_german = gdq_match[3],
          page_english = gdq_match[4],
          footnote = fn_val,
          footnote_english = fn_val  # Same value, clearer name - footnotes only apply to English ed.
        )
        gdq_page_refs[[length(gdq_page_refs) + 1]] <- pr
      }
    }

    if (length(gdq_page_refs) > 0) {
      # Check for comma-separated continuation footnotes (same English page, different footnote)
      # Example: "3:213/553n65, 553n67–68" - the second part lacks vol:page/ prefix
      last_ref <- gdq_page_refs[[length(gdq_page_refs)]]
      last_eng_page <- last_ref$page_english
      if (!is.null(last_eng_page) && !is.na(last_eng_page)) {
        # Pattern: ", 553n67–68" (same English page base, different footnote)
        # Match continuation footnotes that reference the same page
        cont_pattern <- paste0(",\\s*(", last_eng_page, ")(nn?[\\d–-]+)")
        cont_matches <- gregexpr(cont_pattern, text, perl = TRUE)
        if (cont_matches[[1]][1] != -1) {
          for (cs in regmatches(text, cont_matches)[[1]]) {
            cont_match <- regmatches(cs, regexec(cont_pattern, cs, perl = TRUE))[[1]]
            if (length(cont_match) >= 3 && cont_match[1] != "") {
              fn_val <- gsub("^nn?", "", cont_match[3], perl = TRUE)
              pr <- list(
                volume = last_ref$volume,
                page = last_ref$page_german,
                page_german = last_ref$page_german,
                page_english = cont_match[2],
                footnote = fn_val,
                footnote_english = fn_val  # Footnotes only apply to English edition
              )
              gdq_page_refs[[length(gdq_page_refs) + 1]] <- pr
            }
          }
        }
      }

      result$page_refs <- gdq_page_refs
      # Set first match as main values for backwards compatibility
      result$volume <- gdq_page_refs[[1]]$volume
      result$page_german <- gdq_page_refs[[1]]$page_german
      result$page_english <- gdq_page_refs[[1]]$page_english
      result$footnote <- gdq_page_refs[[1]]$footnote
      result$page <- gdq_page_refs[[1]]$page
    }
  } else {
    # Standard volume:page extraction (may have footnote attached)
    # Check for multiple vol:page refs like "2:565, and 3:1209"
    # Pattern: vol:page or vol:page-range, with optional footnote nN or nnN-N
    # Use gregexpr to find ALL vol:page patterns
    all_vol_page_matches <- gregexpr("(\\d+):(\\d+(?:[–-]\\d+)?)(nn?[\\d–-]+)?", text, perl = TRUE)
    if (all_vol_page_matches[[1]][1] != -1) {
      match_strings <- regmatches(text, all_vol_page_matches)[[1]]
      page_refs_list <- list()

      for (ms in match_strings) {
        vp_match <- regmatches(ms, regexec("(\\d+):(\\d+(?:[–-]\\d+)?)(nn?[\\d–-]+)?", ms, perl = TRUE))[[1]]
        if (length(vp_match) >= 3 && vp_match[1] != "") {
          page_val <- vp_match[3]
          range_parts <- strsplit(page_val, "[–-]", perl = TRUE)[[1]]
          pr <- list(
            volume = vp_match[2],
            page_start = trimws(range_parts[1]),
            page_end = if (length(range_parts) > 1) trimws(range_parts[2]) else NA,
            page = page_val,  # Keep original for backwards compatibility
            footnote = if (length(vp_match) >= 4 && vp_match[4] != "") gsub("^nn?", "", vp_match[4], perl = TRUE) else NA
          )
          page_refs_list[[length(page_refs_list) + 1]] <- pr
        }
      }

      result$page_refs <- page_refs_list

      # Set first match as main volume/page for backwards compatibility
      if (length(page_refs_list) > 0) {
        result$volume <- page_refs_list[[1]]$volume
        result$page <- page_refs_list[[1]]$page
        result$footnote <- page_refs_list[[1]]$footnote
      }
    }
  }

  # RESTRUCTURED PAGE EXTRACTION - more robust approach
  # Skip if already handled by Ibn Khayr, GdQ, or vol:page patterns above
  if (is.na(result$page) && length(result$page_refs) == 0) {

    # Strategy 1: Page after quoted title with any quote type
    # Pattern handles: "Title," 38 or "Title," 38–39 or "Title" 38 or "Title," 38n1
    # Use character class for both straight and curly quotes
    # Added '(' to terminators to handle cases like page followed by section "(§ ...)"
    quote_page_pattern <- '["""][,]?\\s*(\\d+(?:[–-]\\d+)?)(nn?[\\d–-]+)?(?:\\s|$|№|;|,|\\(|\\.)'
    quote_match <- regmatches(text, regexec(quote_page_pattern, text, perl = TRUE))[[1]]
    if (length(quote_match) >= 2 && quote_match[1] != "") {
      result$page <- quote_match[2]
      if (length(quote_match) >= 3 && quote_match[3] != "") {
        result$footnote <- gsub("^nn?", "", quote_match[3], perl = TRUE)
      }
    }

    # Strategy 2: Page before № entry marker (if still no page)
    # Example: "Author, Title, 38 №A.3.25" or "Author, Title, 38–39 №A.3.26"
    if (is.na(result$page)) {
      entry_pattern <- ',?\\s*(\\d+(?:[–-]\\d+)?)\\s*№'
      entry_match <- regmatches(text, regexec(entry_pattern, text, perl = TRUE))[[1]]
      if (length(entry_match) >= 2 && entry_match[1] != "") {
        result$page <- entry_match[2]
      }
    }

    # Strategy 3: Standard comma + page pattern (for non-quoted titles)
    # Example: "Author, Title, 219n1" or "Author, Title, 38" or "Author, Title, 3203–4(§ section)"
    # Added '(' to terminators to handle cases like "Našr, 3203–4(§ fihris)"
    if (is.na(result$page)) {
      comma_page_pattern <- ',\\s*(\\d+(?:[–-]\\d+)?)(nn?[\\d–-]+)?(?:\\s|$|№|;|,|\\(|\\.)'
      comma_match <- regmatches(text, regexec(comma_page_pattern, text, perl = TRUE))[[1]]
      if (length(comma_match) >= 2 && comma_match[1] != "") {
        result$page <- comma_match[2]
        if (length(comma_match) >= 3 && comma_match[3] != "") {
          result$footnote <- gsub("^nn?", "", comma_match[3], perl = TRUE)
        }
      }
    }

    # Strategy 4: No comma between title and page (Title page format)
    # Example: "Author, Našr 3210 (§ fihris)" or "Author, MM 135n3"
    # Match title (letters) followed by space and page number
    # Use [[:alpha:]] WITHOUT perl to match all Unicode letters (including š, ǧ, etc.)
    if (is.na(result$page)) {
      # Can't use [[:alpha:]] with perl for Unicode, so use simpler pattern
      # Match: comma, space, non-digits (title), space, digits (page)
      no_comma_pattern <- ',\\s*([^,\\d]+?)\\s+(\\d+(?:[–-]\\d+)?)(nn?[\\d–-]+)?'
      no_comma_match <- regmatches(text, regexec(no_comma_pattern, text, perl = TRUE))[[1]]
      if (length(no_comma_match) >= 3 && no_comma_match[1] != "") {
        # Verify that the captured "title" part looks like a title (starts with letter, not too long)
        potential_title <- trimws(no_comma_match[2])
        if (nchar(potential_title) <= 30 && grepl("^[[:alpha:]]", potential_title, perl = FALSE)) {
          result$page <- no_comma_match[3]
          if (length(no_comma_match) >= 4 && no_comma_match[4] != "") {
            result$footnote <- gsub("^nn?", "", no_comma_match[4], perl = TRUE)
          }
        }
      }
    }
  }

  # SEPARATE PASS: Extract footnote if page was found but footnote wasn't
  # This handles cases where footnote is attached to page like "219n1"
  if (!is.na(result$page) && is.na(result$footnote)) {
    # Look for nN or nnN-N immediately after the page number in original text
    fn_pattern <- paste0(gsub("([–-])", "[–-]", result$page, perl = TRUE), "(nn?[\\d–-]+)")
    fn_match <- regmatches(text, regexec(fn_pattern, text, perl = TRUE))[[1]]
    if (length(fn_match) >= 2 && fn_match[2] != "") {
      result$footnote <- gsub("^nn?", "", fn_match[2], perl = TRUE)
    }
  }

  # Extract entry number (may be alphanumeric like №A.1.15 or №2831)
  entry_match <- regmatches(text, regexpr("№([A-Za-z0-9.]+)", text, perl = TRUE))
  if (length(entry_match) > 0) {
    result$entry_number <- gsub("№", "", entry_match, perl = TRUE)
  }

  # Extract "mentioned X times" for Ibn al-Jazari Nashr citations
  # Pattern: "mentioned 163 times" or "mentioned X times"
  mentions_match <- regmatches(text, regexec("mentioned\\s+(\\d+)\\s+times?", text, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(mentions_match) >= 2 && mentions_match[1] != "") {
    result$nashr_mentions <- as.integer(mentions_match[2])
  }

  # =========================================================================
  # NOTES EXTRACTION (Short Form)
  # =========================================================================
  # Extract contextual notes that appear AFTER the core reference data.
  # Patterns: "noting that...", "where X suggests...", "giving the title...",
  #           "reporting that...", "mentioned X times"

  # Pattern 1: Standard contextual phrases
  notes_pattern <- ",\\s*(noting that|where\\s+[^,]+\\s+(?:suggests|notes|states)|giving the title|reporting that|described as)\\s+[^;]+"
  notes_match <- regmatches(text, regexpr(notes_pattern, text, ignore.case = TRUE, perl = TRUE))
  if (length(notes_match) > 0 && notes_match != "") {
    result$notes <- trimws(gsub("^,\\s*", "", notes_match, perl = TRUE))
  }

  # Pattern 2: "mentioned X times" - also put in notes for consistency
  # This appears after section markers like "(§ fihris), mentioned 26 times"
  if (is.na(result$notes)) {
    mentions_notes_pattern <- "[,)]\\s*mentioned\\s+\\d+\\s+times?"
    mentions_notes_match <- regmatches(text, regexpr(mentions_notes_pattern, text, ignore.case = TRUE, perl = TRUE))
    if (length(mentions_notes_match) > 0 && mentions_notes_match != "") {
      result$notes <- trimws(gsub("^[,)]\\s*", "", mentions_notes_match, perl = TRUE))
    }
  }

  result
}

# Parse serial short references (multiple short citations separated by semicolons)
# Examples:
#   "al-Ḏahabī, MQK, 2:533–38 №266; and Ibn al-Ǧazarī, ĠN, 1:44 №183"
#   "Ibn Ḫayr, Fahrasah, 25/51 №6; al-Ḏahabī, MQK, 2:728, 2:861; and Ibn al-Ǧazarī, ĠN, 1:357"
#   "al-Ḏahabī, MQK, 2:805; and Ibn al-Ǧazarī, ĠN, 1:164. Consult further the references in Ḥamdān, MM, 169n2."
parse_serial_short <- function(text) {
  text <- trimws(text)

  # Guard against NA/NULL input - return empty result instead of crashing
  if (is.null(text) || length(text) == 0 || is.na(text) || text == "") {
    return(list(
      raw = NA,
      type = "serial_short",
      n_references = 0,
      references = list(),
      related_references = list(),
      n_related = 0
    ))
  }

  # Check for "Consult further...", "See also/further", "Cf.", "For X, see" section
  # This introduces related references (mostly secondary sources)
  related_references <- list()
  related_ref_pattern <- "(?:\\.\\s*|^)(Consult(?:\\s+further)?\\s+(?:the\\s+)?(?:references\\s+in)?|See\\s+(?:also|further)|Cf\\.|For\\s+[^,]+,\\s*see)\\s*"
  consult_match <- regmatches(text, regexpr(related_ref_pattern, text, ignore.case = TRUE, perl = TRUE))
  if (length(consult_match) > 0) {
    # Split the text at related reference marker - with guard against split_pos == -1
    split_pos <- regexpr(related_ref_pattern, text, ignore.case = TRUE, perl = TRUE)
    if (split_pos[1] != -1) {
      main_text <- trimws(substr(text, 1, split_pos - 1))
      refs_text <- trimws(substr(text, split_pos + attr(split_pos, "match.length"), nchar(text)))
    } else {
      main_text <- text
      refs_text <- ""
    }

    # Remove trailing period from refs_text if present
    refs_text <- gsub("\\.$", "", refs_text, perl = TRUE)

    # Parse the refs as short references, expanding multiple page_refs into separate entries
    if (nchar(refs_text) > 0) {
      ref_parts <- strsplit(refs_text, ";\\s*(?:and\\s+)?", perl = TRUE)[[1]]
      ref_parts <- trimws(ref_parts)
      ref_parts <- ref_parts[ref_parts != ""]

      rel_ref_num <- 1
      for (i in seq_along(ref_parts)) {
        is_nashr <- safe_grepl("Ibn al-Ǧazarī.*Našr|Našr.*Ibn al-Ǧazarī", ref_parts[i])

        if (safe_grepl("\\s+=\\s+", ref_parts[i])) {
          # Equality citation - expand each sub-reference into a separate entry
          eq_ref <- parse_short_equality(ref_parts[i])
          for (subref in eq_ref$references) {
            expanded_ref <- list(
              raw = ref_parts[i],
              type = "short",
              author = eq_ref$author,
              title_abbrev = subref$title_abbrev,
              volume = subref$volume,
              page = subref$page,
              footnote = subref$footnote,
              page_refs = if (length(subref$page_refs) > 0) subref$page_refs else list(),
              entry_number = subref$entry_number,
              section = subref$section,
              is_primary = if (!is_nashr) FALSE else subref$is_primary,
              reference_number = rel_ref_num,
              equality_group = i
            )

            # If this sub-ref has multiple page_refs, expand those too
            if (length(expanded_ref$page_refs) > 1) {
              for (pr in expanded_ref$page_refs) {
                pr_ref <- expanded_ref
                pr_ref$page <- pr$page
                pr_ref$footnote <- pr$footnote
                pr_ref$page_refs <- list(pr)
                pr_ref$reference_number <- rel_ref_num
                related_references[[length(related_references) + 1]] <- pr_ref
                rel_ref_num <- rel_ref_num + 1
              }
            } else {
              related_references[[length(related_references) + 1]] <- expanded_ref
              rel_ref_num <- rel_ref_num + 1
            }
          }
        } else {
          ref <- parse_short_citation(ref_parts[i])

          # If multiple page_refs, expand into separate entries
          if (length(ref$page_refs) > 1) {
            for (pr in ref$page_refs) {
              expanded_ref <- ref
              expanded_ref$volume <- pr$volume
              expanded_ref$page <- pr$page
              expanded_ref$footnote <- pr$footnote
              expanded_ref$page_refs <- list(pr)
              expanded_ref$reference_number <- rel_ref_num
              if (!is_nashr) expanded_ref$is_primary <- FALSE
              related_references[[length(related_references) + 1]] <- expanded_ref
              rel_ref_num <- rel_ref_num + 1
            }
          } else {
            ref$reference_number <- rel_ref_num
            if (!is_nashr) ref$is_primary <- FALSE
            related_references[[length(related_references) + 1]] <- ref
            rel_ref_num <- rel_ref_num + 1
          }
        }
      }
    }

    text <- main_text
  }

  # Split main text on "; and " or ";" first
  parts <- strsplit(text, ";\\s*(?:and\\s+)?", perl = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[parts != ""]

  # Expand parts that have citation-boundary "and" (not author-name "and")
  # Citation ends with: digit, №entry, footnote nN, ) -- then "and" starts new author
  expanded_parts <- list()
  for (part in parts) {
    # Pattern: citation ending (digit, №entry, footnote, or paren) followed by " and " followed by author name
    # Author names start with letters (including Arabic/transliteration characters)
    split_pattern <- "(\\d|№[A-Za-z0-9.]+|n\\d+|\\))\\s+and\\s+([A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū])"
    if (grepl(split_pattern, part, perl = TRUE)) {
      and_pos <- regexpr("\\s+and\\s+(?=[A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū])", part, perl = TRUE)
      if (and_pos[1] != -1) {
        part1 <- trimws(substr(part, 1, and_pos[1] - 1))
        part2 <- trimws(substr(part, and_pos[1] + attr(and_pos, "match.length"), nchar(part)))
        expanded_parts <- c(expanded_parts, part1, part2)
        next
      }
    }
    expanded_parts <- c(expanded_parts, part)
  }
  parts <- unlist(expanded_parts)

  # Parse each part as a short citation (or equality citation if it contains " = ")
  # Expand citations with multiple page_refs into separate entries
  references <- list()
  ref_num <- 1

  for (i in seq_along(parts)) {
    if (safe_grepl("\\s+=\\s+", parts[i])) {
      # Equality citation - expand each sub-reference into a separate entry
      eq_ref <- parse_short_equality(parts[i])
      for (subref in eq_ref$references) {
        # Convert sub-reference to a standalone short citation entry
        expanded_ref <- list(
          raw = parts[i],
          type = "short",
          author = eq_ref$author,
          title_abbrev = subref$title_abbrev,
          volume = subref$volume,
          page = subref$page,
          footnote = subref$footnote,
          page_refs = if (length(subref$page_refs) > 0) subref$page_refs else list(),
          entry_number = subref$entry_number,
          section = subref$section,
          is_primary = subref$is_primary,
          reference_number = ref_num,
          equality_group = i  # Track that these came from same equality citation
        )

        # If this sub-ref has multiple page_refs, expand those too
        if (length(expanded_ref$page_refs) > 1) {
          for (pr in expanded_ref$page_refs) {
            pr_ref <- expanded_ref
            pr_ref$page <- pr$page
            pr_ref$footnote <- pr$footnote
            pr_ref$page_refs <- list(pr)
            pr_ref$reference_number <- ref_num
            references[[length(references) + 1]] <- pr_ref
            ref_num <- ref_num + 1
          }
        } else {
          references[[length(references) + 1]] <- expanded_ref
          ref_num <- ref_num + 1
        }
      }
    } else {
      ref <- parse_short_citation(parts[i])

      # If multiple page_refs, expand into separate entries
      if (length(ref$page_refs) > 1) {
        for (pr in ref$page_refs) {
          expanded_ref <- ref
          expanded_ref$volume <- pr$volume
          expanded_ref$page <- pr$page
          expanded_ref$footnote <- pr$footnote
          expanded_ref$page_refs <- list(pr)  # Single page_ref
          expanded_ref$reference_number <- ref_num
          references[[length(references) + 1]] <- expanded_ref
          ref_num <- ref_num + 1
        }
      } else {
        ref$reference_number <- ref_num
        references[[length(references) + 1]] <- ref
        ref_num <- ref_num + 1
      }
    }
  }

  result <- list(
    raw = text,
    type = "serial_short",
    n_references = length(references),
    references = references
  )

  # Add related references if found (from "Consult further..." section)
  if (length(related_references) > 0) {
    result$related_references <- related_references
    result$n_related <- length(related_references)
  }

  result
}

# Parse short equality citation (same author cross-referencing multiple works)
# Example: "Ḥamdān, Adab, 339–50 №220 = MM 100n1"
# The = sign indicates the same author with references to different works
parse_short_equality <- function(text) {
  text <- trimws(text)

  # Guard against NA/NULL input - return empty result instead of crashing
  if (is.null(text) || length(text) == 0 || is.na(text) || text == "") {
    return(list(
      raw = NA,
      type = "short_equality",
      author = NA,
      is_primary = TRUE,
      references = list()
    ))
  }

  # Extract shared author (before first comma)
  shared_author <- NA
  if (safe_grepl("^([^,]+),", text)) {
    shared_author <- trimws(sub("^([^,]+),.*", "\\1", text, perl = TRUE))
  }

  # Remove trailing period
  text <- gsub("\\.$", "", text, perl = TRUE)

  # Split on " = " to get individual work references
  parts <- strsplit(text, "\\s+=\\s+", perl = TRUE)[[1]]
  parts <- trimws(parts)

  # Determine if author is primary or secondary based on work abbreviations
  # Derived from schema (is_primary == FALSE) — single source of truth
  secondary_works <- get_secondary_abbreviations()
  is_primary <- TRUE
  for (sw in secondary_works) {
    if (safe_grepl(sw, text, fixed = TRUE)) {
      is_primary <- FALSE
      break
    }
  }

  # Parse each work reference
  references <- lapply(seq_along(parts), function(i) {
    part <- parts[i]

    ref <- list(
      reference_number = i,
      author = shared_author,
      title_abbrev = NA,
      volume = NA,
      page = NA,
      footnote = NA,
      page_refs = list(),   # For multiple page references like "90n3, 89nn1-4, 90n2"
      entry_number = NA,
      section = NA,
      nashr_mentions = NA,  # For "mentioned X times" in Ibn al-Jazari Nashr citations
      is_primary = is_primary
    )

    if (i == 1) {
      # First part has "Author, Work, vol:pages..." or "Author, Work, pages..."
      # Also handles "Author, Work pages..." (no comma between title and page)
      comma_parts <- strsplit(part, ",\\s*", perl = TRUE)[[1]]
      if (length(comma_parts) >= 2) {
        title_part <- trimws(comma_parts[2])
        # Check if title_part contains page info (no comma between title and page)
        # Pattern: "Adab 399–400 №276" or "ĠN 1:49" - title followed by space and digits
        if (safe_grepl("^([A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾ-]+)\\s+\\d", title_part)) {
          # Extract just the title (letters/hyphens before the first digit)
          title_only <- regmatches(title_part, regexpr("^[A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾ-]+", title_part, perl = TRUE))
          if (length(title_only) > 0) {
            ref$title_abbrev <- trimws(title_only)
          }
          # Extract page from the remaining part (after title)
          page_part <- trimws(sub("^[A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾ-]+\\s*", "", title_part, perl = TRUE))
          # Check for vol:page pattern first
          vp_match <- regmatches(page_part, regexec("^(\\d+):(\\d+(?:[–-]\\d+)?)", page_part, perl = TRUE))[[1]]
          if (length(vp_match) >= 3 && vp_match[1] != "") {
            ref$volume <- vp_match[2]
            ref$page <- vp_match[3]
          } else {
            # Just page (digits/range before any entry number or section)
            p_match <- regmatches(page_part, regexpr("^\\d+(?:[–-]\\d+)?", page_part, perl = TRUE))
            if (length(p_match) > 0) {
              ref$page <- p_match
            }
          }
        } else {
          ref$title_abbrev <- title_part
        }
      }

      # Try volume:page pattern if not already extracted (e.g., "ĠN, 2:192")
      if (is.na(ref$volume) && is.na(ref$page)) {
        vol_page_match <- regmatches(part, regexec(",\\s*(\\d+):(\\d+(?:[–-]\\d+)?)", part, perl = TRUE))[[1]]
        if (length(vol_page_match) >= 3 && vol_page_match[1] != "") {
          ref$volume <- vol_page_match[2]
          ref$page <- vol_page_match[3]
        } else {
          # Fallback: Extract page only (digits or digit-range after title, comma-separated)
          page_match <- regmatches(part, regexec(",\\s*([A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾ]+),\\s*(\\d+(?:[–-]\\d+)?)", part, perl = TRUE))[[1]]
          if (length(page_match) >= 3 && page_match[1] != "") {
            ref$page <- page_match[3]
          }
        }
      }

      # Extract entry number
      entry_match <- regmatches(part, regexpr("№([A-Za-z0-9.]+)", part, perl = TRUE))
      if (length(entry_match) > 0) {
        ref$entry_number <- gsub("№", "", entry_match, perl = TRUE)
      }

      # Extract section
      section_match <- regmatches(part, regexpr("\\(§\\s*([^)]+)\\)", part, perl = TRUE))
      if (length(section_match) > 0) {
        ref$section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_match, perl = TRUE))
      }

      # Extract "mentioned X times" for Nashr citations
      mentions_match <- regmatches(part, regexec("mentioned\\s+(\\d+)\\s+times?", part, ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(mentions_match) >= 2 && mentions_match[1] != "") {
        ref$nashr_mentions <- as.integer(mentions_match[2])
      }
    } else {
      # Subsequent parts: "Work pages[footnote]"
      # Pattern: Title page[footnote] or Title vol:page[footnote]
      # SPECIAL: "Supplement vol:page" inherits title_abbrev from first ref

      # Initialize edition_qualifier field
      ref$edition_qualifier <- NA
      ref$inherited_title <- FALSE

      # Check for GAL Supplement pattern: "Supplement vol:page_german/page_english №entry"
      # Example: "Supplement 1:720/746 №3" or "Supplement 1:721/747 №4c"
      supplement_dual_pattern <- "^Supplement\\s+(\\d+):(\\d+(?:[–-]\\d+)?)/([\\d–-]+)(?:\\s*№([A-Za-z0-9.]+))?"
      supplement_dual_match <- regmatches(part, regexec(supplement_dual_pattern, part, perl = TRUE))[[1]]
      if (length(supplement_dual_match) >= 4 && supplement_dual_match[1] != "") {
        ref$edition_qualifier <- "Supplement"
        ref$inherited_title <- TRUE
        # Inherit title_abbrev from first reference (will be set after loop)
        ref$volume <- supplement_dual_match[2]
        ref$page_german <- supplement_dual_match[3]
        ref$page_english <- supplement_dual_match[4]
        ref$page <- supplement_dual_match[3]  # German page for backwards compatibility
        if (length(supplement_dual_match) >= 5 && supplement_dual_match[5] != "") {
          ref$entry_number <- supplement_dual_match[5]
        }
      } else {
        # Check for simple Supplement pattern: "Supplement vol:page №entry"
        # Example: "Supplement 1:720 №3"
        supplement_simple_pattern <- "^Supplement\\s+(\\d+):(\\d+(?:[–-]\\d+)?)(?:\\s*№([A-Za-z0-9.]+))?"
        supplement_simple_match <- regmatches(part, regexec(supplement_simple_pattern, part, perl = TRUE))[[1]]
        if (length(supplement_simple_match) >= 3 && supplement_simple_match[1] != "") {
          ref$edition_qualifier <- "Supplement"
          ref$inherited_title <- TRUE
          ref$volume <- supplement_simple_match[2]
          ref$page <- supplement_simple_match[3]
          if (length(supplement_simple_match) >= 4 && supplement_simple_match[4] != "") {
            ref$entry_number <- supplement_simple_match[4]
          }
        } else {
          # Check for dual-page format (GAL/GdQ style): "Title vol:page_german/page_english"
          dual_page_pattern <- "^([A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾŠšČčŽž]+)\\s+(\\d+):(\\d+(?:[–-]\\d+)?)/([\\d–-]+)(?:\\s*№([A-Za-z0-9.]+))?"
          dual_page_match <- regmatches(part, regexec(dual_page_pattern, part, perl = TRUE))[[1]]
          if (length(dual_page_match) >= 5 && dual_page_match[1] != "") {
            ref$title_abbrev <- dual_page_match[2]
            ref$volume <- dual_page_match[3]
            ref$page_german <- dual_page_match[4]
            ref$page_english <- dual_page_match[5]
            ref$page <- dual_page_match[4]  # German page for backwards compatibility
            if (length(dual_page_match) >= 6 && dual_page_match[6] != "") {
              ref$entry_number <- dual_page_match[6]
            }
          } else {
            # Check for volume:page pattern (includes transliteration chars like š, ǧ)
            vol_page_match <- regmatches(part, regexec("^([A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾŠšČčŽž]+)\\s+(\\d+):(\\d+(?:–\\d+)?)(nn?[\\d–]+)?", part, perl = TRUE))[[1]]
            if (length(vol_page_match) >= 4 && vol_page_match[1] != "") {
              ref$title_abbrev <- vol_page_match[2]
              ref$volume <- vol_page_match[3]
              ref$page <- vol_page_match[4]
              if (length(vol_page_match) >= 5 && vol_page_match[5] != "") {
                ref$footnote <- gsub("^nn?", "", vol_page_match[5], perl = TRUE)
              }
            } else {
        # Title and page(s): "MM 100n1" or "MM 90n3, 89nn1-4, 90n2, 90n4" or "Našr 3227"
        # First extract the title (letters at start, including transliteration chars)
        # Character class includes: š (hacek), ǧ, and other Arabic transliteration chars
        title_match <- regmatches(part, regexpr("^[A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾŠšČčŽž]+", part, perl = TRUE))
        if (length(title_match) > 0) {
          ref$title_abbrev <- title_match
          # Get everything after the title, stripping "mentioned X times" to avoid
          # the count being misinterpreted as a page number
          page_part <- trimws(sub("^[A-Za-zĀāĪīŪūḌḏĠǦḪḫṢṣṬṭẒẓʿʾŠšČčŽž]+\\s*", "", part, perl = TRUE))
          page_part <- gsub(",?\\s*mentioned\\s+\\d+\\s+times?", "", page_part, ignore.case = TRUE, perl = TRUE)

          # Check if there are multiple comma-separated page references
          # Pattern for page ref: digits (optional range) with optional footnote
          # Use both hyphen - and en-dash – in character classes
          page_ref_pattern <- "(\\d+(?:[–-]\\d+)?)(nn?[\\d–-]+)?"

          # Split by comma and parse each page reference
          page_parts <- strsplit(page_part, ",\\s*", perl = TRUE)[[1]]
          page_refs_list <- list()

          for (j in seq_along(page_parts)) {
            pp <- trimws(page_parts[j])
            pr_match <- regmatches(pp, regexec(page_ref_pattern, pp, perl = TRUE))[[1]]
            if (length(pr_match) >= 2 && pr_match[1] != "") {
              pr <- list(
                page = pr_match[2],
                footnote = if (length(pr_match) >= 3 && pr_match[3] != "") gsub("^nn?", "", pr_match[3], perl = TRUE) else NA
              )
              page_refs_list[[length(page_refs_list) + 1]] <- pr
            }
          }

          ref$page_refs <- page_refs_list

          # Set first page ref as the main page/footnote for backwards compatibility
          if (length(page_refs_list) > 0) {
            ref$page <- page_refs_list[[1]]$page
            ref$footnote <- page_refs_list[[1]]$footnote
          }
        } else {
          # Fallback: just set title to the whole part
          ref$title_abbrev <- trimws(gsub("\\d.*$", "", part, perl = TRUE))
          # Try to extract page
          page_extract <- regmatches(part, regexpr("\\d+(?:–\\d+)?", part, perl = TRUE))
          if (length(page_extract) > 0) {
            ref$page <- page_extract
          }
          # Try to extract footnote
          fn_extract <- regmatches(part, regexpr("nn?[\\d–]+", part, perl = TRUE))
          if (length(fn_extract) > 0) {
            ref$footnote <- gsub("^nn?", "", fn_extract, perl = TRUE)
          }
        }  # close title_match if/else
      }  # close vol_page_match if/else
    }  # close dual_page_match if/else
  }  # close supplement_simple_match if/else
}  # close supplement_dual_match if/else

      # Extract entry number if present (skip if already extracted for Supplement)
      if (is.na(ref$entry_number)) {
        entry_match <- regmatches(part, regexpr("№([A-Za-z0-9.]+)", part, perl = TRUE))
        if (length(entry_match) > 0) {
          ref$entry_number <- gsub("№", "", entry_match, perl = TRUE)
        }
      }

      # Extract section if present
      section_match <- regmatches(part, regexpr("\\(§\\s*([^)]+)\\)", part, perl = TRUE))
      if (length(section_match) > 0) {
        ref$section <- trimws(gsub("^\\(§\\s*|\\)$", "", section_match, perl = TRUE))
      }

      # Extract "mentioned X times" for Nashr citations
      mentions_match <- regmatches(part, regexec("mentioned\\s+(\\d+)\\s+times?", part, ignore.case = TRUE, perl = TRUE))[[1]]
      if (length(mentions_match) >= 2 && mentions_match[1] != "") {
        ref$nashr_mentions <- as.integer(mentions_match[2])
      }
    }

    ref
  })

  # Post-processing: Inherit title_abbrev for Supplement references
  # The first reference provides the title_abbrev for subsequent Supplement references
  if (length(references) > 1) {
    first_title <- references[[1]]$title_abbrev
    for (i in 2:length(references)) {
      if (isTRUE(references[[i]]$inherited_title) && !is.null(first_title) && !is.na(first_title)) {
        references[[i]]$title_abbrev <- first_title
      }
    }
  }

  list(
    raw = text,
    type = "short_equality",
    author = shared_author,
    is_primary = is_primary,
    n_references = length(references),
    references = references
  )
}

# Master parser for citation sequences
# Splits on semicolons FIRST, then routes each segment to appropriate parser
# This is the main entry point for parsing complex citation sequences
parse_citation_sequence <- function(text) {
  # Guard against NA/NULL input - return empty result instead of crashing
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return(list(
      raw = NA,
      raw_original = NA,
      type = "citation_sequence",
      n_references = 0,
      references = list(),
      related_references = list(),
      n_related = 0,
      input_normalized = FALSE
    ))
  }

  # Store original input before any normalization
  raw_original <- text

  # Apply comprehensive input normalization
  # This handles copy/paste issues: smart quotes, dashes, whitespace, Unicode forms
  text <- normalize_input(text)

  # Track whether normalization changed anything (for transparency)
  input_was_normalized <- (raw_original != text)

  # Guard against empty string after normalization
  if (text == "") {
    return(list(
      raw = "",
      raw_original = raw_original,
      type = "citation_sequence",
      n_references = 0,
      references = list(),
      related_references = list(),
      n_related = 0,
      input_normalized = input_was_normalized
    ))
  }

  # Normalize common abbreviation typos (e.g., GN → ĠN)
  text <- normalize_abbreviations(text)

  # Step 1: Extract related reference section (if present)
  # Patterns: "Consult...", "See also/further", "Cf.", "For X, see"
  # Also match when input starts with "Consult..." (no main citations before it)
  related_text <- NULL
  consult_pattern <- "(?:\\.\\s*|^)(Consult(?:\\s+further)?\\s+(?:the\\s+)?(?:references\\s+in)?|See\\s+(?:also|further)|Cf\\.|For\\s+[^,]+,\\s*see)\\s*"
  if (isTRUE(grepl(consult_pattern, text, ignore.case = TRUE, perl = TRUE))) {
    split_pos <- regexpr(consult_pattern, text, ignore.case = TRUE, perl = TRUE)
    # Guard against split_pos == -1 before arithmetic
    if (split_pos[1] != -1) {
      main_text <- trimws(substr(text, 1, split_pos - 1))
      related_text <- trimws(substr(text, split_pos + attr(split_pos, "match.length"), nchar(text)))
      related_text <- gsub("\\.$", "", related_text, perl = TRUE)
    } else {
      main_text <- gsub("\\.$", "", text, perl = TRUE)
    }
  } else {
    main_text <- gsub("\\.$", "", text, perl = TRUE)
  }

  # Step 1b: Extract embedded short references from narrative text
  # Pattern: "in [Author], [Work], [page] [№entry]" or "identified in [Author], [Work]..."
  # These appear between the main citation and "Consult further" section
  embedded_refs <- list()
  embedded_pattern <- "\\b(identified as [^.]*|identified|described|mentioned|noted|cited|given)\\s+(?:as [^.]*\\s+)?in\\s+([A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū][^,]+),\\s+([A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū][^,]+),\\s*(\\d+(?:[–-]\\d+)?(?:\\s*№[A-Za-z0-9.]+)?(?:,\\s*[^.]+)?)"
  embedded_matches <- gregexpr(embedded_pattern, main_text, perl = TRUE, ignore.case = TRUE)
  if (embedded_matches[[1]][1] != -1) {
    matched_strings <- regmatches(main_text, embedded_matches)[[1]]
    for (match_str in matched_strings) {
      # Extract just the citation part: "Author, Work, page [№entry]"
      # Pattern to extract: after "in " until end
      cite_part <- sub("^.*\\bin\\s+", "", match_str, ignore.case = TRUE, perl = TRUE)
      if (nchar(cite_part) > 0) {
        embedded_refs[[length(embedded_refs) + 1]] <- cite_part
      }
    }
  }

  # Step 2: Smart-split main text on semicolons (respecting parentheses)
  # Then further split on ", citing " to separate chained references
  segments <- split_comma_and(split_period_boundary(split_citing(smart_split_semicolon(main_text))))

  # Step 3: Parse each segment based on its type
  references <- list()
  ref_num <- 1

  for (seg in segments) {
    seg <- trimws(seg)
    if (nchar(seg) == 0) next

    seg_type <- detect_citation_type(seg)

    parsed <- switch(seg_type$type,
      "monograph" = parse_long_monograph(seg),
      "monograph_equality" = parse_equality_citation(seg),
      "journal_article" = parse_journal_article(seg),
      "book_section" = parse_book_section(seg),
      "dissertation" = parse_dissertation(seg),
      "thesis" = parse_dissertation(seg, type = "thesis"),
      "short_equality" = parse_short_equality(seg),
      "serial_short" = parse_serial_short(seg),
      "multiple_articles" = parse_multiple_articles(seg),
      parse_short_citation(seg)  # default for short, primary, secondary, secondary_gdq
    )

    # Handle expansion for different types
    refs_to_add <- expand_parsed_reference(parsed, ref_num)
    for (r in refs_to_add) {
      r$is_primary <- if (is.null(r$is_primary)) TRUE else r$is_primary
      references[[length(references) + 1]] <- r
      ref_num <- ref_num + 1
    }
  }

  # Step 3b: Process embedded short references from narrative text
  # These are short citations found in phrases like "identified in Sirāj al-Dīn al-Qazwīnī, Mašyaḫah, 135 №14"
  if (length(embedded_refs) > 0) {
    for (embed_cite in embedded_refs) {
      embed_cite <- trimws(embed_cite)
      if (nchar(embed_cite) == 0) next

      embed_parsed <- parse_short_citation(embed_cite)
      embed_parsed$is_primary <- FALSE  # Embedded refs are supporting citations
      embed_parsed$source_type <- "embedded_narrative"
      embed_parsed$raw <- embed_cite

      refs_to_add <- expand_parsed_reference(embed_parsed, ref_num)
      for (r in refs_to_add) {
        r$is_primary <- FALSE
        r$source_type <- "embedded_narrative"
        references[[length(references) + 1]] <- r
        ref_num <- ref_num + 1
      }
    }
  }

  # Step 4: Process related references similarly (from "Consult further..." section)
  related_references <- list()
  if (!is.null(related_text) && nchar(related_text) > 0) {
    rel_segments <- split_comma_and(split_period_boundary(split_citing(smart_split_semicolon(related_text))))
    rel_num <- 1

    for (seg in rel_segments) {
      seg <- trimws(seg)
      if (nchar(seg) == 0) next

      seg_type <- detect_citation_type(seg)

      parsed <- switch(seg_type$type,
        "monograph" = parse_long_monograph(seg),
        "monograph_equality" = parse_equality_citation(seg),
        "journal_article" = parse_journal_article(seg),
        "book_section" = parse_book_section(seg),
        "dissertation" = parse_dissertation(seg),
        "thesis" = parse_dissertation(seg, type = "thesis"),
        "short_equality" = parse_short_equality(seg),
        "serial_short" = parse_serial_short(seg),
        parse_short_citation(seg)  # default for short, primary, secondary
      )

      # Handle expansion
      refs_to_add <- expand_parsed_reference(parsed, rel_num)
      for (r in refs_to_add) {
        # Mark as NOT primary unless it's Ibn al-Jazari Nashr
        is_nashr <- safe_grepl("Ibn al-Ǧazarī.*Našr|Našr.*Ibn al-Ǧazarī", seg)
        r$is_primary <- if (is_nashr) TRUE else FALSE
        # Preserve raw field for display in viewer - use segment text if not set
        r$raw <- if (is.null(r$raw) || is.na(r$raw) || r$raw == "") seg else r$raw
        related_references[[length(related_references) + 1]] <- r
        rel_num <- rel_num + 1
      }
    }
  }

  list(
    raw = text,
    raw_original = raw_original,
    type = "citation_sequence",
    n_references = length(references),
    references = references,
    related_references = related_references,
    n_related = length(related_references),
    input_normalized = input_was_normalized
  )
}

# Extract citations from running prose text
# This function identifies citation start patterns and extracts citation strings
# for further parsing with the standard parsers
#
# Examples:
#   "...discussed in Jan Restö, "The Problem of...," in Clause Combining..., 297–364, at 336..."
#   "Compare with the voweling "Samayfaʿ" in Nasser, The Second Canonization, 17..."
#
# Returns a list of extracted citation objects with raw text, detected form, and source type
extract_citations_from_prose <- function(text) {
  extracted <- list()

  # Pattern 1: Long-form citations - Author, "Title," in Book, ed. Editor (Place: Pub, Year), pages
  # Look for: Name, "quoted title"... followed by publication info (Place: Publisher, Year)
  # Character class for Arabic/transliteration names: includes ʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū
  long_pattern <- '([A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū][^,]+,\\s*"[^"]+,"[^)]+\\([^:]+:[^,]+,\\s*\\d{4}[^)]*\\)[^.]*)'
  long_matches <- gregexpr(long_pattern, text, perl = TRUE)
  if (long_matches[[1]][1] != -1) {
    for (m in regmatches(text, long_matches)[[1]]) {
      extracted[[length(extracted) + 1]] <- list(
        raw = trimws(m),
        form = "long",
        source = "prose_extraction"
      )
    }
  }

  # Pattern 2: Short-form citations with known abbreviations
  # Known short-form indicators: common abbreviated titles
  short_indicators <- c("Našr", "ĠN", "MQK", "MM", "Adab", "GdQ", "GAL", "Fihrist", "Fahrasah")
  for (ind in short_indicators) {
    # Match: AuthorName, AbbreviatedTitle, pages/volume info
    short_pattern <- paste0('([A-Za-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū][^,]+,\\s*', ind, '[^.;]*\\d+[^.;]*)')
    short_matches <- gregexpr(short_pattern, text, perl = TRUE)
    if (short_matches[[1]][1] != -1) {
      for (m in regmatches(text, short_matches)[[1]]) {
        extracted[[length(extracted) + 1]] <- list(
          raw = trimws(m),
          form = "short",
          source = "prose_extraction"
        )
      }
    }
  }

  # Pattern 3: Short-form with capitalized author + title + page
  # Example: "Nasser, The Second Canonization, 17"
  # Generic pattern: CapitalizedName, Title Words, page
  generic_short <- '([A-Z][a-zʿʾḌḏĠǦḪḫṢṣṬṭẒẓĀāĪīŪū]+,\\s+[A-Z][^,]+,\\s*\\d+(?:[–-]\\d+)?)'
  gen_matches <- gregexpr(generic_short, text, perl = TRUE)
  if (gen_matches[[1]][1] != -1) {
    for (m in regmatches(text, gen_matches)[[1]]) {
      # Verify it looks like a citation (has comma then digits)
      if (grepl(',\\s*\\d+', m, perl = TRUE)) {
        # Check it wasn't already captured by other patterns
        already_captured <- FALSE
        for (existing in extracted) {
          if (grepl(m, existing$raw, fixed = TRUE) || grepl(existing$raw, m, fixed = TRUE)) {
            already_captured <- TRUE
            break
          }
        }
        if (!already_captured) {
          extracted[[length(extracted) + 1]] <- list(
            raw = trimws(m),
            form = "short",
            source = "prose_extraction"
          )
        }
      }
    }
  }

  # Character length heuristic check - can help classify ambiguous cases
  # Short citations: Typically 30-80 characters
  # Long citations: Typically 150-400+ characters
  for (i in seq_along(extracted)) {
    len <- nchar(extracted[[i]]$raw)
    extracted[[i]]$char_length <- len
    # Add heuristic confidence based on length
    if (extracted[[i]]$form == "short" && len > 150) {
      extracted[[i]]$length_hint <- "may_be_long"
    } else if (extracted[[i]]$form == "long" && len < 100) {
      extracted[[i]]$length_hint <- "may_be_short"
    } else {
      extracted[[i]]$length_hint <- "consistent"
    }
  }

  extracted
}

# ============================================================================
# CSL-JSON Export Functions (Zotero-compatible)
# ============================================================================

# Generate a unique CSL ID from parsed citation
generate_csl_id <- function(parsed) {
  # Create ID from author + year + first word of title
  author <- parsed$author %||% parsed$article_author %||% parsed$student_author %||% "Unknown"
  author_clean <- gsub("[^A-Za-z]", "", substr(author, 1, 20), perl = TRUE)

  year <- parsed$year_gregorian %||% parsed$year_hijri %||% "nd"
  year_clean <- gsub("[^0-9]", "", substr(as.character(year), 1, 4), perl = TRUE)

  title <- parsed$title %||% parsed$article_title %||% parsed$dissertation_title %||% ""
  title_word <- gsub("[^A-Za-z]", "", strsplit(title, "\\s+", perl = TRUE)[[1]][1] %||% "untitled", perl = TRUE)

  paste0(tolower(author_clean), year_clean, tolower(title_word))
}

# Convert parsed citation to CSL-JSON format
# Now uses approved schemas from parser-work-schemas.json for type resolution
to_csl_json <- function(parsed) {
  # Try to get CSL type from schema based on title_abbrev
  title_abbrev <- parsed$title_abbrev
  schema <- NULL
  if (!is.null(title_abbrev) && !is.na(title_abbrev)) {
    schema <- get_work_schema(title_abbrev)
  }

  # Determine CSL type - prefer schema, fall back to type-based switch
  if (!is.null(schema)) {
    csl_type <- schema$csl_type
  } else {
    csl_type <- switch(parsed$type %||% "book",
      "monograph" = "book",
      "monograph_equality" = "book",
      "journal_article" = "article-journal",
      "article" = "article-journal",
      "book_section" = "chapter",
      "dissertation" = "thesis",
      "thesis" = "thesis",
      "short" = "book",
      "short_equality" = "book",
      "secondary" = "book",
      "secondary_gdq" = "book",
      "book"  # default
    )
  }

  # Build title - for encyclopedias, combine article_title with entry_number
  title_value <- parsed$title %||% parsed$article_title %||% parsed$dissertation_title %||% NA
  if (csl_type == "entry-encyclopedia" || csl_type == "webpage") {
    # Use article_title if available
    if (!is.null(parsed$article_title) && !is.na(parsed$article_title)) {
      title_value <- parsed$article_title
    }
    # Append entry number to title for searchability
    if (!is.null(parsed$entry_number) && !is.na(parsed$entry_number)) {
      title_value <- paste0(title_value, " (№", parsed$entry_number, ")")
    }
  }

  # Build base CSL item
  csl <- list(
    id = generate_csl_id(parsed),
    type = csl_type,
    title = title_value
  )

  # Add container-title for encyclopedias and webpages
  if (csl_type == "entry-encyclopedia" || csl_type == "webpage") {
    if (!is.null(schema)) {
      csl$`container-title` <- schema$full_title
    } else if (!is.null(title_abbrev) && !is.na(title_abbrev)) {
      csl$`container-title` <- title_abbrev
    }
  }

  # Add authors - handle different patterns based on schema properties
  author_field <- NULL

  # For encyclopedias/webpages: check schema properties for author handling
  if (!is.null(schema) && (csl_type == "entry-encyclopedia" || csl_type == "webpage")) {
    # Check if work has article authors
    if (isTRUE(schema$properties$has_article_authors)) {
      author_field <- parsed$article_author %||% parsed$author
    }
    # Check if editors should be used as authors (when no article author)
    if (is.null(author_field) || is.na(author_field)) {
      if (isTRUE(schema$properties$editors_as_authors)) {
        author_field <- parsed$editor %||% schema$author %||% parsed$author
      }
    }
  }

  # Fall back to standard author handling
  if (is.null(author_field) || is.na(author_field)) {
    author_field <- if (csl_type == "article-journal") {
      parsed$article_author %||% parsed$author
    } else if (csl_type == "thesis") {
      parsed$student_author %||% parsed$author
    } else {
      parsed$author %||% parsed$article_author %||% parsed$student_author
    }
  }

  if (!is.null(author_field) && !is.na(author_field)) {
    csl$author <- list(list(literal = author_field))
  }

  # Add editors if present
  if (!is.null(parsed$editor) && !is.na(parsed$editor)) {
    csl$editor <- list(list(literal = parsed$editor))
  }

  # Add URL for webpages
  if (csl_type == "webpage") {
    if (!is.null(parsed$url) && !is.na(parsed$url)) {
      csl$URL <- parsed$url
    }
    # Add accessed date
    csl$accessed <- list(`date-parts` = list(list(
      as.integer(format(Sys.Date(), "%Y")),
      as.integer(format(Sys.Date(), "%m")),
      as.integer(format(Sys.Date(), "%d"))
    )))
  }

  # Type-specific fields
  if (csl_type == "article-journal") {
    if (!is.null(parsed$journal_name) && !is.na(parsed$journal_name)) {
      csl$`container-title` <- parsed$journal_name
    }
    if (!is.null(parsed$journal_volume) && !is.na(parsed$journal_volume)) {
      csl$volume <- parsed$journal_volume
    }
    if (!is.null(parsed$journal_issue) && !is.na(parsed$journal_issue)) {
      csl$issue <- parsed$journal_issue
    }
    if (!is.null(parsed$journal_pages) && !is.na(parsed$journal_pages)) {
      csl$page <- parsed$journal_pages
    }
  } else if (csl_type == "chapter") {
    if (!is.null(parsed$book_title) && !is.na(parsed$book_title)) {
      csl$`container-title` <- parsed$book_title
    }
  } else if (csl_type == "thesis") {
    csl$genre <- if (parsed$type == "dissertation") "Ph.D. dissertation" else "M.A. thesis"
    if (!is.null(parsed$supervisor) && !is.na(parsed$supervisor)) {
      csl$note <- paste0("Supervised by ", parsed$supervisor)
    }
  }

  # Add date
  year <- parsed$year_gregorian %||% parsed$year_hijri
  if (!is.null(year) && !is.na(year) && year != "N.D.") {
    year_num <- as.integer(gsub("[^0-9].*", "", as.character(year), perl = TRUE))
    if (!is.na(year_num)) {
      csl$issued <- list(`date-parts` = list(list(year_num)))
    }
  }

  # Add place/publisher
  if (!is.null(parsed$place) && !is.na(parsed$place)) {
    csl$`publisher-place` <- parsed$place
  }
  if (!is.null(parsed$publisher) && !is.na(parsed$publisher)) {
    csl$publisher <- parsed$publisher
  }

  # Add volumes if > 1
  if (!is.null(parsed$volumes) && !is.na(parsed$volumes) && parsed$volumes > 1) {
    csl$`number-of-volumes` <- as.character(parsed$volumes)
  }

  # Add Hijri year as note if present
  if (!is.null(parsed$year_hijri) && !is.na(parsed$year_hijri)) {
    hijri_note <- paste0("Hijri: ", parsed$year_hijri)
    if (!is.null(csl$note)) {
      csl$note <- paste0(csl$note, "; ", hijri_note)
    } else {
      csl$note <- hijri_note
    }
  }

  # Build Extra field for non-standard data (Zotero convention)
  extra_parts <- c()

  # Handle multi-edition pagination (GAL, GdQ, Fahrasah, ǦB)
  if (!is.null(schema) && isTRUE(schema$properties$multi_edition_pagination)) {
    ep <- schema$edition_pagination
    if (!is.null(ep) && !is.null(ep$editions) && length(ep$editions) > 0) {
      for (ed in ep$editions) {
        # Get the page field for this edition (page_german, page_english, page_codera, page_maruf, etc.)
        page_field <- paste0("page_", ed$key)
        page_val <- parsed[[page_field]]
        if (!is.null(page_val) && !is.na(page_val)) {
          extra_parts <- c(extra_parts, paste0("Page (", ed$label, "): ", page_val))
        }
      }
    }
  } else {
    # Standard page handling for books
    if (csl_type == "book" && !is.null(parsed$page) && !is.na(parsed$page)) {
      csl$page <- parsed$page
    }
  }

  # Add volume for short citations
  if (!is.null(parsed$volume) && !is.na(parsed$volume)) {
    csl$volume <- parsed$volume
  }

  # Add entry number to Extra field (for bio-bibliographies like GAL, ĠN, etc.)
  if (!is.null(parsed$entry_number) && !is.na(parsed$entry_number)) {
    # For encyclopedias, entry number is already in title - skip here
    if (csl_type != "entry-encyclopedia" && csl_type != "webpage") {
      extra_parts <- c(extra_parts, paste0("Entry: ", parsed$entry_number))
    }
  }

  # Add edition_qualifier to Extra field (e.g., "Supplement")
  if (!is.null(parsed$edition_qualifier) && !is.na(parsed$edition_qualifier)) {
    extra_parts <- c(extra_parts, paste0("Edition-Qualifier: ", parsed$edition_qualifier))
  }

  # Add section to Extra field
  if (!is.null(parsed$section) && !is.na(parsed$section)) {
    extra_parts <- c(extra_parts, paste0("Section: ", parsed$section))
  }

  # Add footnote to Extra field
  if (!is.null(parsed$footnote) && !is.na(parsed$footnote)) {
    extra_parts <- c(extra_parts, paste0("Note: fn. ", parsed$footnote))
  }

  # Look up URL/DOI for encyclopedia articles from database
  # This applies to entry-encyclopedia types (EI2, EI3, DMBI, TDVİA, etc.)
  if (csl_type == "entry-encyclopedia" && !is.null(title_abbrev)) {
    # Use article_title for lookup (which should be the entry name)
    lookup_title <- parsed$article_title %||% title_value
    if (!is.null(lookup_title) && !is.na(lookup_title)) {
      # Clean up title - remove entry number suffix if present
      lookup_title <- gsub("\\s*\\(№[^)]+\\)\\s*$", "", lookup_title)

      url_info <- lookup_article_url(title_abbrev, lookup_title)
      if (!is.null(url_info)) {
        if (url_info$url_type == "doi") {
          # Add DOI (extract from full URL)
          csl$DOI <- extract_doi_from_url(url_info$url)
          # Also add URL for convenience
          csl$URL <- url_info$url
        } else {
          # Add URL directly
          csl$URL <- url_info$url
        }
        # Add accessed date for online resources
        if (is.null(csl$accessed)) {
          csl$accessed <- list(`date-parts` = list(list(
            as.integer(format(Sys.Date(), "%Y")),
            as.integer(format(Sys.Date(), "%m")),
            as.integer(format(Sys.Date(), "%d"))
          )))
        }
      }
    }
  }

  # Combine Extra field parts with existing note
  if (length(extra_parts) > 0) {
    extra_str <- paste(extra_parts, collapse = "\n")
    if (!is.null(csl$note)) {
      csl$note <- paste0(csl$note, "\n", extra_str)
    } else {
      csl$note <- extra_str
    }
  }

  # Remove NULL/NA values for cleaner JSON
  csl <- csl[!sapply(csl, function(x) is.null(x) || (length(x) == 1 && is.na(x)))]

  csl
}

# Convert a monograph_equality citation to multiple CSL items (one per edition)
monograph_equality_to_csl <- function(parsed) {
  csl_items <- list()

  if (is.null(parsed$editions) || length(parsed$editions) == 0) {
    # No editions, just convert the main citation
    return(list(to_csl_json(parsed)))
  }

  for (i in seq_along(parsed$editions)) {
    ed <- parsed$editions[[i]]

    # Merge edition fields with main citation fields
    merged <- list(
      type = ed$edition_type %||% "monograph",
      author = ed$author %||% parsed$author,
      title = ed$title %||% parsed$title,
      editor = ed$editor,
      place = ed$place,
      publisher = ed$publisher,
      year_gregorian = ed$year_gregorian,
      year_hijri = ed$year_hijri,
      volumes = ed$volumes,
      # Journal article fields
      article_author = ed$article_author,
      article_title = ed$article_title,
      journal_name = ed$journal_name,
      journal_volume = ed$journal_volume,
      journal_issue = ed$journal_issue,
      journal_pages = ed$journal_pages,
      # Dissertation fields
      student_author = ed$student_author,
      dissertation_title = ed$dissertation_title,
      supervisor = ed$supervisor
    )

    csl_items[[i]] <- to_csl_json(merged)
    # Make ID unique per edition
    csl_items[[i]]$id <- paste0(csl_items[[i]]$id, "_ed", i)
  }

  csl_items
}

# Batch export citations to CSL-JSON file
export_to_csl_json <- function(citations, output_file) {
  csl_items <- list()

  for (c in citations) {
    # Get the parsed data (handle both adjudicated format and direct parsed)
    parsed <- c$parsed %||% c

    if (parsed$type == "monograph_equality") {
      # Expand equality citations to multiple items
      items <- monograph_equality_to_csl(parsed)
      csl_items <- c(csl_items, items)
    } else {
      csl_items[[length(csl_items) + 1]] <- to_csl_json(parsed)
    }
  }

  # Write to file
  jsonlite::write_json(csl_items, output_file, pretty = TRUE, auto_unbox = TRUE)

  invisible(csl_items)
}
