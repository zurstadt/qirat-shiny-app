# Bayesian Multinomial Shiny App - REFACTORED VERSION
# Progressive UX with Auto-initialization and Integrated Methodology Text
# Based on UX reconfiguration plan and Bayesian methodology documents

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
# library(leaflet)  # Removed with Geographic Explorer (see geographic_alpha.R)
# library(sf)  # Removed with Geographic Explorer (see geographic_alpha.R)

# Note: cmdstanr not required - using pre-computed Bayesian results for cloud deployment
# library(cmdstanr)  # Disabled for Posit Connect Cloud

# Database path (relative to app directory for deployment)
DB_PATH <- "data/iqsa_bibliography.db"

# Pre-computed Bayesian results (replaces live MCMC)
PRECOMPUTED_PATH <- "data/precomputed_bayesian_results.rds"
PRECOMPUTED <- if (file.exists(PRECOMPUTED_PATH)) {
  readRDS(PRECOMPUTED_PATH)
} else {
  NULL
}

# Primary/secondary citation classification from parser-work-schemas.json
WORK_SCHEMAS <- tryCatch({
  raw <- jsonlite::fromJSON("data/parser-work-schemas.json")$schemas
  raw
}, error = function(e) NULL)
PRIMARY_ABBREVS <- if (!is.null(WORK_SCHEMAS)) {
  WORK_SCHEMAS$abbrev[WORK_SCHEMAS$is_primary == TRUE]
} else {
  c("\u0120N", "Fahrasah", "\u01F0B", "TMD", "\u1E62ilah", "al-\u1E0Eayl wa-l-takmilah",
    "Sullam", "al-\u1E62ilat al-\u1E2Balaf", "Ma\u0161ya\u1E2Bah", "MQK", "Na\u0161r", "Fihrist", "SAN")
}

# Scholar routes JSON path (used by Geographic Explorer - see geographic_alpha.R)
# SCHOLAR_ROUTES_PATH <- "routes/all_scholar_routes.json"

# GIS sea route geometries (coastal-following paths)
# SEA_ROUTES_GIS_PATH <- "routes/sea_routes_gis.json"  # Removed with Geographic Explorer
# SEA_ROUTE_GEOMETRIES_PATH <- "routes/sea_route_geometries.json"  # Removed with Geographic Explorer

# Mediterranean ports for sea route visualization (removed with Geographic Explorer)
# MEDITERRANEAN_PORTS_PATH <- "routes/mediterranean_ports.json"

# Sea route category colors removed with Geographic Explorer (see geographic_alpha.R)

# Tufte-inspired theme for all visualizations
theme_tufte_custom <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # Remove major gridlines, keep very subtle minor ones
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.25),

      # Remove panel background and border
      panel.background = element_blank(),
      panel.border = element_blank(),

      # Minimal axis lines - only show data range
      axis.line = element_line(color = "gray40", linewidth = 0.5),
      axis.ticks = element_line(color = "gray40", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),

      # Clean text
      axis.title = element_text(color = "gray20", size = rel(0.9)),
      axis.text = element_text(color = "gray30", size = rel(0.85)),

      # Minimal plot title
      plot.title = element_text(color = "gray10", size = rel(1.1),
                                face = "plain", hjust = 0),
      plot.subtitle = element_text(color = "gray40", size = rel(0.9),
                                    face = "plain", hjust = 0),
      plot.caption = element_text(color = "gray50", size = rel(0.75),
                                  hjust = 0, margin = margin(t = 10)),

      # Facet strips - minimal
      strip.background = element_blank(),
      strip.text = element_text(color = "gray20", size = rel(0.95),
                               face = "plain", hjust = 0),

      # Legend minimal
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(color = "gray30", size = rel(0.9)),
      legend.text = element_text(color = "gray30", size = rel(0.85)),

      # Minimal plot margin
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}

# Okabe-Ito colorblind-friendly palette for academic publication
# These colors are used consistently throughout the project
COLORS <- list(
  # Reading systems (sets) - colors for debugging ordering
  system = c(
    "7"   = "#E69F00",   # Orange
    "7+1" = "#009E73",   # Green
    "10+" = "#0072B2"    # Blue
  ),
  # Regions
  region = c(
    "maġrib" = "#56B4E9",  # Sky Blue (cool, western)
    "mašriq" = "#E69F00"   # Orange (warm, eastern)
  ),
  # For lost/extant works
  extant = c(
    "Extant" = "#009E73",  # Bluish Green
    "Lost"   = "#999999"   # Gray
  ),
  # MCMC chain colors (for diagnostics - up to 8 chains)
  chains = c(
    "#0072B2",  # Blue
    "#D55E00",  # Vermillion
    "#009E73",  # Bluish Green
    "#E69F00",  # Orange
    "#56B4E9",  # Sky Blue
    "#CC79A7",  # Reddish Purple
    "#F0E442",  # Yellow
    "#000000"   # Black
  ),
  # ESS diagnostic colors
  ess = c(
    "Bulk" = "#0072B2",   # Blue
    "Tail" = "#D55E00"    # Vermillion
  ),
  # Correlation heatmap
  correlation = c(
    low = "#56B4E9",      # Sky Blue (negative)
    mid = "#FFFFFF",      # White (zero)
    high = "#D55E00"      # Vermillion (positive)
  )
)

# Helper: softmax function
softmax <- function(x) {
  ex <- exp(x = x - max(x))
  ex / sum(ex)
}

# Helper: Format Arabic transliteration in proper case
# type = "title" -> Only capitalize first word (after K.), rest lowercase
# type = "author" -> Capitalize each name/nisbah component
#
# AUTHOR FORMATTING RULES:
# 1. SLASH VARIANTS: Extract canonical (first) form only for display
#    - "al-ʿāṣī/al-ʿāṣ b. ḫalaf" → canonical: "al-ʿĀṣī b. Ḫalaf"
#    - The "/" represents an alternative form in the same position
#
# 2. FIRST POSITION:
#    - "B." or "b." → "Ibn" (expand abbreviation at start of name)
#    - "ibn" → "Ibn"
#    - "al-" → "al-" (always lowercase, even at start)
#
# 3. MID-NAME PATRONYMICS:
#    - "ibn" → "b." (abbreviate)
#    - "bint" → "bt." (abbreviate)
#    - "b."/"bt." → stay lowercase
#
# 4. KUNYAS (ʾAbū, ʾAbī, ʾUmm):
#    - Capitalize the kunya AND the following name component
#    - "ʾabū bakr" → "ʾAbū Bakr"
#    - "ʾabū al-fatḥ" → "ʾAbū al-Fatḥ"
#
# 5. al- PREFIX: Always lowercase, capitalize word after hyphen
#
# 6. PARTICLES: Always lowercase (fī, min, ʿan, ʿalā, ilā, maʿa, bi, wa, li)
#
# 7. ALL OTHER WORDS: Capitalize first real letter (after ʾ/ʿ)

format_camel_case <- function(text, type = "author") {
  if (is.na(text) || text == "" || is.null(text)) return(text)

  # For author names, first extract canonical form from slash variants
  # Slash "/" represents alternative forms - take first (canonical) form for each word
  if (type == "author") {
    # Process each word to extract canonical form before splitting
    text <- gsub("([^/\\s]+)/[^\\s]+", "\\1", as.character(text))
  }

  # Split into words
  words <- strsplit(as.character(text), "\\s+")[[1]]
  if (length(words) == 0) return(text)

  # Capitalize first "real" letter, accounting for ʾ (hamza) and ʿ (ayn)
  capitalize_first_real <- function(word) {
    if (nchar(word) == 0) return(word)
    chars <- strsplit(word, "")[[1]]
    if (length(chars) == 0) return(word)
    for (j in seq_along(chars)) {
      ch <- chars[j]
      if (!(ch %in% c("ʾ", "ʿ"))) {
        chars[j] <- toupper(ch)
        break
      }
    }
    paste0(chars, collapse = "")
  }

  # Check if a word is a kunya (ʾAbū, ʾAbī, ʾUmm)
  is_kunya <- function(word) {
    word_clean <- tolower(gsub("^[ʾʿ]+", "", word))
    word_clean %in% c("abū", "abu", "abī", "abi", "umm")
  }

  # ===== TITLE FORMATTING =====
  # Rules:
  # 1. First word of title: capitalize (al- stays lowercase, capitalize after)
  # 2. K. prefix: "K." then capitalize next word
  # 3. Bracketed references [...]: treat as embedded title, capitalize first word inside
  # 4. Names inside brackets (ʾAbī Ḥātim): capitalize as names
  # 5. Everything else: lowercase
  if (type == "title") {
    # Track if we're inside brackets and if we just saw opening bracket
    in_bracket <- FALSE
    after_bracket_open <- FALSE
    after_k_dot <- FALSE

    formatted <- character(length(words))
    for (i in seq_along(words)) {
      word <- words[i]
      word_lower <- tolower(word)

      # Check if word starts with [ (opening bracket)
      starts_with_bracket <- grepl("^\\[", word)
      if (starts_with_bracket) {
        in_bracket <- TRUE
        after_bracket_open <- TRUE
        # Remove the bracket for processing, add back later
        word_no_bracket <- sub("^\\[", "", word)
      } else {
        word_no_bracket <- word
      }

      # Check if word ends with ] (closing bracket)
      ends_with_bracket <- grepl("\\]$", word_no_bracket)
      if (ends_with_bracket) {
        word_no_bracket <- sub("\\]$", "", word_no_bracket)
      }

      word_lower_nb <- tolower(word_no_bracket)

      # Determine if this word should be capitalized
      should_capitalize <- FALSE

      # First word of main title
      if (i == 1) {
        should_capitalize <- TRUE
      }

      # Word after standalone K.
      if (i == 2 && tolower(words[1]) == "k.") {
        should_capitalize <- TRUE
      }

      # First word after opening bracket (start of referenced title)
      if (after_bracket_open) {
        should_capitalize <- TRUE
        after_bracket_open <- FALSE
      }

      # Word after K. inside brackets
      if (after_k_dot) {
        should_capitalize <- TRUE
        after_k_dot <- FALSE
      }

      # Check if this is K. (sets flag for next word)
      if (word_lower_nb == "k." || word_lower_nb == "k") {
        after_k_dot <- TRUE
      }

      # Check for names (kunyas) inside brackets - should be capitalized
      is_name_in_bracket <- in_bracket && is_kunya(word_no_bracket)

      # Format the word
      if (word_lower_nb == "k." || word_lower_nb == "k") {
        # K. always uppercase
        result <- "K."
      } else if (grepl("^k\\.", word_no_bracket, ignore.case = TRUE)) {
        # K.something - split and handle
        remainder <- sub("^k\\.", "", word_no_bracket, ignore.case = TRUE)
        if (grepl("^al-", remainder, ignore.case = TRUE)) {
          after_al <- sub("^al-", "", remainder, ignore.case = TRUE)
          result <- paste0("K. al-", capitalize_first_real(after_al))
        } else {
          result <- paste0("K. ", capitalize_first_real(remainder))
        }
      } else if (should_capitalize || is_name_in_bracket) {
        # Capitalize this word
        if (grepl("^al-", word_no_bracket, ignore.case = TRUE)) {
          after_al <- sub("^al-", "", word_no_bracket, ignore.case = TRUE)
          result <- paste0("al-", capitalize_first_real(after_al))
        } else {
          result <- capitalize_first_real(word_no_bracket)
        }
      } else {
        # Default: lowercase
        result <- tolower(word_no_bracket)
      }

      # Add brackets back
      if (starts_with_bracket) {
        result <- paste0("[", result)
      }
      if (ends_with_bracket) {
        result <- paste0(result, "]")
        in_bracket <- FALSE
      }

      formatted[i] <- result
    }
    return(paste(formatted, collapse = " "))
  }

  # ===== AUTHOR FORMATTING =====
  particles <- c("fī", "fi", "min", "ʿan", "ʿalā", "ʿala", "ilā", "ila", "maʿa", "bi", "wa", "li")

  # Track if previous word was a kunya (to capitalize following name)
  prev_was_kunya <- FALSE

  formatted <- character(length(words))
  for (i in seq_along(words)) {
    word <- words[i]
    word_lower <- tolower(word)

    # === FIRST WORD RULES ===
    if (i == 1) {
      # B./b. at start of name expands to "Ibn" (not abbreviated)
      if (word_lower %in% c("b.", "b")) {
        formatted[i] <- "Ibn"
        prev_was_kunya <- FALSE
        next
      }

      # Ibn/Bint at start of name - capitalize
      if (word_lower == "ibn") {
        formatted[i] <- "Ibn"
        prev_was_kunya <- FALSE
        next
      }
      if (word_lower == "bint") {
        formatted[i] <- "Bint"
        prev_was_kunya <- FALSE
        next
      }

      # al- prefix: ALWAYS lowercase, even at start of name
      if (grepl("^al-", word, ignore.case = TRUE)) {
        after_al <- sub("^al-", "", word, ignore.case = TRUE)
        formatted[i] <- paste0("al-", capitalize_first_real(after_al))
        prev_was_kunya <- FALSE
        next
      }

      # Check if it's a kunya
      if (is_kunya(word)) {
        formatted[i] <- capitalize_first_real(word)
        prev_was_kunya <- TRUE
        next
      }

      # Default: capitalize first real letter
      formatted[i] <- capitalize_first_real(word)
      prev_was_kunya <- FALSE
      next
    }

    # === MID-NAME RULES ===

    # Ibn mid-name becomes "b." (standard abbreviation)
    if (word_lower == "ibn") {
      formatted[i] <- "b."
      prev_was_kunya <- FALSE
      next
    }

    # Bint mid-name becomes "bt." (daughter of)
    if (word_lower == "bint") {
      formatted[i] <- "bt."
      prev_was_kunya <- FALSE
      next
    }

    # b./bt. stays lowercase
    if (word_lower %in% c("b.", "b", "bt.", "bt")) {
      formatted[i] <- paste0(gsub("\\.$", "", word_lower), ".")
      prev_was_kunya <- FALSE
      next
    }

    # Particles stay lowercase
    if (word_lower %in% particles) {
      formatted[i] <- word_lower
      prev_was_kunya <- FALSE
      next
    }

    # al- prefix: keep al- lowercase, capitalize word after it
    if (grepl("^al-", word, ignore.case = TRUE)) {
      after_al <- sub("^al-", "", word, ignore.case = TRUE)
      formatted[i] <- paste0("al-", capitalize_first_real(after_al))
      prev_was_kunya <- FALSE
      next
    }

    # Check if this word is a kunya
    if (is_kunya(word)) {
      formatted[i] <- capitalize_first_real(word)
      prev_was_kunya <- TRUE
      next
    }

    # Default: capitalize first real letter (names, kunyas, nisbahs)
    # This handles ʾabū -> ʾAbū, ʿabd -> ʿAbd, etc.
    formatted[i] <- capitalize_first_real(word)
    prev_was_kunya <- FALSE
  }

  paste(formatted, collapse = " ")
}

# Helper: Parse semicolon-delimited titles - return canonical (first) element
parse_first_title <- function(title_str) {
  if (is.na(title_str) || title_str == "") return(title_str)
  trimws(strsplit(as.character(title_str), ";")[[1]][1])
}

# Helper: Extract canonical author name from slash-variant format
# Input: "al-ʿāṣī/al-ʿāṣ b. ḫalaf"
# Output: "al-ʿāṣī b. ḫalaf" (takes first variant at each position)
extract_canonical_author <- function(author_str) {
  if (is.na(author_str) || author_str == "") return(author_str)
  # Replace each "word/variant" pattern with just "word"
  gsub("([^/\\s]+)/[^\\s]+", "\\1", as.character(author_str))
}

# Helper: Extract all variants from author name
# Input: "al-ʿāṣī/al-ʿāṣ b. ḫalaf"
# Output: list(canonical = "al-ʿāṣī b. ḫalaf", variants = c("al-ʿāṣ b. ḫalaf"))
parse_author_variants <- function(author_str) {
  if (is.na(author_str) || author_str == "") {
    return(list(canonical = author_str, variants = character(0)))
  }

  # Find all slash positions
  words <- strsplit(as.character(author_str), "\\s+")[[1]]
  has_slash <- grepl("/", words)

  if (!any(has_slash)) {
    return(list(canonical = author_str, variants = character(0)))
  }

  # Extract canonical (first) form
  canonical_words <- sapply(words, function(w) {
    if (grepl("/", w)) strsplit(w, "/")[[1]][1] else w
  })
  canonical <- paste(canonical_words, collapse = " ")

  # Extract variant forms (second form at each slash position)
  variant_words <- sapply(words, function(w) {
    if (grepl("/", w)) strsplit(w, "/")[[1]][2] else w
  })
  variant <- paste(variant_words, collapse = " ")

  list(canonical = canonical, variants = variant)
}

# Helper: Create HTML badge with color coding
create_color_badge <- function(value, type = "system") {
  if (is.na(value) || value == "") return("")

  colors <- switch(type,
    "system" = c(
      "7"   = "#E69F00",   # Orange
      "7+1" = "#009E73",   # Green
      "10+" = "#0072B2"    # Blue
    ),
    "region" = c(
      "maġrib" = "#56B4E9",
      "mašriq" = "#E69F00",
      "maghrib" = "#56B4E9",
      "mashriq" = "#E69F00",
      "inter-regional" = "#CC79A7",
      "Maġrib" = "#56B4E9",
      "Mašriq" = "#E69F00",
      "Inter-regional" = "#CC79A7"
    ),
    "type" = c(
      "commentary_compression" = "#9467bd",
      "commentary_expansion" = "#8c564b",
      "descriptive catalogue" = "#e377c2",
      "didactic poem" = "#7f7f7f",
      "mufradah" = "#bcbd22",
      "adaa" = "#17becf",
      "ʾadāʾ" = "#17becf"
    ),
    "extant" = c(
      "Extant" = "#009E73",
      "Lost" = "#999999",
      "Unknown" = "#CCCCCC"
    )
  )

  bg_color <- colors[value]
  if (is.na(bg_color)) bg_color <- "#999999"

  # Determine text color based on background brightness
  rgb_vals <- col2rgb(bg_color)
  brightness <- (rgb_vals[1] * 299 + rgb_vals[2] * 587 + rgb_vals[3] * 114) / 1000
  text_color <- if (brightness < 128) "white" else "black"

  sprintf(
    '<span style="background-color: %s; color: %s; padding: 2px 8px; border-radius: 4px; font-size: 0.85em; display: inline-block; margin: 1px;">%s</span>',
    bg_color, text_color, value
  )
}

# Helper: Elastic search - normalize text for flexible matching
# Supports: digraphs (dh→ḏ), dediacritics (ḍ→d), Arabic script, b.→ibn
normalize_for_search <- function(text) {
  if (is.na(text) || text == "") return("")
  text <- tolower(text)

  # Normalize initial "b." to "ibn" for author searches
  text <- gsub("(^|\\s)b\\.\\s*", "\\1ibn ", text)

  # Normalize apostrophe variations (all become empty for matching)
  text <- gsub("['`'ʿʾʼ]", "", text)

  # Digraph mappings (both directions for flexibility)
  # User might type "dh" to find "ḏ" or vice versa
  text <- gsub("ḏ", "dh", text)
  text <- gsub("ḍ", "d", text)
  text <- gsub("ġ", "gh", text)
  text <- gsub("ḫ", "kh", text)
  text <- gsub("š", "sh", text)
  text <- gsub("ṯ", "th", text)
  text <- gsub("ǧ", "j", text)

  # Dediacriticize emphatic/velarized consonants
  text <- gsub("ṣ", "s", text)
  text <- gsub("ṭ", "t", text)
  text <- gsub("ẓ", "z", text)
  text <- gsub("ḥ", "h", text)

  # Dediacriticize long vowels
  text <- gsub("ā", "a", text)
  text <- gsub("ī", "i", text)
  text <- gsub("ū", "u", text)

  # Remove common prefixes for more flexible matching (optional word boundaries)
  # This helps match "muhadhdhab" with "al-muhadhdhab" or "k. al-muhadhdhab"
  text <- gsub("^k\\.?\\s*", "", text)
  text <- gsub("^kitab\\s+", "", text)

  text
}

# Helper: Normalize Arabic text for flexible matching
normalize_arabic_for_search <- function(text) {
  if (is.na(text) || text == "") return("")

  # Remove Arabic diacritics (tashkeel): fatha, damma, kasra, shadda, sukun, etc.
  text <- gsub("[\u064B-\u065F\u0670]", "", text)

  # Normalize alif variations
  text <- gsub("[\u0622\u0623\u0625\u0627]", "\u0627", text)  # All alifs → plain alif

  # Normalize taa marbuta to haa

  text <- gsub("\u0629", "\u0647", text)

  # Normalize yaa variations
  text <- gsub("\u0649", "\u064A", text)  # alif maqsura → yaa

  text
}

# Helper: Elastic search match
# Returns TRUE if query matches target using elastic normalization
elastic_match <- function(query, target_latin, target_arabic = NULL) {
  if (is.na(query) || query == "") return(TRUE)  # Empty query matches all

  query_trimmed <- trimws(query)

  # Check if query is Arabic (contains Arabic Unicode range)
  is_arabic_query <- grepl("[\u0600-\u06FF]", query_trimmed)

  if (is_arabic_query) {
    # Arabic query: match against Arabic field with normalization
    if (!is.null(target_arabic) && !is.na(target_arabic) && target_arabic != "") {
      query_norm <- normalize_arabic_for_search(query_trimmed)
      target_norm <- normalize_arabic_for_search(target_arabic)
      return(grepl(query_norm, target_norm, fixed = TRUE))
    }
    return(FALSE)
  }

  # Latin query: try exact match first (case-insensitive)
  query_lower <- tolower(query_trimmed)
  target_lower <- tolower(target_latin)
  if (grepl(query_lower, target_lower, fixed = TRUE)) {
    return(TRUE)
  }

  # Try normalized/elastic match
  query_norm <- normalize_for_search(query_lower)
  target_norm <- normalize_for_search(target_lower)

  if (nchar(query_norm) > 0 && grepl(query_norm, target_norm, fixed = TRUE)) {
    return(TRUE)
  }

  FALSE
}

# Vectorized elastic match for data frames
elastic_match_vec <- function(query, target_latin_vec, target_arabic_vec = NULL) {
  if (is.na(query) || query == "") return(rep(TRUE, length(target_latin_vec)))

  sapply(seq_along(target_latin_vec), function(i) {
    arabic <- if (!is.null(target_arabic_vec)) target_arabic_vec[i] else NULL
    elastic_match(query, target_latin_vec[i], arabic)
  })
}

# GEOGRAPHIC EXPLORER HELPER FUNCTIONS - Removed (see deploy/geographic_alpha.R)
if (FALSE) {
load_scholar_routes_from_db <- function(db_path = DB_PATH) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    # Get author itineraries with resolved coordinates
    query <- "
      SELECT
        author_id, author_name, death_century, regionality,
        resolved_place, latitude, longitude, sequence_order
      FROM author_itineraries
      ORDER BY author_id, sequence_order
    "
    df <- dbGetQuery(con, query)
    if (nrow(df) == 0) return(NULL)

    # Group by author into route list
    scholar_list <- list()
    for (aid in unique(df$author_id)) {
      author_df <- df[df$author_id == aid, ]
      if (nrow(author_df) < 2) next  # Need at least 2 places for a route

      author_name <- author_df$author_name[1]
      scholar_list[[author_name]] <- list(
        author = author_name,
        regionality = author_df$regionality[1] %||% "unknown",
        death_century = as.numeric(author_df$death_century[1]) %||% NA,
        places_sequence = author_df$resolved_place,
        coords = data.frame(
          place = author_df$resolved_place,
          lat = author_df$latitude,
          lon = author_df$longitude
        ),
        total_distance_km = 0,
        uses_sea_routes = FALSE
      )
    }
    scholar_list
  }, error = function(e) {
    message("Scholar routes from DB error: ", e$message)
    NULL
  })
}

# Load scholar routes - prefer JSON (has path waypoints for route visualization)
load_scholar_routes <- function(json_path = SCHOLAR_ROUTES_PATH, db_path = DB_PATH) {
  # Try JSON first (has detailed path waypoints from Dijkstra algorithm)
  if (file.exists(json_path)) {
    tryCatch({
      routes_json <- fromJSON(json_path, simplifyVector = FALSE)

      scholar_list <- lapply(names(routes_json), function(name) {
        scholar <- routes_json[[name]]
        list(
          author = scholar$author,
          regionality = scholar$regionality %||% "unknown",
          death_century = as.numeric(scholar$death_century) %||% NA,
          places_sequence = scholar$places_sequence %||% character(0),
          routes = scholar$routes %||% list(),  # Include detailed routes with path waypoints
          total_distance_km = scholar$total_distance_km %||% 0,
          uses_sea_routes = scholar$uses_sea_routes %||% FALSE
        )
      })
      names(scholar_list) <- names(routes_json)
      message("Loaded ", length(scholar_list), " scholar routes from JSON with path data")
      return(scholar_list)
    }, error = function(e) {
      message("JSON loading error: ", e$message, " - falling back to database")
    })
  }

  # Fall back to database (no path waypoints, only places_sequence)
  routes <- load_scholar_routes_from_db(db_path)
  if (!is.null(routes) && length(routes) > 0) {
    message("Loaded ", length(routes), " scholar routes from database (no path waypoints)")
    return(routes)
  }

  message("No routes data available")
  NULL
}

# Load GIS-generated sea route geometries (coastal-following paths)
load_gis_sea_routes <- function(json_path = SEA_ROUTES_GIS_PATH) {
  tryCatch({
    if (!file.exists(json_path)) {
      message("GIS sea routes file not found: ", json_path)
      return(list())
    }

    data <- fromJSON(json_path, simplifyVector = FALSE)
    routes <- list()

    for (feature in data$features) {
      props <- feature$properties
      geom <- feature$geometry
      coords <- geom$coordinates

      # Create lookup key from port names
      route_key <- paste0(tolower(props$from), "_", tolower(props$to))
      reverse_key <- paste0(tolower(props$to), "_", tolower(props$from))

      # Store coordinates as matrix
      coord_matrix <- do.call(rbind, lapply(coords, function(c) c(c[[1]], c[[2]])))

      routes[[route_key]] <- list(
        from = props$from,
        to = props$to,
        coordinates = coord_matrix,
        distance_km = props$distance_km
      )

      # Also store reverse direction
      routes[[reverse_key]] <- list(
        from = props$to,
        to = props$from,
        coordinates = coord_matrix[nrow(coord_matrix):1, ],  # Reverse order
        distance_km = props$distance_km
      )
    }

    message("Loaded ", length(data$features), " GIS sea routes with coastal geometries")
    return(routes)
  }, error = function(e) {
    message("Error loading GIS sea routes: ", e$message)
    return(list())
  })
}

# Extract coordinate portion from Thurayya code (e.g., "004W394N" from "BALANSIYYA_004W394N_S")
extract_coords_from_code <- function(code) {
  # Pattern: NAME_XXXEYYYN_S or NAME_XXXWYYYN_S (X=lon digits, Y=lat digits)
  match <- regmatches(code, regexpr("[0-9]{3}[EW][0-9]{3}N", code))
  if (length(match) > 0) return(match) else return(NULL)
}

# Load sea route geometries with multiple lookup keys for flexible path segment matching
# Uses coordinate-based matching as primary method (more robust than name matching)
load_sea_route_geometries <- function(json_path = SEA_ROUTE_GEOMETRIES_PATH) {
  tryCatch({
    if (!file.exists(json_path)) {
      message("Sea route geometries file not found: ", json_path)
      return(list())
    }

    data <- fromJSON(json_path, simplifyVector = FALSE)
    routes <- list()

    for (key in names(data)) {
      route <- data[[key]]
      coords <- route$coordinates

      # Store coordinates as matrix
      if (!is.null(coords) && length(coords) > 0) {
        coord_matrix <- do.call(rbind, lapply(coords, function(c) {
          if (is.list(c)) c(c[[1]], c[[2]]) else c
        }))

        from_code <- route$from_code
        to_code <- route$to_code

        # Extract coordinate portions for flexible matching
        from_coords <- extract_coords_from_code(from_code)
        to_coords <- extract_coords_from_code(to_code)

        # Normalize codes (strip suffix)
        from_base <- sub("_[SRW]$", "", from_code)
        to_base <- sub("_[SRW]$", "", to_code)

        # Store route data
        route_data <- list(
          from_code = from_code,
          to_code = to_code,
          from_name = route$from_name,
          to_name = route$to_name,
          coordinates = coord_matrix,
          distance_km = route$distance_km
        )

        # Key by exact codes
        routes[[paste0(from_code, "_TO_", to_code)]] <- route_data

        # Key by base codes (without suffix)
        routes[[paste0(from_base, "_TO_", to_base)]] <- route_data

        # Key by coordinates only (most flexible - recommended by user)
        if (!is.null(from_coords) && !is.null(to_coords)) {
          routes[[paste0(from_coords, "_TO_", to_coords)]] <- route_data
        }

        # Key by port names
        routes[[paste0(tolower(route$from_name), "_TO_", tolower(route$to_name))]] <- route_data

        # Reverse direction for all key types
        reverse_data <- route_data
        reverse_data$coordinates <- coord_matrix[nrow(coord_matrix):1, , drop = FALSE]
        routes[[paste0(to_code, "_TO_", from_code)]] <- reverse_data
        routes[[paste0(to_base, "_TO_", from_base)]] <- reverse_data
        if (!is.null(from_coords) && !is.null(to_coords)) {
          routes[[paste0(to_coords, "_TO_", from_coords)]] <- reverse_data
        }
        routes[[paste0(tolower(route$to_name), "_TO_", tolower(route$from_name))]] <- reverse_data
      }
    }

    message("Loaded ", length(data), " sea route geometries with coordinate-based lookup")
    return(routes)
  }, error = function(e) {
    message("Error loading GIS sea routes: ", e$message)
    return(list())
  })
}

# Load Mediterranean port locations for sea routes visualization
load_mediterranean_ports <- function(json_path = MEDITERRANEAN_PORTS_PATH) {
  tryCatch({
    if (!file.exists(json_path)) {
      message("Mediterranean ports file not found: ", json_path)
      return(NULL)
    }

    data <- fromJSON(json_path, simplifyVector = FALSE)
    ports_df <- do.call(rbind, lapply(data$features, function(f) {
      data.frame(
        name = f$properties$name,
        thurayya_code = f$properties$thurayya_code,
        lon = f$geometry$coordinates[[1]],
        lat = f$geometry$coordinates[[2]],
        stringsAsFactors = FALSE
      )
    }))

    message("Loaded ", nrow(ports_df), " Mediterranean ports")
    return(ports_df)
  }, error = function(e) {
    message("Error loading Mediterranean ports: ", e$message)
    return(NULL)
  })
}

# Load city coordinates from database (uses normalized places table)
load_city_coordinates <- function(db_path = DB_PATH) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    query <- "
      SELECT
        place_name_latin as city,
        latitude as lat,
        longitude as lon,
        region
      FROM places
      WHERE latitude IS NOT NULL
    "
    dbGetQuery(con, query)
  }, error = function(e) {
    message("City coordinates loading error: ", e$message)
    NULL
  })
}

# Parse coordinates from Thurayya code
# e.g., "BAGHDAD_443E333N_S" → list(lon=44.3, lat=33.3)
parse_thurayya_coords <- function(code) {
  if (is.na(code) || code == "" || is.null(code)) return(NULL)

  # Extract coordinate portion: "443E333N" from "NAME_443E333N_S"
  match <- regmatches(code, regexec("_(\\d{3})([EW])(\\d{3})([NS])_", code))[[1]]
  if (length(match) < 5) return(NULL)

  lon <- as.numeric(match[2]) / 10
  if (match[3] == "W") lon <- -lon

  lat <- as.numeric(match[4]) / 10
  if (match[5] == "S") lat <- -lat

  list(lon = lon, lat = lat)
}

# Extract city pairs from scholar journeys
extract_city_pairs <- function(scholar_routes) {
  pairs <- list()

  for (scholar_name in names(scholar_routes)) {
    scholar <- scholar_routes[[scholar_name]]
    places <- unlist(scholar$places_sequence)

    if (length(places) < 2) next

    for (i in 1:(length(places) - 1)) {
      city1 <- as.character(places[i])
      city2 <- as.character(places[i + 1])
      pair <- sort(c(city1, city2))
      key <- paste(pair, collapse = "|")

      if (is.null(pairs[[key]])) {
        pairs[[key]] <- list(
          from = pair[1],
          to = pair[2],
          scholars = character(0),
          centuries = numeric(0),
          regionalities = character(0)
        )
      }

      pairs[[key]]$scholars <- c(pairs[[key]]$scholars, scholar$author)
      pairs[[key]]$centuries <- c(pairs[[key]]$centuries, scholar$death_century)
      pairs[[key]]$regionalities <- c(pairs[[key]]$regionalities, scholar$regionality)
    }
  }

  # Convert to dataframe
  if (length(pairs) == 0) return(NULL)

  pairs_df <- do.call(rbind, lapply(names(pairs), function(key) {
    p <- pairs[[key]]
    has_cross_regional <- any(grepl("visits", p$regionalities, ignore.case = TRUE))
    edge_regionality <- if (has_cross_regional) {
      "cross-regional"
    } else {
      names(sort(table(p$regionalities), decreasing = TRUE)[1])
    }

    data.frame(
      from = p$from,
      to = p$to,
      count = length(p$scholars),
      scholars = paste(p$scholars, collapse = "; "),
      primary_century = as.numeric(names(sort(table(p$centuries), decreasing = TRUE)[1])),
      primary_regionality = edge_regionality,
      stringsAsFactors = FALSE
    )
  }))

  pairs_df %>% arrange(desc(count))
}

# Count city visit frequencies
count_city_visits <- function(scholar_routes) {
  city_data <- list()

  for (scholar_name in names(scholar_routes)) {
    scholar <- scholar_routes[[scholar_name]]
    places <- unlist(scholar$places_sequence)

    for (city in places) {
      if (is.null(city_data[[city]])) {
        city_data[[city]] <- list(
          visits = 0,
          scholars = character(0),
          centuries = numeric(0),
          regionalities = character(0)
        )
      }

      city_data[[city]]$visits <- city_data[[city]]$visits + 1
      city_data[[city]]$scholars <- c(city_data[[city]]$scholars, scholar$author)
      city_data[[city]]$centuries <- c(city_data[[city]]$centuries, scholar$death_century)
      city_data[[city]]$regionalities <- c(city_data[[city]]$regionalities, scholar$regionality)
    }
  }

  if (length(city_data) == 0) return(NULL)

  cities_df <- do.call(rbind, lapply(names(city_data), function(city) {
    c <- city_data[[city]]
    data.frame(
      city = city,
      visits = c$visits,
      scholars = paste(unique(c$scholars), collapse = "; "),
      n_scholars = length(unique(c$scholars)),
      primary_century = as.numeric(names(sort(table(c$centuries), decreasing = TRUE)[1])),
      primary_regionality = names(sort(table(c$regionalities), decreasing = TRUE)[1]),
      stringsAsFactors = FALSE
    )
  }))

  cities_df %>% arrange(desc(visits))
}

# Filter scholar routes by criteria
filter_scholar_routes <- function(scholar_routes, century_range, regionality) {
  filtered <- scholar_routes

  # Filter by century
  filtered <- filtered[sapply(filtered, function(s) {
    cent <- s$death_century
    !is.na(cent) && cent >= century_range[1] && cent <= century_range[2]
  })]

  # Filter by regionality
  if (regionality != "all") {
    filtered <- filtered[sapply(filtered, function(s) {
      reg <- s$regionality
      if (regionality == "mashriq") {
        reg %in% c("mašriq", "mašriq visits maġrib")
      } else if (regionality == "maghrib") {
        reg %in% c("maġrib", "maġrib visits mašriq")
      } else if (regionality == "cross") {
        grepl("visits", reg)
      } else {
        TRUE
      }
    })]
  }

  filtered
}

# Get edge color based on regionality
get_edge_color <- function(regionality) {
  if (grepl("visits", regionality, ignore.case = TRUE)) {
    return("#CC79A7")  # Cross-regional (reddish purple)
  } else if (regionality == "maġrib") {
    return("#56B4E9")  # Maghrib (sky blue)
  } else if (regionality == "mašriq") {
    return("#E69F00")  # Mashriq (orange)
  }
  "#666666"
}

# Map Thurayya regions to Mašriq/Maġrib
get_region_category <- function(thurayya_region) {
  maghrib_regions <- c(
    "al-Andalus (Spain)", "al-Maġrib", "Miṣr (Egypt)", "Barqaŧ (Lybia)"
  )
  if (thurayya_region %in% maghrib_regions) {
    return("Maġrib")
  }
  return("Mašriq")
}

# Subregion rectangles (matching GIF animations)
get_subregion_rects <- function() {
  data.frame(
    subregion = c("al-ʾandalus", "egypt", "ʾifrīqiyyah", "al-šām", "al-ʿirāq",
                  "ǧibāl-ṭabaristān", "fārs", "ḫurāsān", "ḥiǧāz"),
    xmin = c(-9.5, 25.0, -8.0, 34.0, 42.0, 48.0, 47.0, 57.0, 36.5),
    xmax = c(-2.0, 35.0, 13.0, 42.0, 48.0, 54.0, 57.0, 70.0, 43.0),
    ymin = c(36.0, 22.0, 30.0, 29.5, 29.0, 32.0, 26.0, 32.0, 20.0),
    ymax = c(43.0, 32.0, 37.5, 37.5, 37.5, 42.0, 32.0, 42.0, 29.0),
    center_lon = c(-4.0, 31.2, 3.0, 36.3, 44.4, 51.0, 52.0, 60.0, 39.8),
    center_lat = c(37.9, 30.0, 35.5, 33.5, 33.3, 36.5, 29.6, 36.0, 24.5),
    meta_region = c("maġrib", "maġrib", "maġrib", "mašriq", "mašriq",
                    "mašriq", "mašriq", "mašriq", "mašriq"),
    color = c("#56B4E9", "#56B4E9", "#56B4E9", "#E69F00", "#E69F00",
              "#E69F00", "#E69F00", "#E69F00", "#E69F00"),
    stringsAsFactors = FALSE
  )
}

# Mashriq-Maghrib dividing line coordinates (through Suez Canal)
get_dividing_line <- function() {
  data.frame(
    lon = c(32.5, 32.5, 30.0),
    lat = c(20.0, 31.5, 45.0)
  )
}

# Get all Thurayya regions grouped by Mašriq/Maġrib (kept for compatibility)
get_thurayya_region_groups <- function() {
  list(
    "Maġrib" = c(
      "al-Andalus (Spain)", "al-Maġrib", "Miṣr (Egypt)", "Barqaŧ (Lybia)"
    ),
    "Mašriq" = c(
      "al-Šām (Greater Syria)", "al-ʿIrāq", "Aqūr (al-Jazīraŧ)",
      "al-Jibāl", "Ḫurāsān", "Fārs(or Fāris)", "Kirmān",
      "Mā-warāʾ-l-nahr (Transoxiana)", "al-Daylam", "Jazīraŧ al-ʿarab",
      "Sijistān (Sīstān)", "al-Sind", "Ḫūzistān (al-Ahwāz)"
    )
  )
}

# Create rectangular subregion polygons for leaflet
create_subregion_polygons <- function() {
  rects <- get_subregion_rects()

  # Create SF polygons from rectangles
  polygons_list <- lapply(1:nrow(rects), function(i) {
    coords <- matrix(c(
      rects$xmin[i], rects$ymin[i],
      rects$xmax[i], rects$ymin[i],
      rects$xmax[i], rects$ymax[i],
      rects$xmin[i], rects$ymax[i],
      rects$xmin[i], rects$ymin[i]  # Close the polygon
    ), ncol = 2, byrow = TRUE)

    st_sf(
      subregion = rects$subregion[i],
      meta_region = rects$meta_region[i],
      color = rects$color[i],
      center_lon = rects$center_lon[i],
      center_lat = rects$center_lat[i],
      geometry = st_sfc(st_polygon(list(coords)), crs = 4326)
    )
  })

  do.call(rbind, polygons_list)
}

# Legacy function kept for compatibility
create_region_polygons <- function(city_coords) {
  # Now just calls the new subregion function
  create_subregion_polygons()
}

# Combined geographic data loader
load_geographic_data <- function() {
  list(
    scholar_routes = load_scholar_routes(),
    city_coords = load_city_coordinates(),
    gis_sea_routes = load_gis_sea_routes(),
    sea_route_geometries = load_sea_route_geometries(),  # Thurayya code keyed for path matching
    mediterranean_ports = load_mediterranean_ports()
  )
}
} # end if (FALSE) - Geographic Explorer helpers

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
        w.system,
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
        AND w.system IS NOT NULL
        AND w.system != ''
        AND w.system != 'NA'
    "

    df <- dbGetQuery(con, query)

    # Also get works that are subjects of commentaries (other works reference them)
    commentaries_query <- "
      SELECT
        text_reuse_source as work_id,
        GROUP_CONCAT(w.work_id, '|') as commentary_ids,
        GROUP_CONCAT(w.title, '|') as commentary_titles,
        GROUP_CONCAT(w.type, '|') as commentary_types,
        GROUP_CONCAT(COALESCE(a.author_name_canonical, a.author_name), '|') as commentary_authors
      FROM works w
      LEFT JOIN authors a ON w.author_id = a.author_id
      WHERE w.text_reuse = 1 AND w.text_reuse_source IS NOT NULL
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

    # Clean up system values (e.g., "7.0" -> "7", keep "7+1" and "10+" as-is)
    df$system <- sapply(df$system, function(x) {
      if (x == "7.0") "7" else x
    })
    # Parse semicolon-delimited titles to show canonical (first) element only
    # Note: author_name already uses canonical form from database
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

  if (geo_col == "regionality" || any(grepl(pattern = "mašriq|maġrib", x = geo_chr, ignore.case = TRUE))) {
    # Group by HOME region (first word):
    # "maġrib" or "maġrib visits mašriq" → Maġrib (home region)
    # "mašriq" or "mašriq visits maġrib" → Mašriq (home region)
    geo_chr <- ifelse(test = grepl(pattern = "^maġrib", x = geo_chr, ignore.case = TRUE),
                      yes = "maġrib",
                      no = ifelse(test = grepl(pattern = "^mašriq", x = geo_chr, ignore.case = TRUE),
                                  yes = "mašriq",
                                  no = geo_chr))
    geo_fac <- factor(x = geo_chr, levels = c("maġrib", "mašriq"))
  } else {
    geo_fac <- factor(x = geo_chr)
  }

  # Filter valid indices (exclude NA, empty strings, and "NA" string values)
  valid_idx <- !is.na(outcome_chr) & !is.na(geo_chr) &
               outcome_chr != "" & geo_chr != "" &
               outcome_chr != "NA" & geo_chr != "NA"

  if (!is.null(century_col) && century_col %in% names(df)) {
    century_raw <- df[[century_col]][valid_idx]
    century_numeric <- as.numeric(century_raw)
    century_valid <- !is.na(century_numeric) & century_numeric >= 4 & century_numeric <= 9

    # Create factor from filtered data only (excludes NA levels)
    outcome_filtered <- outcome_chr[valid_idx][century_valid]
    geo_filtered <- geo_fac[valid_idx][century_valid]

    result <- data.frame(
      outcome = factor(outcome_filtered, levels = c("7", "7+1", "10+")),  # Correct ordering: 7, 7+1, 10+
      geo = droplevels(geo_filtered),
      century = century_numeric[century_valid],
      stringsAsFactors = FALSE
    )
    return(result)
  }

  # Create factor from filtered data only
  data.frame(
    outcome = factor(outcome_chr[valid_idx], levels = c("7", "7+1", "10+")),  # Correct ordering: 7, 7+1, 10+
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
      outcome_col = "system",
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

# UI
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),

  # Custom CSS
  tags$head(
    tags$style(HTML("
      .app-header {
        background: linear-gradient(135deg, #001158 0%, #0052D6 100%);
        color: white;
        padding: 25px;
        margin-bottom: 20px;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }

      .app-header h2 {
        margin: 0;
        font-weight: 600;
      }

      .app-header p {
        margin: 5px 0 0 0;
        opacity: 0.95;
      }

      .info-box {
        background-color: #e7f3ff;
        border-left: 4px solid #2196F3;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }

      .info-box h5 {
        margin-top: 0;
        color: #1976D2;
      }

      .card {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border-radius: 4px;
        border: 1px solid #e0e0e0;
        margin-bottom: 20px;
        background: white;
      }

      .card-header {
        font-size: 18px;
        font-weight: 600;
        padding: 15px;
        border-radius: 4px 4px 0 0;
        background-color: #f5f5f5;
        border-bottom: 1px solid #e0e0e0;
      }

      .card-body {
        padding: 20px;
      }

      /* Fast hover tooltip for text reuse symbols — fixed positioning to escape overflow */
      .text-reuse-tooltip {
        position: relative;
      }
      .text-reuse-tooltip::after {
        content: attr(data-tooltip);
        position: fixed;
        left: var(--tt-left, 0);
        top: var(--tt-top, 0);
        transform: translateX(-50%) translateY(-100%);
        background-color: #333;
        color: white;
        padding: 14px 18px;
        border-radius: 8px;
        font-size: 14px;
        white-space: pre-line;
        max-width: 550px;
        min-width: 300px;
        z-index: 99999;
        opacity: 0;
        visibility: hidden;
        transition: opacity 0.1s ease-in-out;
        pointer-events: none;
        box-shadow: 0 4px 12px rgba(0,0,0,0.4);
        text-align: left;
        line-height: 1.5;
      }
      .text-reuse-tooltip::before {
        content: '';
        position: fixed;
        left: var(--tt-left, 0);
        top: var(--tt-top, 0);
        transform: translateX(-50%) translateY(-100%);
        margin-top: -2px;
        border: 6px solid transparent;
        border-top-color: #333;
        z-index: 99999;
        opacity: 0;
        visibility: hidden;
        transition: opacity 0.1s ease-in-out;
      }
      .text-reuse-tooltip:hover::after,
      .text-reuse-tooltip:hover::before {
        opacity: 1;
        visibility: visible;
      }

      /* Citation link styling */
      .citation-link { color: inherit; text-decoration: none; }
      .citation-link:hover { text-decoration: underline; color: #007bff; cursor: pointer; }

      /* Copy buttons in citation modals */
      .copy-btn {
        background: none; border: 1px solid #ccc; border-radius: 4px;
        padding: 1px 6px; cursor: pointer; font-size: 0.8em; color: #666;
        margin-left: 6px; vertical-align: middle; transition: all 0.2s;
      }
      .copy-btn:hover { background: #e9ecef; border-color: #999; color: #333; }
      .copy-section-btn {
        background: none; border: 1px solid #ccc; border-radius: 4px;
        padding: 2px 10px; cursor: pointer; font-size: 0.75em; color: #666;
        margin-left: 10px; vertical-align: middle; transition: all 0.2s;
      }
      .copy-section-btn:hover { background: #e9ecef; border-color: #999; color: #333; }
      /* citation-indicator removed — click on title/author name to view citations */

      .status-badge {
        display: inline-block;
        padding: 8px 12px;
        border-radius: 4px;
        font-size: 14px;
        margin: 5px;
      }

      .status-success {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }

      .results-card {
        background-color: #f8f9fa;
        border: 2px solid #17a2b8;
        padding: 20px;
        margin: 15px 0;
        border-radius: 8px;
      }

      .results-card h4 {
        color: #17a2b8;
        margin-top: 0;
      }

      .interpretation-text {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 15px 0;
      }

      .nav-pills .nav-link.active {
        background-color: #001158 !important;
      }

      .collapsible-section {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        margin-top: 15px;
        padding: 15px;
      }

      .section-divider {
        border-top: 2px solid #e0e0e0;
        margin: 30px 0;
      }

      /* Card Navigation for Bayesian Analysis */
      .card-navigation {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 20px;
        margin-bottom: 20px;
        padding: 15px;
        background: #f8f9fa;
        border-radius: 8px;
      }

      .card-indicator {
        font-size: 16px;
        font-weight: 500;
        color: #001158;
      }

      /* Home page styles */
      .home-section {
        margin-bottom: 30px;
      }

      .home-section h3 {
        font-size: 1.4em;
        font-weight: 600;
        color: #333;
        margin-bottom: 15px;
        border-bottom: 2px solid #0072B2;
        padding-bottom: 8px;
      }

      .home-blurb {
        font-size: 1.1em;
        line-height: 1.8;
        color: #333;
      }

      .home-blurb a {
        color: #0072B2;
        text-decoration: none;
        font-weight: 500;
      }

      .home-blurb a:hover {
        text-decoration: underline;
      }

      .home-animation {
        text-align: center;
        margin: 20px 0;
      }

      .home-animation img {
        max-width: 100%;
      }

      /* Geographic Explorer styles removed (see geographic_alpha.R) */

      /* Bold Section Headers */
      .section-header-bold {
        font-size: 20px;
        font-weight: 700;
        color: #333;
        border-bottom: 3px solid #001158;
        padding-bottom: 10px;
        margin-bottom: 20px;
      }

      /* Corpus search */
      .corpus-search-results {
        font-size: 14px;
        color: #666;
        margin-bottom: 15px;
      }

      /* Interactive plot container */
      .plotly-container {
        border: 1px solid #e0e0e0;
        border-radius: 4px;
        padding: 10px;
        background: white;
      }
    ")),
    tags$script(HTML("
      // Position tooltips in viewport coordinates to escape overflow containers
      document.addEventListener('mouseover', function(e) {
        if (e.target.classList.contains('text-reuse-tooltip')) {
          var rect = e.target.getBoundingClientRect();
          var left = rect.left + rect.width / 2;
          var top = rect.top - 10;
          left = Math.max(180, Math.min(left, window.innerWidth - 180));
          e.target.style.setProperty('--tt-left', left + 'px');
          e.target.style.setProperty('--tt-top', top + 'px');
        }
      });

      // Clipboard helper for citation copy buttons
      function copyCitationText(text, btn) {
        navigator.clipboard.writeText(text).then(function() {
          var orig = btn.innerHTML;
          btn.innerHTML = '\\u2713';
          btn.style.color = '#28a745';
          setTimeout(function() { btn.innerHTML = orig; btn.style.color = ''; }, 1200);
        });
      }
    "))
  ),

  # Header
  div(class = "app-header",
      h2("The Mašriqī and Maġribī Pedagogical Canons of Qurʾānic Reading Traditions"),
      p("Exploring the Development of the Classical ", tags$em("qirāʾāt"), " Corpus")
  ),

  # Main content
  div(class = "container-fluid",
    tabsetPanel(
      id = "tabs",
      type = "pills",

      # ========== Tab 1: HOME ==========
      tabPanel(
        title = "Home",
        value = "home",
        br(),

        # Introduction section
        div(class = "home-section",
          h3("Introduction"),
          p(class = "home-blurb",
            "This app accompanies a study of geography and pedagogical canonicity in the Qurʾānic Reading ",
            "Traditions (qirāʾāt), 4th–7th centuries AH. It comprises ",
            textOutput("home_works_count", inline = TRUE),
            " works by authors across the Maġrib and Mašriq."),
          p("Use the tabs above to:",
            tags$ul(
              tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'corpus_explorer', {priority: 'event'});", "Corpus Explorer"),
                " — search, filter, and download the full bibliography"),
              tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'methodology', {priority: 'event'});", "Methodology"),
                " — a complete walk-through of the data, model, and interpretation"),
              tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'bayesian_analysis', {priority: 'event'});", "Bayesian Analysis"),
                " — interactive results of the multinomial regression model")
            )
          )
        ),

        # Animation section
        div(class = "home-section",
          h3("Scholar Mobility Visualization"),
          p("The animated map below shows the geographic distribution and mobility patterns of scholars in the corpus across centuries."),
          imageOutput("home_animation", height = "auto")
        )
      ),

      # ========== Tab 2: Corpus Explorer ==========
      tabPanel(
        title = tagList(icon("book"), "Corpus Explorer"),
        value = "corpus_explorer",
        br(),

        # Status badge
        uiOutput("data_status_badge"),

        p("Showing ", textOutput("corpus_works_count", inline = TRUE),
          " works on Qurʾānic Reading Traditions (4th–7th c. AH). ",
          "For definitions of reading systems, regions, and the statistical model, see the ",
          tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'methodology', {priority: 'event'});", "Methodology"),
          " tab."),
        br(),

        # Data Distribution
        h4(icon("chart-bar"), " Data Distribution"),
        fluidRow(
          column(4, plotlyOutput("data_dist_outcome", height = "280px")),
          column(4, plotlyOutput("data_dist_geo", height = "280px")),
          column(4, plotlyOutput("data_dist_century", height = "280px"))
        ),

        br(),

        # Enhanced Corpus Search
        div(class = "card",
          div(class = "card-header", icon("search"), " Corpus Search"),
          div(class = "card-body",
            # Row 1: Combined text search (title + author)
            fluidRow(
              column(12, textInput("search_all", "Search Title or Author:",
                placeholder = "tadhkirah, Shatibi, تذكرة, الشاطبي, or šāṭibī..."))
            ),
            tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
              "Flexible search: use Arabic script, digraphs (dh, gh, sh, th, kh), or simplified Latin (tadhkira = taḏkira = تذكرة)"
            ),
            # Row 2: Filters (multi-select enabled)
            fluidRow(
              column(3, selectizeInput("filter_system", "Reading Set:",
                choices = c("7", "7+1", "10+"),
                multiple = TRUE,
                options = list(placeholder = "All sets..."))),
              column(3, selectizeInput("filter_region", "Origin:",
                choices = c("Maġrib" = "maġrib", "Mašriq" = "mašriq", "Inter-regional" = "inter-regional"),
                multiple = TRUE,
                options = list(placeholder = "All regions..."))),
              column(3, selectizeInput("filter_type", "Work Type:",
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "All types..."))),
              column(3, sliderInput("filter_century", "Century (AH):", min = 4, max = 7, value = c(4, 7), step = 1))
            ),
            # Row 3: Clear filters only
            fluidRow(
              column(12, actionButton("clear_filters", icon("times"), " Clear Filters", class = "btn-secondary"))
            ),
            hr(),
            uiOutput("corpus_results_count"),
            DTOutput("enhanced_data_table"),
            # Download buttons below results
            br(),
            fluidRow(
              column(4, downloadButton("download_csv", "Download CSV", class = "btn-primary btn-block")),
              column(4, downloadButton("download_json", "Download JSON", class = "btn-info btn-block")),
              column(4, downloadButton("download_ris", "Download RIS (Zotero)", class = "btn-success btn-block"))
            )
          )
        )
      ),

      # ========== Tab 3: Geographic Explorer (REMOVED - see geographic_alpha.R) ==========

      # ========== Tab 4: Methodology ==========
      tabPanel(
        title = tagList(icon("graduation-cap"), "Methodology"),
        value = "methodology",
        br(),
        div(class = "card",
          div(class = "card-header", "How the Bayesian Analysis Works"),
          div(class = "card-body",
            h4("A Complete Guide for the IQSA Qirāʾāt Bibliography App"),
            p(em("QurCan ERC Grant (No. 101054849) — Leiden University Centre for Linguistics")),
            hr(),

            # Overview
            h3("Overview"),
            p("This app uses ", strong("Bayesian multinomial logistic regression"),
              " to estimate the probability that a scholar from a given region wrote about a particular ",
              "Qurʾānic reading system. The model takes as input a bibliography of works on the qirāʾāt, ",
              "each classified by reading system (7, 7+1, or 10+) and by the author's regional affiliation ",
              "(Mašriq or Maġrib), and produces probability estimates with full uncertainty quantification."),
            p("This page walks through every step of the analysis, from raw data to the final probability statements."),
            hr(),

            # 1. The Data
            h3("1. The Data"),
            p("The corpus consists of ", strong("172 bibliographic works"),
              " on Qurʾānic reading traditions composed between the 4th and 7th centuries AH ",
              "(10th–13th centuries CE). Each work is classified by:"),
            tags$ul(
              tags$li(strong("Reading system"), " — which pedagogical canon the work describes:",
                tags$ul(
                  tags$li(strong("7 readings"), ": the system canonized by Ibn Muǧāhid (d. 324/936), encompassing seven readers"),
                  tags$li(strong("7+1 readings"), ": the seven plus Yaʿqūb al-Ḥaḍramī, an eighth reader widely recognized in Baṣra"),
                  tags$li(strong("10+ readings"), ": ten or more reading traditions, such as the systems compiled by Ibn Mihrān (d. 381/991), al-Ahwāzī (d. 446/1054), and other Mašriqī authors before 653/1255")
                )),
              tags$li(strong("Region"), " — the author's primary scholarly affiliation:",
                tags$ul(
                  tags$li(strong("Maġrib"), ": the western Islamic world (al-Andalus, Ifrīqiyyah, Egypt)"),
                  tags$li(strong("Mašriq"), ": the eastern Islamic world (al-Šām, Iraq, Fārs, Ǧibāl-Ṭabaristān, Ḫurāsān)")
                )),
              tags$li(strong("Century"), " — the author's death century (AH), used as a proxy for the period of the work's circulation")
            ),

            h4("Observed Counts"),
            tags$table(class = "table table-bordered table-sm", style = "max-width: 500px;",
              tags$thead(tags$tr(
                tags$th("System"), tags$th("Maġrib"), tags$th("Mašriq"), tags$th("Total")
              )),
              tags$tbody(
                tags$tr(tags$td("7"), tags$td("48"), tags$td("38"), tags$td("86")),
                tags$tr(tags$td("7+1"), tags$td("10"), tags$td("17"), tags$td("27")),
                tags$tr(tags$td("10+"), tags$td("4"), tags$td("55"), tags$td("59")),
                tags$tr(tags$td(strong("Total")), tags$td(strong("62")), tags$td(strong("110")), tags$td(strong("172")))
              )
            ),
            p("Even at a glance, these raw numbers suggest a strong pattern: the 10+ system is overwhelmingly ",
              "Mašriqī (55 of 59 works), while the 7-reading system is more evenly distributed with a Maġribī lean. ",
              "But raw counts do not account for the overall imbalance between regions (110 Mašriqī vs. 62 Maġribī works) ",
              "or quantify our uncertainty. The Bayesian model addresses both problems."),
            hr(),

            # 2. The Question
            h3("2. The Question"),
            p("We want to answer: ", strong("Given a scholar's regional affiliation (and, optionally, century), ",
              "what is the probability that they wrote about each reading system?")),
            p("Formally, we want to estimate P(system = k | region, century) for each system ",
              em("k"), " ∈ {7, 7+1, 10+}, and to know how confident we can be in those estimates. ",
              "The 172 works form a contingency table of system × region × century. Our goal is to estimate the cell probabilities ",
              "of this table while accounting for the fact that some cells are sparse (e.g., only 4 Maġribī works on 10+)."),
            hr(),

            # 3. Why Bayesian Methods?
            h3("3. Why Bayesian Methods?"),
            p("A standard chi-square test could tell us whether system and region are statistically associated ",
              "(they are: ", em("p"), " < 0.001), but it cannot tell us ", em("how much"),
              " more likely a Mašriqī scholar is to write about the 10+ system. Bayesian inference directly computes the ",
              strong("probability distribution"), " over plausible parameter values given the data, enabling statements like ",
              "\"there is a 99.8% probability that P(10+ | Mašriq) > P(10+ | Maġrib)\" and ",
              "\"the most likely value of P(10+ | Mašriq) is 0.48, with a 95% credible interval of [0.39, 0.57].\" ",
              "These are direct answers to the research question — not statements about hypothetical repeated experiments."),
            p("All of Bayesian inference rests on a single formula:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "posterior = (likelihood × prior) / normalizing constant"),
            p("The ", strong("prior"), " encodes what we believe before seeing data; the ", strong("likelihood"),
              " measures how probable the data are for each parameter value; the ", strong("posterior"),
              " is what we believe after combining the two. Every probability statement, credible interval, ",
              "and regional comparison in this app is extracted from the posterior."),
            hr(),

            # 4. The Model
            h3("4. The Model"),
            h4("Why Multinomial Logistic Regression?"),
            p("Our outcome has three unordered categories (7, 7+1, 10+). This rules out binary logistic regression ",
              "(two outcomes only) and ordinal regression (requires a natural ordering). Multinomial logistic regression ",
              "estimates the probability of each category simultaneously, respecting the constraint that ",
              "P(7) + P(7+1) + P(10+) = 1."),

            h4("The Reference Category"),
            p("With three categories summing to one, we only need to model two freely — the third is determined. ",
              "The model uses the ", strong("10+ system"), " as the reference (baseline) and models the other two ",
              "relative to it. This choice is arbitrary and does not affect the final probability estimates."),

            h4("The Linear Predictor"),
            p("For each non-reference category ", em("k"), " ∈ {7, 7+1}, the model computes a ",
              strong("log-odds ratio"), " relative to the 10+ baseline:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "η_k = α_k + β_k × region + γ_k × (century − mean_century)"),
            p("where:"),
            tags$ul(
              tags$li(strong("α_k"), " (intercept) = baseline log-odds of system ", em("k"), " vs. 10+, for a Maġribī scholar in the average century"),
              tags$li(strong("β_k"), " (regional effect) = how the log-odds change when moving from Maġrib to Mašriq"),
              tags$li(strong("γ_k"), " (century effect) = how the log-odds change per century (century model only)")
            ),
            p("For the reference category (10+): η₃ = 0 by definition."),
            p(strong("What are log-odds?"), " The log-odds (or logit) is log(p / (1−p)). It maps probabilities from the bounded range [0, 1] ",
              "onto the entire real line (−∞, +∞), which is necessary because the linear predictor can take any value. ",
              "A log-odds of 0 corresponds to probability 0.5; positive values mean probability > 0.5; negative values mean < 0.5."),

            h4("The Parameters: What the Model Learns"),
            p("The abstract subscript ", em("k"), " maps onto the two non-reference reading systems:"),
            p(strong("For the 7-reading system"), " (k = 1, comparing 7 vs. 10+):"),
            tags$ul(
              tags$li(strong("α₁"), " = baseline log-odds of 7 vs. 10+ for a Maġribī scholar at the average century. ",
                "A positive α₁ means the 7-reading system starts out more probable than 10+ in the Maġrib."),
              tags$li(strong("β₁"), " = regional effect on the 7 vs. 10+ log-odds. ",
                "A negative β₁ means moving from Maġrib to Mašriq makes the 7-reading system ", em("less"), " probable relative to 10+."),
              tags$li(strong("γ₁"), " = century effect on the 7 vs. 10+ log-odds (century model only). ",
                "A negative γ₁ means the 7-reading system becomes less probable relative to 10+ in later centuries.")
            ),
            p(strong("For the 7+1 reading system"), " (k = 2, comparing 7+1 vs. 10+):"),
            tags$ul(
              tags$li(strong("α₂"), " = baseline log-odds of 7+1 vs. 10+ for a Maġribī scholar at the average century."),
              tags$li(strong("β₂"), " = regional effect on the 7+1 vs. 10+ log-odds."),
              tags$li(strong("γ₂"), " = century effect on the 7+1 vs. 10+ log-odds (century model only).")
            ),
            p(strong("The 10+ system"), " has no parameters of its own — it is the baseline against which the other two are measured."),
            p("This gives us ", strong("4 parameters"), " in the simple (region-only) model: α₁, α₂, β₁, β₂. ",
              "In the century model, we add γ₁ and γ₂ for ", strong("6 parameters"), " total. ",
              "We refer to the complete set as ", strong("θ"), " (theta). The entire goal of the Bayesian analysis is to ",
              "estimate the posterior distribution over θ — to determine which combinations of these values are consistent with the observed data."),

            h4("The Softmax Transformation"),
            p("The softmax converts the log-odds values back into probabilities that automatically sum to one:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "P(system = k) = exp(η_k) / [exp(η₁) + exp(η₂) + exp(η₃)]\n\nSince η₃ = 0, exp(η₃) = 1, so:\n\nP(7)   = exp(η₁) / [exp(η₁) + exp(η₂) + 1]\nP(7+1) = exp(η₂) / [exp(η₁) + exp(η₂) + 1]\nP(10+) = 1 / [exp(η₁) + exp(η₂) + 1]"),

            h4("A Worked Example"),
            p("Suppose for a Mašriqī scholar in the 5th century, the model estimates η₁ (7 vs. 10+) = −1.2 and η₂ (7+1 vs. 10+) = −2.0. Then:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "P(7)   = exp(−1.2) / (exp(−1.2) + exp(−2.0) + 1) = 0.301 / 1.436 = 0.210\nP(7+1) = exp(−2.0) / 1.436 = 0.094\nP(10+) = 1 / 1.436 = 0.696"),
            p("This scholar has an estimated 70% probability of writing about the 10+ system, 21% for the 7-reading system, ",
              "and 9% for 7+1. The negative log-odds for both non-reference categories reflect that, for this Mašriqī scholar, ",
              "the 10+ system is more probable than either alternative. This estimate is consistent with the raw bibliographic data, ",
              "which shows that a majority of 5th-century Mašriqī works in the corpus transmit 10+ Readings."),
            hr(),

            # 5. Prior Distributions
            h3("5. Prior Distributions"),
            p("Before seeing any data, we specify what we believe about the parameters — the ", strong("prior distributions"), ":"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "α₁, α₂ ~ Normal(0, 5)    intercepts (7 vs. 10+ and 7+1 vs. 10+)\nβ₁, β₂ ~ Normal(0, 2)    regional effects\nγ₁, γ₂ ~ Normal(0, 2)    century effects (century model only)"),
            p(strong("Intercepts, Normal(0, 5):"), " Centered at zero (no prior expectation that the 7 or 7+1 system is more or less common ",
              "than 10+), with a standard deviation of 5 on the log-odds scale. A log-odds of ±5 corresponds to probabilities ",
              "near 0.007 or 0.993 — this is a very wide prior allowing any baseline probability."),
            p(strong("Effects, Normal(0, 2):"), " Centered at zero (no prior expectation about the direction of effects), ",
              "with a standard deviation of 2. A log-odds shift of ±2 can move a probability from 0.50 to about 0.88 or 0.12 — a substantial effect. ",
              "Given that our observed proportions range from 0.02 (Maġrib × 10+, 4 works) to 0.51 (Mašriq × 10+, 55 works), ",
              "shifts of this magnitude are large enough to accommodate any plausible regional effect while discouraging estimates that imply near-zero or near-one probabilities."),
            p("These are ", strong("weakly informative priors"), ": they gently regularize the estimates without imposing strong beliefs. ",
              "With 172 observations, the data overwhelm the prior for most parameters. The prior matters most for sparse cells ",
              "(e.g., Maġrib × 10+ with only 4 works), where it prevents absurdly confident claims from minimal evidence."),
            hr(),

            # 6. The Likelihood
            h3("6. The Likelihood"),
            p("The likelihood measures how probable the observed data are for each possible set of parameter values — ",
              "it is the bridge between the abstract model and the concrete data. For work ", em("i"),
              ", observed to describe system ", em("y_i"), ", the likelihood is the predicted probability of the system actually observed:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "L_i(θ) = P(y_i | θ, region_i, century_i)"),
            p("If work ", em("i"), " describes the 10+ system and the model predicts P(10+) = 0.70, then L_i = 0.70. ",
              "If instead the model predicted P(10+) = 0.10 for this work, L_i would be only 0.10 — the model would be penalized ",
              "for assigning low probability to what was actually observed. Assuming independence across works, the total likelihood is the product over all 172 observations:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "L(θ) = ∏ᵢ P(y_i | θ, region_i, century_i)"),
            p("In practice, we work with the ", strong("log-likelihood"), " (sum of log-probabilities) to avoid numerical underflow. ",
              "The likelihood concentrates the posterior around parameter values that are consistent with the observed data."),
            hr(),

            # 7. The Posterior Distribution
            h3("7. The Posterior Distribution"),
            p("Combining the prior and likelihood via Bayes' theorem yields the ", strong("posterior"),
              " — a probability distribution over all plausible parameter values given our data:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "P(θ | data) ∝ L(θ) × P(θ)"),
            p("The ∝ (\"proportional to\") means we can ignore the normalizing constant P(data), which is the same for all parameter values. ",
              "Values of θ that were plausible a priori (high prior probability) ", em("and"), " that make the observed data probable ",
              "(high likelihood) receive the most posterior weight. The posterior thus represents a compromise between what we expected and what we observed."),
            p("For our model, the posterior is a continuous distribution over the 4-dimensional (or 6-dimensional) parameter space of θ. ",
              "It has no closed-form expression — we cannot write it as a simple formula. Instead, we ", strong("sample"), " from it numerically."),
            hr(),

            # 8. HMC
            h3("8. Hamiltonian Monte Carlo: How We Sample the Posterior"),
            h4("The Core Idea of MCMC"),
            p("Markov Chain Monte Carlo (MCMC) generates a sequence of parameter values — a \"chain\" — constructed so that the frequency ",
              "with which it visits any region of parameter space is proportional to the posterior probability there. ",
              "Collect enough samples, and you have an empirical approximation to the posterior."),
            p("A key property: MCMC only needs ", strong("ratios"), " of posterior densities. When comparing two parameter values, ",
              "the intractable normalizing constant P(data) appears in both the numerator and denominator and cancels out. ",
              "This is what makes Bayesian inference computationally feasible for complex models — we never need to evaluate the integral ",
              "that would be required to compute P(data) directly."),

            h4("The Problem with Simple Approaches"),
            p("The simplest MCMC method (the Metropolis algorithm, 1953) proposes new parameter values by adding random noise to the ",
              "current position. It then computes the ratio of posterior density at the proposed point to the current point: if the new point is ",
              "more probable, always accept; if less probable, accept with probability equal to the ratio (so a point half as probable is ",
              "accepted 50% of the time). Run long enough, this produces exact posterior samples. But for models with more than a few parameters, ",
              "it faces a fundamental tension: small random steps are almost always accepted but barely explore the posterior, while large ",
              "random steps would cover ground but almost always land in low-probability regions and get rejected. In high dimensions, the volume ",
              "of \"wrong\" landing spots grows exponentially relative to \"right\" ones."),

            h4("How HMC Breaks the Impasse"),
            p("Hamiltonian Monte Carlo (HMC), named after the physicist William Rowan Hamilton (1805–1865), replaces random jumps with ",
              strong("gradient-guided trajectories"), ". It treats the model parameters as the position of a particle in a physical system ",
              "where the negative log-posterior is \"potential energy\" — high-probability regions are valleys, low-probability regions are peaks. ",
              "At each step, the particle receives a random momentum (a random \"flick\"), then its trajectory is simulated using Hamilton's equations of motion."),
            p("The crucial insight is ", strong("energy conservation"), ": Hamilton's equations guarantee that total energy (kinetic + potential) ",
              "is conserved along the trajectory. If the particle starts in a high-probability valley with some kinetic energy, it will end up ",
              "in another high-probability valley regardless of how far it travels — trading potential for kinetic energy and back, like a ball ",
              "rolling between hills. Because the trajectory also follows the ", strong("gradient"), " of the log-posterior, the particle naturally ",
              "curves along the contours of the posterior rather than shooting off into probability desert. The result: proposals that are both far ",
              "from the current position and in high-probability regions, breaking the tension that cripples random-walk methods."),
            p("The version used by Stan is the ", strong("No-U-Turn Sampler (NUTS)"), ", which automatically determines trajectory length by ",
              "stopping when the particle begins to double back. The step size is tuned during warmup. A Metropolis accept/reject check at the end ",
              "of each trajectory corrects for small numerical errors in the discrete-time simulation, guaranteeing that the samples come from ",
              "exactly the right distribution."),
            p("Computing the gradient at every step along the trajectory requires ", strong("automatic differentiation"),
              " — Stan compiles the model to C++ code that evaluates both the log-posterior and its gradient simultaneously. ",
              "This compilation step (the brief delay when fitting the model) is the price paid for HMC's efficiency gains over random-walk methods."),
            hr(),

            # 9. Sampling Configuration
            h3("9. Sampling Configuration"),
            tags$ul(
              tags$li(strong("Chains"), ": 4 independent chains, each starting from a different random position"),
              tags$li(strong("Iterations per chain"), ": 2,000 total (1,000 warmup + 1,000 sampling)"),
              tags$li(strong("Total posterior samples"), ": 4,000 (1,000 per chain × 4 chains)")
            ),
            p("The ", strong("warmup phase"), " adapts the sampler's internal tuning parameters — the step size and the \"mass matrix\" ",
              "that scales the momentum to match the posterior's shape. These warmup samples are discarded; they do not represent the posterior. ",
              "Running ", strong("4 chains"), " from different starting points enables convergence diagnostics:"),
            tags$ul(
              tags$li(strong("R̂ (R-hat)"), ": compares within-chain to between-chain variance. R̂ < 1.01 indicates convergence; R̂ > 1.05 means results should not be trusted."),
              tags$li(strong("Effective sample size (ESS)"), ": how many independent samples the chain is worth after accounting for autocorrelation. Values above 400 are adequate."),
              tags$li(strong("Divergent transitions"), ": Stan-specific warning that the posterior surface was too steep to navigate safely, suggesting potential bias.")
            ),
            hr(),

            # 10. From Posterior Samples to Results
            h3("10. From Posterior Samples to Results"),
            p("Once we have 4,000 samples, each is a complete set of values for all parameters in θ. We transform these into quantities of interest."),

            h4("Predicted Probabilities"),
            p("For each posterior sample ", em("s"), " and each region–century combination:"),
            tags$ol(
              tags$li("Plug the sampled parameters into the linear predictor: η_k", tags$sup("(s)"), " = α_k", tags$sup("(s)"), " + β_k", tags$sup("(s)"), " × region + γ_k", tags$sup("(s)"), " × century"),
              tags$li("Apply the softmax to get P", tags$sup("(s)"), "(7), P", tags$sup("(s)"), "(7+1), P", tags$sup("(s)"), "(10+)")
            ),
            p("This gives 4,000 predicted probability vectors per region–century combination, summarized as:"),
            tags$ul(
              tags$li(strong("Posterior mean"), ": average across all samples — our best single estimate"),
              tags$li(strong("95% credible interval"), ": the 2.5th and 97.5th percentiles — the range containing 95% of plausible values")
            ),

            h4("Posterior Contrasts (Regional Comparisons)"),
            p("To compare regions, we compute the difference in predicted probability for each posterior sample:"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
              "Δ(s) = P(s)(k | Mašriq) − P(s)(k | Maġrib)"),
            p("From these 4,000 differences we report: ", strong("P(Δ > 0)"),
              " (the fraction of samples favoring Mašriq — a direct probability statement), the ", strong("mean difference"),
              ", and the ", strong("95% credible interval of the difference"),
              ". For example, if 3,992 of 4,000 samples have P(10+ | Mašriq) > P(10+ | Maġrib), ",
              "we report a 99.8% posterior probability of Mašriqī dominance in 10+ production."),

            h4("Posterior Predictive Checks"),
            p("To assess model adequacy, we simulate fake datasets: for each posterior sample, we use each work's actual region (and century) ",
              "to predict a probability vector, randomly draw a system from it, and count the totals. If the observed counts fall within the ",
              "95% range of simulated counts, the model is capturing the data-generating process adequately."),
            hr(),

            # 11. Interpreting the Results
            h3("11. Interpreting the Results"),
            h4("Credible Intervals"),
            p("A ", strong("95% credible interval"), " of [0.39, 0.57] for P(10+ | Mašriq) means: given the data and the model, there is a ",
              "95% probability the true value lies in that range. This is a direct probability statement about the parameter — ",
              "unlike a frequentist confidence interval, which has a more complex interpretation involving hypothetical repeated experiments. ",
              "When credible intervals for a regional comparison exclude zero, we have strong evidence of a genuine difference."),

            h4("Posterior Probabilities"),
            p("When the app reports ", strong("\"P(Mašriq > Maġrib for 10+) = 0.998,\""),
              " this means 99.8% of posterior parameter values produce a higher 10+ probability in the Mašriq than in the Maġrib. ",
              "In other words, given what we have observed, we can be 99.8% confident in the direction of this regional difference."),

            h4("Varying Certainty"),
            p("Not all differences are equally certain. The 10+ system (55 Mašriqī vs. 4 Maġribī works) yields narrow posterior uncertainty. ",
              "The 7-reading system (48 Maġribī vs. 38 Mašriqī) yields wider uncertainty, and the 7+1 system (27 total works) the widest. ",
              "The Bayesian framework quantifies this automatically — more data means tighter intervals."),

            h4("Regularization"),
            p("For sparse cells (like Maġrib × 10+ with only 4 works), the prior gently pulls extreme estimates toward the overall average. ",
              "This ", strong("regularization"), " produces more conservative and reliable results than raw proportions, which can be misleadingly ",
              "precise when based on few observations."),
            hr(),

            # 12. The Century Model
            h3("12. The Century Model"),
            p("When \"Include Century Effect\" is enabled, the model adds γ₁ and γ₂ parameters capturing temporal trends in how reading ",
              "system preferences changed over time. The century variable is ", strong("mean-centered"),
              " (each value minus the average century across all works) so that the intercept α_k represents log-odds at the average century ",
              "rather than at century zero, which has no meaningful interpretation."),
            p("The century effect γ_k captures temporal dynamics: a negative γ₁ would indicate that the 7-reading system became less dominant ",
              "over time relative to 10+. Combined with the regional effect, this allows the model to detect patterns like the dramatic historical ",
              "shift in which the 10+ system expanded in the Mašriq while the Maġrib remained focused on the 7-reading tradition — a divergence ",
              "that intensified century by century across the period under study."),
            hr(),

            # 13. Technical Implementation
            h3("13. Technical Implementation"),
            p("The model specification, prior choices, and overall analytical workflow follow closely the approach developed by Richard McElreath in ",
              em("Statistical Rethinking: A Bayesian Course with Examples in R and Stan"),
              " (2nd ed., 2020), particularly his treatment of multinomial categorical models in Chapter 11 (\"God Spiked the Integers\") ",
              "and his framework for generative modeling and posterior predictive checking throughout the book. ",
              "McElreath's companion R package ", code("rethinking"), " — and especially his ", code("ulam()"),
              " function, which translates R model formulae into Stan code — was instrumental in developing the initial version of this app. ",
              "The current version uses ", strong("cmdstanr"), " (the R interface to CmdStan) directly for greater deployment flexibility, ",
              "but the underlying Stan model preserves the structure and parameterization from the ", code("rethinking"), "-based prototype. ",
              "McElreath's code repository (", tags$a(href = "https://github.com/rmcelreath/rethinking", target = "_blank", "github.com/rmcelreath/rethinking"),
              ") and the accompanying lecture series were essential references throughout."),
            p("Stan's ", code("categorical_logit"), " function handles the softmax transformation internally with numerical stability. ",
              "Below is the Stan model for the century version (the simple model omits ", code("beta_cent"), " and the ", code("century"), " data):"),
            tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 0.95em; overflow-x: auto;",
"data {
  int<lower=1> N;                          // number of works (172)
  int<lower=2> K;                          // number of categories (3)
  array[N] int<lower=1,upper=K> y;         // observed system for each work
  array[N] int<lower=0,upper=1> geo;       // region: 0 = Maġrib, 1 = Mašriq
  array[N] real century;                   // mean-centered death century
}
parameters {
  vector[K-1] alpha;                       // intercepts (2 free parameters)
  vector[K-1] beta_geo;                    // regional effects
  vector[K-1] beta_cent;                   // century effects
}
model {
  alpha ~ normal(0, 5);                    // weakly informative priors
  beta_geo ~ normal(0, 2);
  beta_cent ~ normal(0, 2);

  for (i in 1:N) {
    vector[K] p;
    p[K] = 0;                             // reference category (10+ readings)
    for (k in 1:(K-1)) {
      p[k] = alpha[k] + beta_geo[k] * geo[i] + beta_cent[k] * century[i];
    }
    y[i] ~ categorical_logit(p);          // softmax applied internally
  }
}"),
            hr(),

            # 14. Summary of the Pipeline
            h3("14. Summary of the Pipeline"),
            tags$table(class = "table table-bordered table-sm",
              tags$thead(tags$tr(
                tags$th("Step"), tags$th("What Happens"), tags$th("Mathematical Object")
              )),
              tags$tbody(
                tags$tr(tags$td("1. Data"), tags$td("172 works classified by system, region, century"), tags$td("Observed counts")),
                tags$tr(tags$td("2. Model"), tags$td("Multinomial logistic regression with softmax"), tags$td("P(system) = softmax(α + β×region + γ×century)")),
                tags$tr(tags$td("3. Priors"), tags$td("Weakly informative Normal distributions"), tags$td("α ~ N(0,5), β ~ N(0,2), γ ~ N(0,2)")),
                tags$tr(tags$td("4. Likelihood"), tags$td("Probability of observed data given parameters"), tags$td("∏ᵢ P(yᵢ | θ, regionᵢ, centuryᵢ)")),
                tags$tr(tags$td("5. Posterior"), tags$td("Bayes' theorem combines prior and likelihood"), tags$td("P(θ | data) ∝ L(θ) × P(θ)")),
                tags$tr(tags$td("6. Sampling"), tags$td("HMC/NUTS generates 4,000 posterior draws"), tags$td("4 chains × 1,000 post-warmup samples")),
                tags$tr(tags$td("7. Predictions"), tags$td("Softmax applied to each posterior draw"), tags$td("4,000 probability vectors per region–century")),
                tags$tr(tags$td("8. Contrasts"), tags$td("Differences computed sample-by-sample"), tags$td("P(system | Mašriq) − P(system | Maġrib)")),
                tags$tr(tags$td("9. Checks"), tags$td("Simulated data compared to observed"), tags$td("Posterior predictive distributions"))
              )
            ),
            hr(),

            # References
            h3("References"),
            h4("Primary Methodological Source"),
            tags$ul(
              tags$li("McElreath, R. (2020). ", em("Statistical Rethinking: A Bayesian Course with Examples in R and Stan"),
                " (2nd ed.). CRC Press. Companion R package and code: ",
                tags$a(href = "https://github.com/rmcelreath/rethinking", target = "_blank", "github.com/rmcelreath/rethinking"),
                ". Lecture series: ",
                tags$a(href = "https://www.youtube.com/playlist?list=PLDcUM9US4XdMROZ57-OIRtIK0aOynbgZN", target = "_blank", "YouTube playlist"), ".")
            ),
            h4("Additional Bayesian References"),
            tags$ul(
              tags$li("Gelman, A., et al. (2013). ", em("Bayesian Data Analysis"), " (3rd ed.). CRC Press."),
              tags$li("Kruschke, J. K. (2014). ", em("Doing Bayesian Data Analysis"), " (2nd ed.). Academic Press.")
            ),
            h4("Computational Methods"),
            tags$ul(
              tags$li("Betancourt, M. (2017). \"A Conceptual Introduction to Hamiltonian Monte Carlo.\" ", em("arXiv:1701.02434"), "."),
              tags$li("Carpenter, B., et al. (2017). \"Stan: A Probabilistic Programming Language.\" ", em("Journal of Statistical Software"), ", 76(1)."),
              tags$li("Hoffman, M. D., & Gelman, A. (2014). \"The No-U-Turn Sampler.\" ", em("JMLR"), ", 15(1), 1593–1623.")
            )
          )
        )
      ),

      # ========== Tab 5: Bayesian Analysis (4 cards) ==========
      tabPanel(
        title = tagList(icon("chart-line"), "Bayesian Analysis"),
        value = "bayesian_analysis",
        br(),

        # Card Navigation
        div(class = "card-navigation",
          actionButton("bayes_prev", icon("arrow-left"), class = "btn-secondary"),
          uiOutput("bayes_card_indicator"),
          actionButton("bayes_next", icon("arrow-right"), class = "btn-secondary")
        ),

        # Card content container
        uiOutput("bayes_current_card"),

        # Download button for saving model (conditionally shown)
        conditionalPanel(
          condition = "output.model_fitted",
          downloadButton("save_model", "Save Current Model", class = "btn-success", style = "margin-top: 10px;")
        )
      ),

      # ========== Tab 6: Acknowledgements ==========
      tabPanel(
        title = "Acknowledgements",
        value = "acknowledgements",
        br(),
        div(class = "card",
          div(class = "card-header", "Acknowledgements"),
          div(class = "card-body",
            h4("Funding"),
            p("This research is supported by the European Research Council (ERC) under the European Union's ",
              "Horizon 2020 research and innovation programme (Grant number 101054849)."),
            br(),
            h4("Institutional Support"),
            p("This project is based at the Leiden University Centre for Linguistics (LUCL), ",
              "Leiden University, Faculty of Humanities."),
            br(),
            h4("Data Sources"),
            p("Geographic coordinate data is derived from the Thurayya Gazetteer project. ",
              "Bibliographic data has been compiled from primary sources and existing scholarly catalogues."),
            br(),
            h4("Technical"),
            p("This application was built using R Shiny with Bayesian modeling via CmdStan. ",
              "Geographic visualizations use Leaflet.js.")
          )
        )
      ),

      # ========== Tab 6: Reference Annotation (DISABLED - all 38 references validated) ==========
      # Annotation interface commented out - text_reuse data is now validated and displayed in Corpus Explorer
      # To re-enable, uncomment this tabPanel and the server logic at the end of the file
      #
      # tabPanel(
      #   title = tagList(icon("link"), "Reference Annotation"),
      #   value = "reference_annotation",
      #   ... (annotation UI removed)
      # )
    ),

    # ========== Global Footer (appears on all tabs) ==========
    tags$footer(
      style = "
        margin-top: 40px;
        padding: 25px 0;
        border-top: 1px solid #e0e0e0;
        background-color: #fafafa;
        text-align: center;
      ",
      div(
        style = "display: flex; justify-content: center; align-items: center; flex-wrap: wrap; gap: 40px;",
        tags$a(
          href = "https://erc.europa.eu/",
          target = "_blank",
          tags$img(src = "images/erc_logo.png", height = "70px", alt = "European Research Council",
                   style = "opacity: 0.9; transition: opacity 0.2s;",
                   onmouseover = "this.style.opacity='1'",
                   onmouseout = "this.style.opacity='0.9'")
        ),
        tags$a(
          href = "https://www.universiteitleiden.nl/en",
          target = "_blank",
          tags$img(src = "images/leiden_logo.png", height = "70px", alt = "Leiden University",
                   style = "opacity: 0.9; transition: opacity 0.2s;",
                   onmouseover = "this.style.opacity='1'",
                   onmouseout = "this.style.opacity='0.9'")
        ),
        tags$a(
          href = "https://www.universiteitleiden.nl/en/research/research-projects/humanities/quran-quotations-in-literary-arabic-texts",
          target = "_blank",
          tags$img(src = "images/qurxan_logo.png", height = "60px", alt = "QurXan Project",
                   style = "opacity: 0.9; transition: opacity 0.2s;",
                   onmouseover = "this.style.opacity='1'",
                   onmouseout = "this.style.opacity='0.9'")
        )
      ),
      p(
        style = "margin-top: 15px; font-size: 0.85em; color: #666;",
        "Funded by the European Research Council (ERC) under the European Union's Horizon 2020 programme (Grant 101054849)"
      )
    )
  )
)

# Server ====
server <- function(input, output, session) {

  # Initialize app on startup
  init_data <- initialize_app()

  rv <- reactiveValues(
    raw_data = init_data$raw,
    clean_data = init_data$clean,
    # Pre-load Bayesian results if available (for cloud deployment)
    fit_obj = PRECOMPUTED,
    fit_info = if (!is.null(PRECOMPUTED)) PRECOMPUTED$fit_info else NULL,
    fit_counter = 0,
    posterior_preds = NULL,
    contrasts_computed = FALSE,
    contrast_results = list(),
    from_database = TRUE,
    model_summary_visited = FALSE,
    analysis_results_visited = FALSE,
    # Geographic data removed (see geographic_alpha.R)
    # New: Bayesian card navigation
    bayes_current_card = 1,
    bayesian_analysis_visited = if (!is.null(PRECOMPUTED)) TRUE else FALSE,
    # Posterior distribution modal
    selected_posterior_param = NULL,
    # Citation modal export data
    modal_citations = NULL
  )

  # Geographic data loading removed (see geographic_alpha.R)

  # Show initialization notification (using init_data, not rv)
  if (init_data$initialized) {
    showNotification(
      HTML(paste0(
        "<strong>✓ App Initialized</strong><br/>",
        init_data$n_works, " works loaded from database<br/>",
        "<small>system × regionality × century</small>"
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
           " Variables: system (outcome), regionality (geographic)"),
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

  # Dynamic works count for Home Page
  output$home_works_count <- renderText({
    if (!is.null(rv$clean_data)) {
      nrow(rv$clean_data)
    } else {
      "172"  # Fallback
    }
  })

  # Dynamic works count for Corpus Explorer
  output$corpus_works_count <- renderText({
    if (!is.null(rv$clean_data)) {
      nrow(rv$clean_data)
    } else {
      "172"  # Fallback
    }
  })

  # ============================================================================
  # CORPUS EXPLORER - Enhanced Search
  # ============================================================================

  # Update work type filter choices based on data
  # Extract individual types from comma/semicolon-separated values
  observe({
    req(rv$raw_data)
    raw_types <- rv$raw_data$type
    raw_types <- raw_types[!is.na(raw_types) & raw_types != ""]
    # Split on comma or semicolon to get individual types
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
      # Keep rows that match either title OR author
      df <- df[title_matches | author_matches, ]
    }

    # Filter by reading set (multi-select)
    if (!is.null(input$filter_system) && length(input$filter_system) > 0) {
      df <- df %>% filter(system %in% input$filter_system)
    }

    # Filter by region (multi-select)
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      # Build a combined filter for selected regions
      region_filter <- rep(FALSE, nrow(df))
      for (reg in input$filter_region) {
        if (reg == "inter-regional") {
          # Inter-regional: authors with both maġrib AND mašriq in their regionality
          region_filter <- region_filter | (
            grepl("maġrib", df$regionality, ignore.case = TRUE) &
            grepl("mašriq", df$regionality, ignore.case = TRUE)
          )
        } else {
          region_filter <- region_filter | grepl(paste0("^", reg), df$regionality, ignore.case = TRUE)
        }
      }
      df <- df[region_filter, ]
    }

    # Filter by work type (multi-select)
    if (!is.null(input$filter_type) && length(input$filter_type) > 0) {
      # Check if type contains any of the selected types (handles comma-separated)
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

    # Helper to create recycle symbol with hover text (using CSS tooltip for fast display)
    create_text_reuse_symbol <- function(text_reuse, reused_title, reused_author,
                                         commentary_titles, commentary_types, commentary_authors) {
      symbols <- c()

      # Check if this work reuses another (is a commentary)
      if (!is.na(text_reuse) && text_reuse == 1 && !is.na(reused_title)) {
        reused_title_fmt <- format_camel_case(reused_title, "title")
        reused_author_fmt <- format_camel_case(reused_author, "author")
        hover_text <- paste0("Reuses ", reused_title_fmt, " by ", reused_author_fmt)
        # Green recycle symbol for "this work reuses another" - using data-tooltip for CSS hover
        symbols <- c(symbols, sprintf(
          '<span class="text-reuse-tooltip" data-tooltip="%s" style="cursor: help; color: #28a745; margin-left: 5px;">&#x267B;</span>',
          htmltools::htmlEscape(hover_text)
        ))
      }

      # Check if this work is subject of commentary (other works reference it)
      if (!is.na(commentary_titles) && commentary_titles != "") {
        # Parse pipe-separated values
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

        # Build hover text for each commentary (one per line)
        commentary_lines <- sapply(seq_along(titles), function(i) {
          title_fmt <- format_camel_case(titles[i], "title")
          author_fmt <- if (!is.na(authors[i]) && authors[i] != "") {
            format_camel_case(authors[i], "author")
          } else {
            "Unknown"
          }
          # Format type: "commentary_compression" → "commentary (compression)"
          raw_type <- if (i <= length(types) && !is.na(types[i]) && types[i] != "") types[i] else "commentary"
          # Handle underscore-separated type labels
          if (grepl("_", raw_type)) {
            parts <- strsplit(raw_type, "_")[[1]]
            type_label <- paste0(parts[1], " (", paste(parts[-1], collapse = " "), ")")
          } else {
            type_label <- raw_type
          }
          paste0("• ", title_fmt, " (", type_label, ") by ", author_fmt)
        })

        # Header with count, then list each work
        n_works <- length(commentary_lines)
        header <- paste0("Subject of ", n_works, " dependent work", ifelse(n_works > 1, "s", ""), ":")
        # Join with newline character for display
        hover_text <- paste0(header, "\n", paste(commentary_lines, collapse = "\n"))

        # Blue recycle symbol for "this work is subject of commentary" - using data-tooltip
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
        # Create text reuse symbol
        reuse_symbol = create_text_reuse_symbol(text_reuse, reused_title, reused_author,
                                                commentary_titles, commentary_types, commentary_authors),
        # Citation indicator for works
        cite_indicator = "",
        # Clickable title with citation indicator (escaped for XSS safety)
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
        # Citation indicator for authors
        author_cite_indicator = "",
        # Clickable author name with citation indicator (escaped for XSS safety)
        Author = paste0(
          '<a href="#" class="citation-link" onclick="Shiny.setInputValue(\'clicked_author\', ',
          as.integer(author_id),
          ', {priority: \'event\'}); return false;">',
          sapply(author_name, function(x) htmltools::htmlEscape(format_camel_case(x, "author"))),
          '</a>',
          author_cite_indicator
        ),
        # Color badges
        Set = sapply(system, function(x) create_color_badge(x, "system")),
        # Handle multi-type works (separated by semicolon or comma)
        Type = sapply(type, function(x) {
          if (is.na(x) || x == "") return("")
          # Check for multiple types (semicolon or comma separated)
          if (grepl("[;,]", x)) {
            # Split on semicolon or comma, create badge for each type
            types <- trimws(strsplit(x, "[;,]")[[1]])
            types <- types[types != ""]  # Remove empty strings
            paste(sapply(types, function(t) create_color_badge(t, "type")), collapse = " ")
          } else {
            create_color_badge(x, "type")
          }
        }),
        # Origin column - show detailed labels for inter-regional scholars
        Origin = sapply(regionality, function(x) {
          if (is.na(x) || x == "") return("")
          x_lower <- tolower(x)
          # Check for specific inter-regional patterns first
          if (grepl("maġrib visits mašriq", x_lower) || grepl("maghrib visits mashriq", x_lower)) {
            create_color_badge("Maġrib visits Mašriq", "region")
          } else if (grepl("mašriq visits maġrib", x_lower) || grepl("mashriq visits maghrib", x_lower)) {
            create_color_badge("Mašriq visits Maġrib", "region")
          } else if (grepl("^maġrib", x_lower) || grepl("^maghrib", x_lower)) {
            create_color_badge("Maġrib", "region")
          } else if (grepl("^mašriq", x_lower) || grepl("^mashriq", x_lower)) {
            create_color_badge("Mašriq", "region")
          } else {
            x
          }
        }),
        # Simplified century display (just the number)
        Century = as.character(death_century)
      ) %>%
      select(Title, Author, Set, Type, Origin, Century)

    datatable(
      display_df,
      escape = FALSE,  # Allow HTML
      caption = htmltools::tags$caption(style = "caption-side:bottom;text-align:left;font-size:0.85em;color:#888;padding-top:4px;",
        "Click on a title or author name to view citations and details."),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',  # Table, info, pagination only
        columnDefs = list(
          list(className = 'dt-left', targets = c(0, 1))
        )
      ),
      rownames = FALSE
    )
  })

  # Download handlers for CSV and JSON
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("iqsa_corpus_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- filtered_corpus()
      # Prepare export data with formatted fields
      export_df <- df %>%
        mutate(
          title_formatted = sapply(title, function(x) format_camel_case(x, "title")),
          author_formatted = sapply(author_name, function(x) format_camel_case(x, "author"))
        ) %>%
        select(work_id, title = title_formatted, author = author_formatted,
               reading_set = system, type, region = regionality, death_century)
      write.csv(export_df, file, row.names = FALSE)
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      paste0("iqsa_corpus_", format(Sys.Date(), "%Y%m%d"), ".json")
    },
    content = function(file) {
      df <- filtered_corpus()
      # Prepare export data with formatted fields
      export_df <- df %>%
        mutate(
          title_formatted = sapply(title, function(x) format_camel_case(x, "title")),
          author_formatted = sapply(author_name, function(x) format_camel_case(x, "author"))
        ) %>%
        select(work_id, title = title_formatted, author = author_formatted,
               reading_set = system, type, region = regionality, death_century)
      write(jsonlite::toJSON(export_df, pretty = TRUE), file)
    }
  )

  # ============================================================================
  # GEOGRAPHIC EXPLORER - Removed (see deploy/geographic_alpha.R)
  # ============================================================================
  if (FALSE) {

  # City visits (nodes) reactive
  geo_city_visits <- reactive({
    routes <- geo_filtered_routes()
    if (length(routes) == 0) return(NULL)
    count_city_visits(routes)
  })

  # Normalize city name for coordinate lookup
  normalize_city_name <- function(name) {
    name <- tolower(name)
    name <- gsub("[ʾʿ]", "", name)           # Remove hamza/ayn
    name <- gsub("^al-", "", name)           # Remove "al-" prefix
    name <- gsub("^al ", "", name)           # Remove "al " prefix (space variant)
    name
  }

  # Base map output with static elements (rectangles + dividing line)
  # These are rendered immediately so the map is never blank
  output$geo_travel_map <- renderLeaflet({
    rects <- get_subregion_rects()
    dividing_line <- get_dividing_line()

    # Start with base map
    map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(opacity = 0.5)) %>%
      setView(lng = 35, lat = 32, zoom = 4)

    # Add each subregion rectangle with labels
    for (i in 1:nrow(rects)) {
      map <- map %>%
        addRectangles(
          lng1 = rects$xmin[i], lat1 = rects$ymin[i],
          lng2 = rects$xmax[i], lat2 = rects$ymax[i],
          fillColor = rects$color[i],
          fillOpacity = 0.2,
          color = rects$color[i],
          weight = 2,
          opacity = 0.7,
          layerId = paste0("rect_", rects$subregion[i]),
          group = "subregions"
        ) %>%
        addLabelOnlyMarkers(
          lng = rects$center_lon[i], lat = rects$center_lat[i],
          label = rects$subregion[i],
          labelOptions = labelOptions(
            noHide = TRUE, direction = "center", textOnly = TRUE,
            style = list("font-size" = "11px", "font-weight" = "bold", "color" = rects$color[i])
          )
        )
    }

    # Add Mashriq-Maghrib dividing line
    map <- map %>%
      addPolylines(
        lng = dividing_line$lon, lat = dividing_line$lat,
        color = "black", weight = 3, opacity = 0.8,
        dashArray = "10, 10",
        popup = "Mašriq / Maġrib Boundary",
        group = "dividing_line"
      )

    map
  })

  # Load mobility data from database
  geo_mobility_data <- reactive({

    tryCatch({
      con <- dbConnect(SQLite(), DB_PATH)
      on.exit(dbDisconnect(con))

      authors <- dbGetQuery(con, "
        SELECT author_id,
               COALESCE(author_name_canonical, author_name) as author_name,
               author_name_arabic, death_century,
               regionality, home_subregion, subregions_visited,
               inter_regional, total_distance_km, mobility_category
        FROM authors
        WHERE home_subregion IS NOT NULL
          AND in_range = 'T'
      ")

      # Get subregion counts (only for authors in_range)
      subregion_stats <- dbGetQuery(con, "
        SELECT home_subregion as subregion,
               COUNT(*) as n_authors,
               SUM(CASE WHEN mobility_category = 'sedentary' THEN 1 ELSE 0 END) as sedentary,
               SUM(CASE WHEN mobility_category = 'local-traveler' THEN 1 ELSE 0 END) as local_traveler,
               SUM(CASE WHEN mobility_category = 'extensive-local' THEN 1 ELSE 0 END) as extensive,
               SUM(CASE WHEN mobility_category = 'inter-regional' THEN 1 ELSE 0 END) as inter_regional,
               AVG(total_distance_km) as avg_distance,
               AVG(subregions_visited) as avg_subregions
        FROM authors
        WHERE home_subregion IS NOT NULL
          AND in_range = 'T'
        GROUP BY home_subregion
      ")

      list(authors = authors, subregion_stats = subregion_stats)
    }, error = function(e) {
      message("Error loading mobility data: ", e$message)
      NULL
    })
  })

  # Handle clicks on subregion rectangles - show popup with statistics
  observeEvent(input$geo_travel_map_shape_click, {
    click <- input$geo_travel_map_shape_click
    if (is.null(click) || is.null(click$id)) return()

    # Extract subregion name from layer ID (format: "rect_<subregion>")
    if (!startsWith(click$id, "rect_")) return()
    subregion <- sub("^rect_", "", click$id)

    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return()

    subregion_stats <- mobility_data$subregion_stats
    rects <- get_subregion_rects()

    stats <- subregion_stats[subregion_stats$subregion == subregion, ]
    rect_info <- rects[rects$subregion == subregion, ]

    if (nrow(rect_info) == 0) return()

    n_authors <- if (nrow(stats) > 0) stats$n_authors else 0
    sedentary <- if (nrow(stats) > 0) stats$sedentary else 0
    local_trav <- if (nrow(stats) > 0) stats$local_traveler else 0
    extensive <- if (nrow(stats) > 0) stats$extensive else 0
    inter_reg <- if (nrow(stats) > 0) stats$inter_regional else 0
    avg_dist <- if (nrow(stats) > 0 && !is.na(stats$avg_distance)) round(stats$avg_distance) else 0
    avg_subs <- if (nrow(stats) > 0 && !is.na(stats$avg_subregions)) round(stats$avg_subregions, 1) else 0

    fill_color <- rect_info$color[1]
    meta <- rect_info$meta_region[1]

    # Create popup HTML with statistics
    popup_html <- paste0(
      "<div style='min-width: 220px;'>",
      "<strong style='font-size: 1.2em;'>", subregion, "</strong><br/>",
      "<span style='color: ", fill_color, "; font-weight: bold;'>",
      ifelse(meta == "maġrib", "Maġrib", "Mašriq"), "</span>",
      "<hr style='margin: 8px 0;'/>",
      "<strong>", n_authors, "</strong> scholars from here<br/>",
      "<hr style='margin: 8px 0;'/>",
      "<em>Mobility breakdown:</em><br/>",
      "&nbsp;&nbsp;Sedentary: ", sedentary, "<br/>",
      "&nbsp;&nbsp;Local travelers: ", local_trav, "<br/>",
      "&nbsp;&nbsp;Extensive local: ", extensive, "<br/>",
      "&nbsp;&nbsp;Inter-regional: ", inter_reg, "<br/>",
      "<hr style='margin: 8px 0;'/>",
      "<em>Avg distance:</em> ", format(avg_dist, big.mark = ","), " km<br/>",
      "<em>Avg subregions:</em> ", avg_subs,
      "</div>"
    )

    # Show popup at the click location
    leafletProxy("geo_travel_map") %>%
      clearPopups() %>%
      addPopups(
        lng = click$lng, lat = click$lat,
        popup = popup_html
      )
  })

  # Filtered routes reactive - applies all filter criteria
  geo_filtered_routes <- reactive({
    routes <- rv$geo_data$scholar_routes
    mobility_data <- geo_mobility_data()

    if (is.null(routes) || is.null(mobility_data)) return(list())

    authors_df <- mobility_data$authors

    # Apply mobility category filter
    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors_df <- authors_df[authors_df$mobility_category == input$geo_mobility_filter, ]
    }

    # Apply meta-region filter
    if (!is.null(input$geo_meta_region) && input$geo_meta_region != "all") {
      if (input$geo_meta_region == "inter") {
        authors_df <- authors_df[authors_df$inter_regional == 1, ]
      } else {
        authors_df <- authors_df[authors_df$regionality == input$geo_meta_region, ]
      }
    }

    # Apply home subregion filter
    if (!is.null(input$geo_home_subregion) && input$geo_home_subregion != "all") {
      authors_df <- authors_df[authors_df$home_subregion == input$geo_home_subregion, ]
    }

    # Apply century filter
    if (!is.null(input$geo_century_filter)) {
      authors_df <- authors_df[!is.na(authors_df$death_century) &
                               authors_df$death_century >= input$geo_century_filter[1] &
                               authors_df$death_century <= input$geo_century_filter[2], ]
    }

    # Apply subregions visited filter
    if (!is.null(input$geo_subregions_filter)) {
      authors_df <- authors_df[!is.na(authors_df$subregions_visited) &
                               authors_df$subregions_visited >= input$geo_subregions_filter[1] &
                               authors_df$subregions_visited <= input$geo_subregions_filter[2], ]
    }

    # Return only routes for matching authors
    matching_authors <- tolower(authors_df$author_name)
    routes[tolower(names(routes)) %in% matching_authors]
  })

  # Observer 2: Draw filtered routes on the map
  # This runs on initial load and whenever filters change
  observe({
    req(rv$geo_data$scholar_routes)
    req(rv$geo_data$city_coords)

    # Ensure filter inputs are initialized before drawing
    # This helps with initial load display
    isolate({
      input$geo_mobility_filter
      input$geo_meta_region
      input$geo_home_subregion
      input$geo_century_filter
    })

    routes <- geo_filtered_routes()
    coords <- rv$geo_data$city_coords
    gis_sea_routes <- rv$geo_data$gis_sea_routes  # GIS coastal geometries for sea routes
    sea_route_geoms <- rv$geo_data$sea_route_geometries  # Coordinate-keyed lookup for path segments

    # Create coordinate lookup with normalized names
    coords_lookup <- coords
    coords_lookup$city_norm <- sapply(coords_lookup$city, normalize_city_name)

    # Clear previous routes and draw new ones
    proxy <- leafletProxy("geo_travel_map") %>%
      clearGroup("routes")

    if (length(routes) == 0) return()

    # Draw routes for each scholar
    path_routes_drawn <- 0
    fallback_routes_drawn <- 0
    gis_sea_routes_drawn <- 0

    # Helper function to look up GIS sea route geometry by coordinates
    lookup_sea_route <- function(from_code, to_code) {
      if (is.null(sea_route_geoms) || length(sea_route_geoms) == 0) return(NULL)

      # Extract coordinates from Thurayya codes
      from_coords <- extract_coords_from_code(from_code)
      to_coords <- extract_coords_from_code(to_code)

      # Try coordinate-based lookup first (most reliable)
      if (!is.null(from_coords) && !is.null(to_coords)) {
        key <- paste0(from_coords, "_TO_", to_coords)
        if (!is.null(sea_route_geoms[[key]])) return(sea_route_geoms[[key]])
      }

      # Try full code lookup
      key <- paste0(from_code, "_TO_", to_code)
      if (!is.null(sea_route_geoms[[key]])) return(sea_route_geoms[[key]])

      # Try base code lookup (without suffix)
      from_base <- sub("_[SRW]$", "", from_code)
      to_base <- sub("_[SRW]$", "", to_code)
      key <- paste0(from_base, "_TO_", to_base)
      if (!is.null(sea_route_geoms[[key]])) return(sea_route_geoms[[key]])

      return(NULL)
    }

    for (scholar_name in names(routes)) {
      scholar <- routes[[scholar_name]]
      places <- unlist(scholar$places_sequence)
      regionality <- scholar$regionality
      scholar_routes <- scholar$routes  # Detailed routes with path waypoints

      if (length(places) < 2) next

      # Get route color based on regionality
      route_color <- get_edge_color(regionality)

      # Try to use detailed paths from routes array if available
      if (!is.null(scholar_routes) && length(scholar_routes) > 0) {
        for (route in scholar_routes) {
          # Check if this route has a valid path with waypoints
          if (!is.null(route$path) && length(route$path) >= 2) {
            # Draw each segment of the path, using GIS coastal geometry where available
            for (seg_i in 1:(length(route$path) - 1)) {
              from_code <- route$path[seg_i]
              to_code <- route$path[seg_i + 1]

              # Try to get GIS sea route geometry for this segment
              gis_segment <- lookup_sea_route(from_code, to_code)

              if (!is.null(gis_segment) && !is.null(gis_segment$coordinates)) {
                # Draw GIS coastal route for this segment
                seg_coords <- gis_segment$coordinates
                proxy <- proxy %>%
                  addPolylines(
                    lng = seg_coords[, 1],
                    lat = seg_coords[, 2],
                    color = "#0066CC",  # Blue for sea routes
                    weight = 2,
                    opacity = 0.7,
                    popup = paste0("<strong>", scholar_name, "</strong><br/>",
                                  gis_segment$from_name, " → ", gis_segment$to_name,
                                  " (", round(gis_segment$distance_km, 0), " km coastal)"),
                    group = "routes"
                  )
                gis_sea_routes_drawn <- gis_sea_routes_drawn + 1
              } else {
                # Draw straight line for this segment
                from_parsed <- parse_thurayya_coords(from_code)
                to_parsed <- parse_thurayya_coords(to_code)

                if (!is.null(from_parsed) && !is.null(to_parsed)) {
                  proxy <- proxy %>%
                    addPolylines(
                      lng = c(from_parsed$lon, to_parsed$lon),
                      lat = c(from_parsed$lat, to_parsed$lat),
                      color = route_color,
                      weight = 1.5,
                      opacity = 0.6,
                      popup = paste0("<strong>", scholar_name, "</strong><br/>",
                                    route$from, " → ", route$to),
                      group = "routes"
                    )
                  path_routes_drawn <- path_routes_drawn + 1
                }
              }
            }
            next  # Move to next route
          }

          # Fallback: draw straight line for route with no path data
          from_norm <- normalize_city_name(route$from %||% "")
          to_norm <- normalize_city_name(route$to %||% "")

          from_idx <- which(coords_lookup$city_norm == from_norm)
          to_idx <- which(coords_lookup$city_norm == to_norm)

          if (length(from_idx) == 0) {
            from_idx <- which(startsWith(coords_lookup$city_norm, substr(from_norm, 1, 4)))
          }
          if (length(to_idx) == 0) {
            to_idx <- which(startsWith(coords_lookup$city_norm, substr(to_norm, 1, 4)))
          }

          if (length(from_idx) > 0 && length(to_idx) > 0) {
            proxy <- proxy %>%
              addPolylines(
                lng = c(coords_lookup$lon[from_idx[1]], coords_lookup$lon[to_idx[1]]),
                lat = c(coords_lookup$lat[from_idx[1]], coords_lookup$lat[to_idx[1]]),
                color = route_color,
                weight = 1.5,
                opacity = 0.6,
                dashArray = "5,5",  # Dashed line indicates fallback
                popup = paste0("<strong>", scholar_name, "</strong><br/>",
                              route$from, " → ", route$to, " (direct)"),
                group = "routes"
              )
            fallback_routes_drawn <- fallback_routes_drawn + 1
          }
        }
      } else {
        # No detailed routes - use places_sequence with straight lines (legacy fallback)
        for (j in 1:(length(places) - 1)) {
          from_name <- places[j]
          to_name <- places[j + 1]
          from_norm <- normalize_city_name(from_name)
          to_norm <- normalize_city_name(to_name)

          from_idx <- which(coords_lookup$city_norm == from_norm)
          to_idx <- which(coords_lookup$city_norm == to_norm)

          if (length(from_idx) == 0) {
            from_idx <- which(startsWith(coords_lookup$city_norm, substr(from_norm, 1, 4)))
          }
          if (length(to_idx) == 0) {
            to_idx <- which(startsWith(coords_lookup$city_norm, substr(to_norm, 1, 4)))
          }

          if (length(from_idx) > 0 && length(to_idx) > 0) {
            proxy <- proxy %>%
              addPolylines(
                lng = c(coords_lookup$lon[from_idx[1]], coords_lookup$lon[to_idx[1]]),
                lat = c(coords_lookup$lat[from_idx[1]], coords_lookup$lat[to_idx[1]]),
                color = route_color,
                weight = 1.5,
                opacity = 0.6,
                dashArray = "5,5",
                popup = paste0("<strong>", scholar_name, "</strong><br/>",
                              from_name, " → ", to_name, " (direct)"),
                group = "routes"
              )
            fallback_routes_drawn <- fallback_routes_drawn + 1
          }
        }
      }
    }

    message("Routes drawn: ", path_routes_drawn, " with paths, ", gis_sea_routes_drawn, " GIS coastal, ", fallback_routes_drawn, " fallback straight lines")
  })

  # Observer: Trigger initial route display when geographic data loads
  # This ensures the map shows pathways on first load rather than being blank
  observeEvent(rv$geo_data$scholar_routes, {
    req(rv$geo_data$scholar_routes)
    req(rv$geo_data$city_coords)

    # Use shinyjs::delay to ensure the map has rendered before drawing routes
    # This triggers after initial data load to populate the map with all routes
    shinyjs::delay(800, {
      routes <- rv$geo_data$scholar_routes
      coords <- rv$geo_data$city_coords
      sea_route_geoms <- rv$geo_data$sea_route_geometries

      # Create coordinate lookup with normalized names
      coords_lookup <- coords
      coords_lookup$city_norm <- sapply(coords_lookup$city, normalize_city_name)

      # Draw initial routes
      proxy <- leafletProxy("geo_travel_map") %>%
        clearGroup("routes")

      if (length(routes) == 0) return()

      # Draw routes for each scholar (initial load - show all)
      for (scholar_name in names(routes)) {
        scholar <- routes[[scholar_name]]
        places <- unlist(scholar$places_sequence)
        regionality <- scholar$regionality
        scholar_routes <- scholar$routes

        if (length(places) < 2) next

        route_color <- get_edge_color(regionality)

        if (!is.null(scholar_routes) && length(scholar_routes) > 0) {
          for (route in scholar_routes) {
            if (!is.null(route$path) && length(route$path) >= 2) {
              for (seg_i in 1:(length(route$path) - 1)) {
                from_code <- route$path[seg_i]
                to_code <- route$path[seg_i + 1]

                from_parsed <- parse_thurayya_coords(from_code)
                to_parsed <- parse_thurayya_coords(to_code)

                if (!is.null(from_parsed) && !is.null(to_parsed)) {
                  proxy <- proxy %>%
                    addPolylines(
                      lng = c(from_parsed$lon, to_parsed$lon),
                      lat = c(from_parsed$lat, to_parsed$lat),
                      color = route_color,
                      weight = 1.5,
                      opacity = 0.6,
                      popup = paste0("<strong>", scholar_name, "</strong><br/>",
                                    route$from, " → ", route$to),
                      group = "routes"
                    )
                }
              }
            }
          }
        }
      }
      message("Initial routes drawn on map load")
    })
  }, once = TRUE, ignoreNULL = TRUE)

  # Filtered authors reactive (applies all filters for dynamic stats)
  geo_filtered_authors <- reactive({
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(NULL)

    authors <- mobility_data$authors

    # Apply region filter
    if (!is.null(input$geo_region_filter) && input$geo_region_filter != "all") {
      reg <- input$geo_region_filter
      if (reg == "inter-regional") {
        authors <- authors[authors$inter_regional == 1 |
                          grepl("visits", authors$regionality, ignore.case = TRUE), ]
      } else if (reg == "maġrib") {
        authors <- authors[grepl("^maġrib", authors$regionality, ignore.case = TRUE), ]
      } else if (reg == "mašriq") {
        authors <- authors[grepl("^mašriq", authors$regionality, ignore.case = TRUE), ]
      }
    }

    # Apply mobility filter
    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors <- authors[authors$mobility_category == input$geo_mobility_filter, ]
    }

    # Apply century filter
    if (!is.null(input$geo_century_filter)) {
      authors <- authors[!is.na(authors$death_century) &
                         authors$death_century >= input$geo_century_filter[1] &
                         authors$death_century <= input$geo_century_filter[2], ]
    }

    # Apply subregions filter
    if (!is.null(input$geo_subregions_filter)) {
      authors <- authors[!is.na(authors$subregions_visited) &
                         authors$subregions_visited >= input$geo_subregions_filter[1] &
                         authors$subregions_visited <= input$geo_subregions_filter[2], ]
    }

    authors
  })

  # Filter summary inline
  output$geo_filter_summary_inline <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors)) return(NULL)
    total_data <- geo_mobility_data()
    total <- if (!is.null(total_data)) nrow(total_data$authors) else 0
    HTML(paste0("Showing <strong>", nrow(authors), "</strong> of ", total, " scholars"))
  })

  # Mobility statistic cards (reactive to filters)
  output$geo_stat_sedentary <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "—"))

    n <- sum(authors$mobility_category == "sedentary", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0

    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #666;", n),
      div(class = "geo-stat-label", "Sedentary"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% stayed in home subregion"))
    )
  })

  output$geo_stat_local <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "—"))

    n <- sum(authors$mobility_category == "local-traveler", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0

    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #56B4E9;", n),
      div(class = "geo-stat-label", "Local Travelers"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% visited 2 subregions"))
    )
  })

  output$geo_stat_extensive <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "—"))

    n <- sum(authors$mobility_category == "extensive-local", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0

    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #E69F00;", n),
      div(class = "geo-stat-label", "Extensive Local"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% visited 3+ subregions"))
    )
  })

  output$geo_stat_interregional <- renderUI({
    authors <- geo_filtered_authors()
    if (is.null(authors) || nrow(authors) == 0) return(div(class = "geo-stat-card", "—"))

    n <- sum(authors$mobility_category == "inter-regional", na.rm = TRUE)
    pct <- if (nrow(authors) > 0) round(100 * n / nrow(authors)) else 0

    div(class = "geo-stat-card", style = "text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px; border: 2px solid #CC79A7;",
      div(class = "geo-stat-number", style = "font-size: 2em; font-weight: bold; color: #CC79A7;", n),
      div(class = "geo-stat-label", "Inter-Regional"),
      p(style = "font-size: 0.85em; color: #888; margin-top: 5px;",
        paste0(pct, "% crossed Mašriq/Maġrib boundary"))
    )
  })

  # Key findings output
  output$geo_key_findings <- renderUI({
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(div("Loading findings..."))

    authors <- mobility_data$authors
    n_total <- nrow(authors)

    # Calculate key statistics
    n_sedentary <- sum(authors$mobility_category == "sedentary", na.rm = TRUE)
    n_interreg <- sum(authors$inter_regional == 1, na.rm = TRUE)

    # Most traveled scholar
    top_traveler <- authors[which.max(authors$subregions_visited), ]

    # Average subregions by inter-regional status
    avg_subs_home <- mean(authors$subregions_visited[authors$inter_regional == 0], na.rm = TRUE)
    avg_subs_inter <- mean(authors$subregions_visited[authors$inter_regional == 1], na.rm = TRUE)

    # Average distance by region
    mashriq_dist <- mean(authors$total_distance_km[authors$regionality == "mašriq"], na.rm = TRUE)
    maghrib_dist <- mean(authors$total_distance_km[authors$regionality == "maġrib"], na.rm = TRUE)

    div(
      tags$ul(style = "list-style-type: none; padding-left: 0;",
        tags$li(style = "margin-bottom: 10px;",
          icon("home", style = "color: #666;"), " ",
          tags$strong(paste0(round(100 * n_sedentary / n_total), "%")),
          " of scholars stayed in their home subregion (sedentary)"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("exchange-alt", style = "color: #CC79A7;"), " ",
          tags$strong(n_interreg),
          " scholars crossed the Mašriq/Maġrib boundary"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("trophy", style = "color: #E69F00;"), " ",
          "Most mobile: ", tags$strong(top_traveler$author_name),
          " (", top_traveler$subregions_visited, " subregions)"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("chart-bar", style = "color: #56B4E9;"), " ",
          "Inter-regional scholars visited ", tags$strong(round(avg_subs_inter, 1)),
          " subregions on average vs ", tags$strong(round(avg_subs_home, 1)), " for home-region scholars"
        ),
        tags$li(style = "margin-bottom: 10px;",
          icon("road", style = "color: #009E73;"), " ",
          "Avg travel distance: Mašriq ", tags$strong(format(round(mashriq_dist), big.mark = ",")), " km",
          " | Maġrib ", tags$strong(format(round(maghrib_dist), big.mark = ",")), " km"
        )
      )
    )
  })

  # Author search match - using elastic search (supports Arabic, digraphs, transliteration)
  output$geo_author_match <- renderUI({
    search_term <- input$geo_author_search
    if (is.null(search_term) || nchar(search_term) < 2) return(NULL)

    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(NULL)

    authors <- mobility_data$authors

    # Use elastic search matching (same as corpus explorer)
    # Supports Arabic script, digraphs (dh, gh, sh, th, kh), and simplified Latin
    match_idx <- elastic_match_vec(search_term, authors$author_name, authors$author_name_arabic)
    matches <- authors[match_idx, ]

    if (nrow(matches) == 0) {
      return(div(style = "padding: 10px; color: #888;", "No matches found"))
    }

    # Show first match details with formatted name
    match <- matches[1, ]
    formatted_name <- format_camel_case(match$author_name, "author")

    div(style = "padding: 10px; background: #f8f9fa; border-radius: 4px; margin-top: 5px;",
      tags$strong(formatted_name), br(),
      "Home: ", match$home_subregion, " | ",
      "Subregions: ", match$subregions_visited, " | ",
      "Category: ", match$mobility_category,
      if (!is.na(match$total_distance_km) && match$total_distance_km > 0) {
        paste0(" | Distance: ", format(round(match$total_distance_km), big.mark = ","), " km")
      },
      if (nrow(matches) > 1) {
        tags$small(style = "display: block; margin-top: 5px; color: #666;",
          paste0("+ ", nrow(matches) - 1, " more match", ifelse(nrow(matches) > 2, "es", "")))
      }
    )
  })

  # Author mobility data table
  output$geo_author_mobility_table <- renderDT({
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return(NULL)

    authors <- mobility_data$authors

    # Apply author search filter using elastic match (same as corpus explorer)
    if (!is.null(input$geo_author_search) && nchar(input$geo_author_search) >= 2) {
      match_idx <- elastic_match_vec(input$geo_author_search, authors$author_name, authors$author_name_arabic)
      authors <- authors[match_idx, ]
    }

    # Apply dropdown filters
    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors <- authors[authors$mobility_category == input$geo_mobility_filter, ]
    }

    if (!is.null(input$geo_home_subregion) && input$geo_home_subregion != "all") {
      authors <- authors[authors$home_subregion == input$geo_home_subregion, ]
    }

    if (!is.null(input$geo_century_filter)) {
      authors <- authors[!is.na(authors$death_century) &
                         authors$death_century >= input$geo_century_filter[1] &
                         authors$death_century <= input$geo_century_filter[2], ]
    }

    # Format for display with proper name capitalization
    # Note: Inter-regional status is shown in Category column (mobility_category)
    # For Bayesian analysis, only the origin region matters (home region from regionality)
    display_df <- authors %>%
      mutate(
        Author = sapply(author_name, function(x) format_camel_case(x, "author")),
        Century = death_century,
        # Capitalize Origin (regionality): mašriq -> Mašriq, maġrib visits mašriq -> Maġrib Visits Mašriq
        Origin = sapply(regionality, function(r) {
          if (is.na(r) || r == "") return("—")
          # Split by space and capitalize each word
          words <- strsplit(r, " ")[[1]]
          paste(sapply(words, function(w) {
            paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
          }), collapse = " ")
        }),
        Subregions = subregions_visited,
        `Distance (km)` = ifelse(is.na(total_distance_km) | total_distance_km == 0, "—",
                                  format(round(total_distance_km), big.mark = ",")),
        Category = mobility_category
      ) %>%
      select(Author, Century, Origin, Subregions, `Distance (km)`, Category)

    datatable(
      display_df,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "compact stripe",
      selection = "single"  # Enable single row selection to connect to map
    )
  })

  # Observer: When a row is selected in the mobility table, update author search to show that scholar on map
  observeEvent(input$geo_author_mobility_table_rows_selected, {
    selected_row <- input$geo_author_mobility_table_rows_selected
    if (is.null(selected_row) || length(selected_row) == 0) return()

    # Get the mobility data and apply same filters as the table
    mobility_data <- geo_mobility_data()
    if (is.null(mobility_data)) return()

    authors <- mobility_data$authors

    # Apply same filters as the table rendering
    if (!is.null(input$geo_mobility_filter) && input$geo_mobility_filter != "all") {
      authors <- authors[authors$mobility_category == input$geo_mobility_filter, ]
    }
    if (!is.null(input$geo_home_subregion) && input$geo_home_subregion != "all") {
      authors <- authors[authors$home_subregion == input$geo_home_subregion, ]
    }
    if (!is.null(input$geo_century_filter)) {
      authors <- authors[!is.na(authors$death_century) &
                         authors$death_century >= input$geo_century_filter[1] &
                         authors$death_century <= input$geo_century_filter[2], ]
    }

    # Get the author name from the selected row
    if (selected_row <= nrow(authors)) {
      selected_author <- authors$author_name[selected_row]
      # Update the author search field to show this scholar's route
      updateTextInput(session, "geo_author_search", value = selected_author)
    }
  })

  # Reset filters
  observeEvent(input$geo_reset, {
    updateSelectInput(session, "geo_mobility_filter", selected = "all")
    updateSelectInput(session, "geo_meta_region", selected = "all")
    updateSelectInput(session, "geo_home_subregion", selected = "all")
    updateSliderInput(session, "geo_century_filter", value = c(4, 7))
    updateSliderInput(session, "geo_subregions_filter", value = c(1, 7))
    updateTextInput(session, "geo_author_search", value = "")
  })

  # NOTE: Geographic Batch Validation Panel removed for cloud deployment
  # Validation features are available in the local development version only

  # Filter summary showing count of matching scholars
  output$geo_filter_summary <- renderUI({
    routes <- geo_filtered_routes()
    n_routes <- length(routes)
    n_total <- length(rv$geo_data$scholar_routes)

    if (n_routes == n_total) {
      div(style = "padding: 10px; margin-top: 25px;",
        tags$strong(n_routes), " scholars shown (all)"
      )
    } else {
      div(style = "padding: 10px; margin-top: 25px; color: #0072B2;",
        tags$strong(n_routes), " of ", n_total, " scholars match filters"
      )
    }
  })

  # Helper function to calculate regional stats from DATABASE (author_places table)
  # This uses 133 authors with 348 place records, not just 57 with itineraries
  calculate_thurayya_stats <- function(routes, city_coords) {
    tryCatch({
      con <- dbConnect(SQLite(), DB_PATH)
      on.exit(dbDisconnect(con))

      # Get author_places with author death century
      author_places <- dbGetQuery(con, "
        SELECT ap.author_id, ap.place_name, a.death_century
        FROM author_places ap
        LEFT JOIN authors a ON ap.author_id = a.author_id
      ")

      if (nrow(author_places) == 0 || is.null(city_coords) || nrow(city_coords) == 0) {
        return(NULL)
      }

      # Create normalized city-to-region lookup
      region_lookup <- setNames(city_coords$region, sapply(city_coords$city, normalize_city_name))

      # Normalize author place names and match to regions
      author_places$place_norm <- sapply(author_places$place_name, normalize_city_name)
      author_places$region <- region_lookup[author_places$place_norm]

      # Filter to matched places
      matched <- author_places[!is.na(author_places$region), ]

      if (nrow(matched) == 0) return(NULL)

      # Aggregate by region
      all_regions <- unique(matched$region)
      region_stats <- lapply(all_regions, function(region) {
        region_data <- matched[matched$region == region, ]
        unique_authors <- unique(region_data$author_id)
        centuries <- region_data$death_century[match(unique_authors, region_data$author_id)]
        centuries <- centuries[!is.na(centuries)]

        century_counts <- if (length(centuries) > 0) {
          table(factor(centuries, levels = 4:9))
        } else {
          table(factor(integer(0), levels = 4:9))
        }

        list(
          region = region,
          total = length(unique_authors),
          by_century = as.list(century_counts)
        )
      })
      names(region_stats) <- all_regions
      region_stats
    }, error = function(e) {
      message("Error calculating Thurayya stats: ", e$message)
      NULL
    })
  }
  } # end if (FALSE) - Geographic Explorer

  # ============================================================================
  # SEA ROUTES VISUALIZATION - Removed (was integrated into Geographic Explorer)
  # ============================================================================
  if (FALSE) {
  sea_routes_selected <- reactive({
    c(input$sea_routes_western,
      input$sea_routes_nafr,
      input$sea_routes_strait)
  })

  # Quick selection handlers
  observeEvent(input$sea_routes_all, {
    updateCheckboxGroupInput(session, "sea_routes_western",
      selected = c("almeria_ibiza", "granada_ibiza", "valencia_ibiza", "ibiza_bajaya"))
    updateCheckboxGroupInput(session, "sea_routes_nafr",
      selected = c("bajaya_tunis", "tunis_mahdia", "mahdia_djerba",
                   "djerba_tripoli", "tripoli_barqa", "barqa_alexandria"))
    updateCheckboxGroupInput(session, "sea_routes_strait",
      selected = c("ceuta_tangier", "sicily_malta", "malta_tunis", "malta_djerba", "malta_tripoli", "malta_barqa"))
  })

  observeEvent(input$sea_routes_none, {
    updateCheckboxGroupInput(session, "sea_routes_western", selected = character(0))
    updateCheckboxGroupInput(session, "sea_routes_nafr", selected = character(0))
    updateCheckboxGroupInput(session, "sea_routes_strait", selected = character(0))
  })

  # Helper: Get route category and color
  get_sea_route_style <- function(route_key) {
    # Ibiza Hub / Bajaya Sea Lane (blue)
    if (route_key %in% c("almeria_ibiza", "granada_ibiza",
                         "valencia_ibiza", "ibiza_bajaya")) {
      return(list(color = "#0072B2", category = "Ibiza Hub (Bajaya Lane)"))
    # Ifriqiya Coastal Lane (green)
    } else if (route_key %in% c("bajaya_tunis", "tunis_mahdia",
                                 "mahdia_djerba", "djerba_tripoli",
                                 "tripoli_barqa", "barqa_alexandria")) {
      return(list(color = "#009E73", category = "Ifriqiya Coastal"))
    # Malta Hub & Crossings (sky blue)
    } else if (route_key %in% c("ceuta_tangier", "sicily_malta",
                                 "malta_tunis", "malta_djerba",
                                 "malta_tripoli", "malta_barqa")) {
      return(list(color = "#56B4E9", category = "Malta Hub / Crossing"))
    }
    return(list(color = "#666666", category = "Unknown"))
  }

  # Base map rendering
  output$sea_routes_map <- renderLeaflet({
    ports <- rv$geo_data$mediterranean_ports

    # Create base map centered on Mediterranean
    map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels,
                       options = providerTileOptions(opacity = 0.6)) %>%
      setView(lng = 12, lat = 35, zoom = 4) %>%
      addScaleBar(position = "bottomleft")

    # Add port markers if available
    if (!is.null(ports) && nrow(ports) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = ports,
          lng = ~lon, lat = ~lat,
          radius = 3,  # 70% smaller than original 8
          color = "#333333",
          fillColor = "#E69F00",
          fillOpacity = 0.9,
          weight = 2,
          popup = ~paste0("<strong>", tools::toTitleCase(name), "</strong><br/>",
                         "<em>Port City</em>"),
          label = ~tools::toTitleCase(name),
          labelOptions = labelOptions(
            noHide = FALSE, direction = "top",
            style = list("font-size" = "11px", "font-weight" = "bold")
          ),
          group = "ports"
        )
    }

    # Add legend
    map <- map %>%
      addLegend(
        position = "bottomright",
        colors = c("#0072B2", "#009E73", "#D55E00", "#56B4E9", "#E69F00", "#FFFFFF"),
        labels = c("Western Mediterranean", "North African Coast",
                   "Long Distance", "Strait Crossing", "Port City", "Waypoint"),
        title = "Route Types",
        opacity = 0.9
      )

    map
  })

  # Dynamic route rendering based on selections
  observe({
    selected <- sea_routes_selected()
    gis_routes <- rv$geo_data$gis_sea_routes

    if (is.null(gis_routes)) return()

    proxy <- leafletProxy("sea_routes_map") %>%
      clearGroup("routes") %>%
      clearGroup("waypoints")

    if (length(selected) == 0) return()

    # Draw each selected route
    for (route_key in selected) {
      route_data <- gis_routes[[route_key]]
      if (is.null(route_data)) next

      style <- get_sea_route_style(route_key)
      coords <- route_data$coordinates

      # Format names for display
      from_name <- tools::toTitleCase(route_data$from)
      to_name <- tools::toTitleCase(route_data$to)
      n_waypoints <- nrow(coords)

      # Add route polyline
      proxy <- proxy %>%
        addPolylines(
          lng = coords[, 1],
          lat = coords[, 2],
          color = style$color,
          weight = 4,
          opacity = 0.8,
          popup = paste0(
            "<strong>", from_name, " → ", to_name, "</strong><br/>",
            "<em>", style$category, "</em><br/>",
            "Distance: ", round(route_data$distance_km, 1), " km<br/>",
            "Waypoints: ", n_waypoints
          ),
          group = "routes",
          layerId = paste0("route_", route_key)
        )

      # Add waypoint markers (hollow circles for intermediate waypoints)
      if (n_waypoints > 2) {
        intermediate <- coords[2:(n_waypoints - 1), , drop = FALSE]

        proxy <- proxy %>%
          addCircleMarkers(
            lng = intermediate[, 1],
            lat = intermediate[, 2],
            radius = 2,  # 70% smaller than original 5
            color = style$color,
            fillColor = "#FFFFFF",
            fillOpacity = 0.8,
            weight = 2,
            popup = paste0("Waypoint on ", from_name, " → ", to_name, " route"),
            group = "waypoints"
          )
      }
    }
  })

  # Route details table
  output$sea_routes_table <- renderDT({
    selected <- sea_routes_selected()
    gis_routes <- rv$geo_data$gis_sea_routes

    if (is.null(gis_routes) || length(selected) == 0) {
      return(datatable(data.frame(Message = "No routes selected"),
                      options = list(dom = 't'), rownames = FALSE))
    }

    # Build table data
    route_data <- lapply(selected, function(route_key) {
      route <- gis_routes[[route_key]]
      if (is.null(route)) return(NULL)

      style <- get_sea_route_style(route_key)
      n_waypoints <- if (!is.null(route$coordinates)) nrow(route$coordinates) else 0

      data.frame(
        From = tools::toTitleCase(route$from),
        To = tools::toTitleCase(route$to),
        Category = style$category,
        Distance_km = round(route$distance_km, 1),
        Waypoints = n_waypoints,
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, Filter(Negate(is.null), route_data))

    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No route data available"),
                      options = list(dom = 't'), rownames = FALSE))
    }

    # Sort by distance
    df <- df[order(-df$Distance_km), ]

    # Add color coding
    df$Category <- sapply(df$Category, function(cat) {
      color <- switch(cat,
        "Western Mediterranean" = "#0072B2",
        "North African Coastal" = "#009E73",
        "Long Distance" = "#D55E00",
        "Strait Crossing" = "#56B4E9",
        "#666666"
      )
      sprintf('<span style="background-color: %s; color: white; padding: 2px 8px; border-radius: 4px;">%s</span>',
              color, cat)
    })

    datatable(
      df,
      escape = FALSE,
      colnames = c("From", "To", "Category", "Distance (km)", "Waypoints"),
      options = list(
        pageLength = 10,
        dom = 'tip',
        order = list(list(3, 'desc'))
      ),
      rownames = FALSE
    )
  })

  # Summary statistics cards
  output$sea_stat_total_routes <- renderUI({
    n <- length(sea_routes_selected())
    div(class = "status-badge", style = "background-color: #e7f3ff; width: 100%;",
        h4(n, style = "margin: 0; color: #0072B2;"),
        p("Routes Selected", style = "margin: 0; font-size: 0.9em;"))
  })

  output$sea_stat_total_distance <- renderUI({
    selected <- sea_routes_selected()
    gis_routes <- rv$geo_data$gis_sea_routes

    total_km <- sum(sapply(selected, function(key) {
      route <- gis_routes[[key]]
      if (!is.null(route)) route$distance_km else 0
    }))

    div(class = "status-badge", style = "background-color: #e7f3ff; width: 100%;",
        h4(format(round(total_km), big.mark = ","), "km", style = "margin: 0; color: #009E73;"),
        p("Total Distance", style = "margin: 0; font-size: 0.9em;"))
  })

  output$sea_stat_avg_waypoints <- renderUI({
    selected <- sea_routes_selected()
    gis_routes <- rv$geo_data$gis_sea_routes

    if (length(selected) == 0) {
      avg <- 0
    } else {
      waypoints <- sapply(selected, function(key) {
        route <- gis_routes[[key]]
        if (!is.null(route) && !is.null(route$coordinates)) nrow(route$coordinates) else 0
      })
      avg <- round(mean(waypoints), 1)
    }

    div(class = "status-badge", style = "background-color: #e7f3ff; width: 100%;",
        h4(avg, style = "margin: 0; color: #D55E00;"),
        p("Avg Waypoints/Route", style = "margin: 0; font-size: 0.9em;"))
  })

  output$sea_stat_longest_route <- renderUI({
    selected <- sea_routes_selected()
    gis_routes <- rv$geo_data$gis_sea_routes

    if (length(selected) == 0) {
      longest <- "None selected"
    } else {
      max_dist <- 0
      max_route <- ""
      for (key in selected) {
        route <- gis_routes[[key]]
        if (!is.null(route) && route$distance_km > max_dist) {
          max_dist <- route$distance_km
          max_route <- paste0(tools::toTitleCase(route$from), " → ", tools::toTitleCase(route$to))
        }
      }
      longest <- paste0(max_route, " (", round(max_dist), " km)")
    }

    div(class = "status-badge", style = "background-color: #e7f3ff; width: 100%;",
        h4(longest, style = "margin: 0; color: #56B4E9; font-size: 1em;"),
        p("Longest Selected", style = "margin: 0; font-size: 0.9em;"))
  })
  } # end if (FALSE) - Sea Routes

  # ============================================================================
  # BAYESIAN ANALYSIS - Card Navigation
  # ============================================================================

  # Card navigation buttons
  observeEvent(input$bayes_prev, {
    if (rv$bayes_current_card > 1) {
      rv$bayes_current_card <- rv$bayes_current_card - 1
    }
  })

  observeEvent(input$bayes_next, {
    if (rv$bayes_current_card < 4) {
      rv$bayes_current_card <- rv$bayes_current_card + 1
    }
  })

  # Next buttons within cards (same functionality as bayes_next)
  observeEvent(input$bayes_next_from_card1, {
    rv$bayes_current_card <- 2
  })

  observeEvent(input$bayes_next_from_card2, {
    rv$bayes_current_card <- 3
  })

  observeEvent(input$bayes_next_from_card3, {
    rv$bayes_current_card <- 4
  })

  # Card indicator
  output$bayes_card_indicator <- renderUI({
    span(class = "card-indicator",
      sprintf("Card %d of 4", rv$bayes_current_card)
    )
  })

  # Card content renderer
  output$bayes_current_card <- renderUI({
    card_num <- rv$bayes_current_card

    switch(card_num,
      # Card 1: Introduction
      render_bayes_card_1(),
      # Card 2: Model Configuration + Parameters
      render_bayes_card_2(),
      # Card 3: Predictions
      render_bayes_card_3(),
      # Card 4: Contrasts + Diagnostics
      render_bayes_card_4()
    )
  })

  # Card 1: Introduction
  render_bayes_card_1 <- function() {
    div(class = "card",
      div(class = "card-header", "Introduction to Bayesian Analysis"),
      div(class = "card-body",
        h4("Why Bayesian?"),
        p("Bayesian analysis provides a principled framework for quantifying uncertainty in our estimates. Unlike traditional frequentist approaches that give point estimates with confidence intervals, Bayesian methods produce full probability distributions that directly answer questions like: 'Given the data, what is the probability that regional affiliation influenced reading system choice?'"),

        h4("Research Questions"),
        tags$ol(
          tags$li("Did regional affiliation (Maġrib vs. Mašriq) influence which reading systems scholars chose to document?"),
          tags$li("How confident can we be in observed regional differences?"),
          tags$li("Did patterns of regional preference change over the study period (4th-7th centuries AH)?")
        ),

        h4("Key Advantages"),
        tags$ul(
          tags$li(tags$strong("Full posterior distributions"), " - Not just point estimates, but complete probability distributions for each parameter"),
          tags$li(tags$strong("Natural uncertainty quantification"), " - Credible intervals have intuitive interpretation: 'There is a 95% probability the true value lies in this range'"),
          tags$li(tags$strong("Small sample handling"), " - Bayesian methods perform well with limited data through principled use of prior information")
        ),

        div(class = "info-box",
          p(icon("info-circle"), " The model will fit automatically when you advance to the next card. This process takes 2-5 minutes.")
        ),
        div(style = "text-align: right; margin-top: 20px;",
          actionButton("bayes_next_from_card1", "Next \u2192", class = "btn-primary")
        )
      )
    )
  }

  # Card 2: Model Configuration + Parameters
  render_bayes_card_2 <- function() {
    div(class = "card",
      div(class = "card-header", "Model Configuration & Parameters"),
      div(class = "card-body",
        # Model understanding
        h4(class = "section-header-bold", "Understanding the Model"),
        p("This analysis uses Bayesian multinomial logistic regression to model the probability that a work describes a particular reading system (7, 7+1, or 10+) based on the author's regional location and the century of production."),

        tags$h5("Model Structure:"),
        tags$ul(
          tags$li(tags$strong("Outcome:"), " Reading set (7, 7+1, or 10+) - the dependent variable"),
          tags$li(tags$strong("Predictor:"), " Regional affiliation (Maġrib vs. Mašriq) - the main independent variable"),
          tags$li(tags$strong("Covariate:"), " Death century (4th-7th c. AH, centered) - temporal control variable")
        ),

        hr(),

        # Model configuration summary
        h4(class = "section-header-bold", "Current Configuration"),
        uiOutput("model_config_summary"),

        hr(),

        # Parameter estimates (only show if model fitted)
        conditionalPanel(
          condition = "output.model_fitted",
          h4(class = "section-header-bold", "Model Parameter Estimates"),
          div(class = "info-box",
            p("The table below shows the posterior distributions of the model's regression coefficients:"),
            tags$ul(
              tags$li(tags$strong("Alpha (Intercepts):"), " Baseline log-odds for each set vs. the reference (10+ readings)"),
              tags$li(tags$strong("Beta_geo (Regional Effects):"), " How being in Mašriq changes the log-odds. Positive = Mašriq preference; negative = Maġrib preference."),
              tags$li(tags$strong("Beta_cent (Temporal Effects):"), " How each additional century changes the log-odds.")
            ),
            p("When the 95% credible interval (q5, q95) excludes zero, we have strong evidence for that effect.")
          ),
          verbatimTextOutput("model_summary")
        ),

        conditionalPanel(
          condition = "!output.model_fitted",
          div(class = "alert alert-info",
            icon("hourglass-half"),
            " Model fitting in progress or not yet started. Advance to the next card to trigger model fitting.")
        ),
        div(style = "text-align: right; margin-top: 20px;",
          actionButton("bayes_next_from_card2", "Next \u2192", class = "btn-primary")
        )
      )
    )
  }

  # Card 3: Predictions
  render_bayes_card_3 <- function() {
    div(class = "card",
      div(class = "card-header", "Model Predictions"),
      div(class = "card-body",
        conditionalPanel(
          condition = "output.model_fitted",
          h4(class = "section-header-bold", "Predicted Probabilities by Region and Century"),
          p("Hover over the bars to see exact probability values and credible intervals."),

          # Info box moved ABOVE the plot
          div(class = "info-box",
            tags$h5("Reading the Plot:"),
            tags$ul(
              tags$li("Each panel represents a different century (4th through 7th AH)"),
              tags$li("Bar height shows the predicted probability (0-1 scale)"),
              tags$li("Colors distinguish regions: ", span(style = "color: #56B4E9;", "Maġrib"), " vs. ", span(style = "color: #E69F00;", "Mašriq")),
              tags$li("Error bars show 95% credible intervals - ranges of plausible values")
            ),
            p(tags$strong("Interpreting differences:"), " When error bars for Maġrib and Mašriq do not overlap for a given set, this indicates strong evidence of regional differences in pedagogical preferences.")
          ),

          hr(),

          div(class = "plotly-container",
            plotlyOutput("pred_plot_interactive", height = "500px")
          )
        ),

        conditionalPanel(
          condition = "!output.model_fitted",
          div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Please wait for the model to finish fitting. This typically takes 2-5 minutes.")
        ),
        div(style = "text-align: right; margin-top: 20px;",
          actionButton("bayes_next_from_card3", "Next \u2192", class = "btn-primary")
        )
      )
    )
  }

  # Card 4: Contrasts + Diagnostics
  render_bayes_card_4 <- function() {
    div(class = "card",
      div(class = "card-header", "Posterior Contrast Analysis & Diagnostics"),
      div(class = "card-body",
        conditionalPanel(
          condition = "output.model_fitted",

          # Reading Set Selector at TOP
          h4(class = "section-header-bold", "Posterior Contrast Analysis"),
          p("Select a reading set to compare probabilities between regions:"),

          fluidRow(
            column(4,
              selectInput("selected_system_card4",
                         "Select Reading Set:",
                         choices = c("7", "7+1", "10+"),
                         selected = "7",
                         width = "100%")
            )
          ),

          # Contrast results
          uiOutput("contrast_display"),

          hr(),

          # Diagnostics at BOTTOM
          h4(class = "section-header-bold", "Model Diagnostics"),

          tabsetPanel(
            id = "diag_tabs_card4",

            tabPanel(
              "Convergence",
              br(),
              fluidRow(
                column(6,
                  h5(class = "section-header-bold", "R-hat Diagnostic"),
                  p("Values < 1.01 indicate convergence", style = "color: gray;"),
                  plotOutput("diag_rhat", height = "280px")),
                column(6,
                  h5(class = "section-header-bold", "Effective Sample Size"),
                  p("ESS > 400 recommended for reliable inference", style = "color: gray;"),
                  plotOutput("diag_ess", height = "280px"))
              ),
              br(),
              h5(class = "section-header-bold", "Diagnostic Summary"),
              uiOutput("diag_summary_table")
            ),

            tabPanel(
              "Trace Plots",
              br(),
              h5(class = "section-header-bold", "MCMC Chain Mixing"),
              p("Good mixing shows 'hairy caterpillar' pattern with chains overlapping", style = "color: gray;"),
              plotOutput("diag_trace", height = "500px")
            ),

            tabPanel(
              "Posteriors",
              br(),
              h5(class = "section-header-bold", "Posterior Distributions by Chain"),
              p("Click on any parameter plot to enlarge with detailed hover statistics.", style = "color: gray;"),
              uiOutput("posteriors_grid"),
              br(),
              h5(class = "section-header-bold", "Chain Information Content"),
              p("Relative contribution of each chain to the posterior estimates.", style = "color: gray; font-size: 0.9em;"),
              uiOutput("chain_info_stats")
            ),

            tabPanel(
              "Correlations",
              br(),
              h5(class = "section-header-bold", "Parameter Correlations"),
              p("High correlations between α and β_geo are structural (expected); very high correlations (>0.95) elsewhere may indicate identifiability issues", style = "color: gray;"),
              plotOutput("diag_correlation", height = "450px")
            ),

            tabPanel(
              "Predictive Check",
              br(),
              h5(class = "section-header-bold", "Posterior Predictive Check"),
              p("Vertical line = observed count; histogram = simulated from posterior. Observed values should fall within the bulk of the distribution.", style = "color: gray;"),
              plotlyOutput("diag_ppc", height = "450px"),
              p("Hover over histograms for detailed statistics. Vertical lines show observed counts.", style = "color: gray; font-size: 0.85em; margin-top: 10px;")
            ),

            tabPanel(
              "Temporal Fit",
              br(),
              h5(class = "section-header-bold", "Temporal Predictions with Uncertainty"),
              p("Posterior predictions across centuries with uncertainty visualization", style = "color: gray;"),
              plotOutput("diag_temporal", height = "500px")
            )
          )
        ),

        conditionalPanel(
          condition = "!output.model_fitted",
          div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Please wait for the model to finish fitting before viewing contrasts and diagnostics.")
        )
      )
    )
  }

  # Interactive prediction plot (plotly version)
  output$pred_plot_interactive <- renderPlotly({
    preds <- posterior_preds()
    req(preds)

    pp <- preds$pp
    K <- preds$K
    lvl <- preds$levels
    centuries <- preds$centuries

    plot_data_list <- list()
    for (c_idx in 1:length(centuries)) {
      cent <- centuries[c_idx]
      mean_probs <- apply(pp[, , c_idx, ], c(2, 3), mean)
      lower <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 0.025)
      upper <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 0.975)

      df_cent <- data.frame(
        Category = rep(lvl, times = 2),
        Region = rep(c("Maġrib", "Mašriq"), each = K),
        Century = paste0(cent, "th c."),
        mean = as.vector(t(mean_probs)),
        low = as.vector(t(lower)),
        high = as.vector(t(upper))
      )
      plot_data_list[[c_idx]] <- df_cent
    }
    df_plot <- do.call(rbind, plot_data_list)
    df_plot$Century <- factor(df_plot$Century, levels = paste0(centuries, "th c."))
    # Display Mašriq first (left), then Maġrib
    df_plot$Region <- factor(df_plot$Region, levels = c("Mašriq", "Maġrib"))
    # Ensure correct category ordering (not alphabetical)
    df_plot$Category <- factor(df_plot$Category, levels = c("7", "7+1", "10+"))

    # Create hover text
    df_plot$hover_text <- sprintf(
      "<b>%s</b><br>Region: %s<br>Century: %s<br>Probability: %.1f%%<br>95%% CI: [%.1f%%, %.1f%%]",
      df_plot$Category, df_plot$Region, df_plot$Century,
      df_plot$mean * 100, df_plot$low * 100, df_plot$high * 100
    )

    p <- ggplot(df_plot, aes(x = Category, y = mean, fill = Region, text = hover_text)) +
      geom_col(position = position_dodge(0.8), width = 0.7, color = NA, alpha = 0.7) +
      geom_errorbar(aes(ymin = low, ymax = high),
                    position = position_dodge(0.8), width = 0.15, linewidth = 0.4, color = "gray30") +
      facet_wrap(~ Century, ncol = 2) +
      scale_fill_manual(values = c("Maġrib" = "#56B4E9", "Mašriq" = "#E69F00")) +
      scale_x_discrete(limits = c("7", "7+1", "10+")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                        labels = scales::percent_format(accuracy = 1)) +
      theme_tufte_custom(base_size = 11) +
      labs(title = "Predicted Probabilities by Century", y = "Probability", x = NULL, fill = NULL) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "bottom", legend.direction = "horizontal",
            panel.spacing = unit(1, "lines"))

    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        xaxis = list(categoryorder = "array", categoryarray = c("7", "7+1", "10+")),
        xaxis2 = list(categoryorder = "array", categoryarray = c("7", "7+1", "10+")),
        xaxis3 = list(categoryorder = "array", categoryarray = c("7", "7+1", "10+")),
        xaxis4 = list(categoryorder = "array", categoryarray = c("7", "7+1", "10+"))
      )
  })

  # Tab navigation observer for automatic computation ====
  observeEvent(input$tabs, {

    # Bayesian Analysis tab: Check for pre-computed results on first visit
    if (input$tabs == "bayesian_analysis" && !rv$bayesian_analysis_visited) {
      rv$bayesian_analysis_visited <- TRUE

      if (!is.null(rv$fit_obj)) {
        # Model already loaded from pre-computed results
        showNotification(
          "Pre-computed Bayesian model results loaded",
          type = "message",
          duration = 3
        )
      } else if (!is.null(rv$clean_data)) {
        # No pre-computed results - would need cmdstanr (disabled for cloud)
        showNotification(
          "Pre-computed model results not found. Please ensure precomputed_bayesian_results.rds is in data/ folder.",
          type = "warning",
          duration = 10
        )
      }
    }

    # Legacy support for old tab names
    if (input$tabs == "model_summary" && !rv$model_summary_visited) {
      rv$model_summary_visited <- TRUE

      if (!is.null(rv$fit_obj)) {
        # Model already loaded
        showNotification(
          "Pre-computed Bayesian model results loaded",
          type = "message",
          duration = 3
        )
      }
    }

    # Analysis Results tab: Auto-compute contrasts on first visit
    if (input$tabs == "analysis_results" && !rv$analysis_results_visited) {
      rv$analysis_results_visited <- TRUE

      if (!rv$contrasts_computed && !is.null(rv$fit_obj)) {
        showNotification(
          "Computing posterior contrasts for all systems...",
          type = "message",
          duration = NULL,
          id = "auto_contrasts"
        )

        compute_all_contrasts()

        removeNotification(id = "auto_contrasts")
        showNotification(
          "✓ All contrasts computed! Select a system to view results.",
          type = "message",
          duration = 5
        )
      }
    }
  })

  # Auto-fit function (disabled for cloud deployment - uses pre-computed results)
  trigger_auto_fit <- function() {
    # For cloud deployment, we use pre-computed results instead of live MCMC
    # The rv$fit_obj is pre-loaded from PRECOMPUTED at startup

    if (!is.null(rv$fit_obj)) {
      showNotification(
        "Pre-computed Bayesian model results already loaded.",
        type = "message",
        duration = 3
      )
      return()
    }

    # If no pre-computed results, show error (cmdstanr disabled for cloud)
    showNotification(
      HTML(paste0(
        "<strong>Model Not Available</strong><br/>",
        "Pre-computed results not found. Live MCMC is disabled for cloud deployment.<br/>",
        "Please ensure data/precomputed_bayesian_results.rds exists."
      )),
      type = "error",
      duration = NULL
    )
  }

  # Compute all contrasts (called by tab navigation) - uses pre-computed if available
  compute_all_contrasts <- function() {
    req(rv$fit_obj, rv$fit_info)

    pre <- rv$fit_obj

    # Use pre-computed contrasts if available
    if (!is.null(pre$contrasts)) {
      for (system in names(pre$contrasts)) {
        rv$contrast_results[[system]] <- pre$contrasts[[system]]
      }
      rv$contrasts_computed <- TRUE
      return()
    }

    # Fallback: compute on the fly (shouldn't happen with pre-computed data)
    preds <- posterior_preds()
    req(preds)

    for (system in preds$levels) {
      rv$contrast_results[[system]] <- compute_single_contrast(preds, system)
    }

    rv$contrasts_computed <- TRUE
  }

  # Helper: Compute single contrast
  compute_single_contrast <- function(preds, target_system) {
    k_idx <- which(preds$levels == target_system)
    pp <- preds$pp

    if (preds$has_century) {
      # Average over all centuries
      p_east <- apply(pp[, 2, , k_idx, drop = FALSE], 1, mean)
      p_west <- apply(pp[, 1, , k_idx, drop = FALSE], 1, mean)
    } else {
      p_east <- pp[, 2, k_idx]
      p_west <- pp[, 1, k_idx]
    }

    diff <- p_east - p_west
    prob_east_greater <- mean(diff > 0)

    list(
      system = target_system,
      p_east = p_east,
      p_west = p_west,
      diff = diff,
      prob_east_greater = prob_east_greater,
      mean_diff = mean(diff),
      ci_lower = quantile(diff, 0.025),
      ci_upper = quantile(diff, 0.975)
    )
  }

  # Save model
  output$save_model <- downloadHandler(
    filename = function() {
      paste0("bayesian_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      req(rv$fit_obj)
      saveRDS(rv$fit_obj, file)
      showNotification("Model saved", type = "message")
    }
  )

  # Model configuration summary
  output$model_config_summary <- renderUI({
    req(rv$clean_data)
    df <- rv$clean_data

    div(class = "card",
      div(class = "card-body",
        tags$ul(
          tags$li(tags$strong("Type:"), " Bayesian Multinomial Logistic Regression"),
          tags$li(tags$strong("Outcome:"), " system (3 levels: 7, 7+1, 10+)"),
          tags$li(tags$strong("Predictor:"), " regionality (Maġrib, Mašriq)"),
          tags$li(tags$strong("Covariate:"), " death_century (mean-centered)"),
          tags$li(tags$strong("Observations:"), nrow(df)),
          tags$li(tags$strong("Fitting:"), " Automatic on first tab visit")
        )
      )
    )
  })

  # Model summary output
  output$model_summary <- renderPrint({
    req(rv$fit_obj)
    # Use pre-computed diagnostic stats for model summary
    pre <- rv$fit_obj
    if (!is.null(pre$diag_stats)) {
      pre$diag_stats
    } else {
      data.frame(message = "No summary available")
    }
  })

  # Convergence summary
  output$convergence_summary <- renderPrint({
    req(rv$fit_obj)
    pre <- rv$fit_obj
    if (!is.null(pre$diag_stats)) {
      cat("Convergence Diagnostics (R-hat):\n\n")
      print(pre$diag_stats[, c("variable", "rhat")])
    } else {
      cat("Diagnostic statistics not available\n")
    }
  })

  # Compute posterior predictions (reactive) - uses pre-computed data
  posterior_preds <- reactive({
    req(rv$fit_obj, rv$fit_info)

    pre <- rv$fit_obj  # This is PRECOMPUTED

    # Return pre-computed posterior predictions
    list(
      pp = pre$pp,
      S = pre$S,
      K = pre$fit_info$K,
      levels = pre$fit_info$levels,
      has_century = pre$has_century,
      centuries = pre$centuries
    )
  })

  # Prediction plot
  output$pred_plot <- renderPlot({
    preds <- posterior_preds()
    req(preds)

    pp <- preds$pp
    K <- preds$K
    lvl <- preds$levels
    centuries <- preds$centuries

    plot_data_list <- list()
    for (c_idx in 1:length(centuries)) {
      cent <- centuries[c_idx]
      mean_probs <- apply(pp[, , c_idx, ], c(2, 3), mean)
      lower <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 0.025)
      upper <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 0.975)

      df_cent <- data.frame(
        Category = rep(lvl, times = 2),
        Region = rep(c("maġrib", "mašriq"), each = K),
        Century = paste0(cent, "th c."),
        mean = as.vector(t(mean_probs)),
        low = as.vector(t(lower)),
        high = as.vector(t(upper))
      )
      plot_data_list[[c_idx]] <- df_cent
    }
    df_plot <- do.call(rbind, plot_data_list)
    df_plot$Century <- factor(df_plot$Century, levels = paste0(centuries, "th c."))
    df_plot$Category <- factor(df_plot$Category, levels = c("7", "7+1", "10+"))

    ggplot(df_plot, aes(x = Category, y = mean, fill = Region)) +
      # Minimal bars - no outline
      geom_col(position = position_dodge(0.8), width = 0.7,
               color = NA, alpha = 0.7) +
      # Thin error bars
      geom_errorbar(aes(ymin = low, ymax = high),
                    position = position_dodge(0.8), width = 0.15,
                    linewidth = 0.4, color = "gray30") +
      facet_wrap(~ Century, ncol = 2) +
      # Grayscale with subtle distinction
      scale_fill_manual(values = COLORS$region,
                        labels = c("Maġrib", "Mašriq")) +
      scale_x_discrete(limits = c("7", "7+1", "10+")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                        labels = scales::percent_format(accuracy = 1)) +
      theme_tufte_custom(base_size = 11) +
      labs(
        title = "Predicted Probabilities by Century",
        y = "Probability",
        x = NULL,  # Category is self-evident
        fill = NULL  # Legend title removed for minimal ink
      ) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.spacing = unit(1, "lines")
      )
  })

  # Prediction table
  output$pred_table <- renderTable({
    preds <- posterior_preds()
    req(preds)

    pp <- preds$pp
    K <- preds$K
    lvl <- preds$levels
    centuries <- preds$centuries
    table_list <- list()

    for (c_idx in 1:length(centuries)) {
      cent <- centuries[c_idx]
      mean_probs <- apply(pp[, , c_idx, ], c(2, 3), mean)
      lower <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 0.025)
      upper <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 0.975)

      df_cent <- data.frame(
        Century = paste0(cent, "th"),
        Region = rep(c("maġrib", "mašriq"), each = K),
        Category = rep(lvl, times = 2),
        MeanProb = round(as.vector(t(mean_probs)), 3),
        CI_low = round(as.vector(t(lower)), 3),
        CI_high = round(as.vector(t(upper)), 3)
      )
      table_list[[c_idx]] <- df_cent
    }
    do.call(rbind, table_list)
  })

  # Contrast display (dynamic based on selected system)
  output$contrast_display <- renderUI({
    req(rv$contrasts_computed, input$selected_system_card4)

    contrast <- rv$contrast_results[[input$selected_system_card4]]
    req(contrast)

    # Get system-specific explanation
    explanation <- get_system_explanation(input$selected_system_card4)

    div(class = "card",
      div(class = "card-header", style = "background-color: #17a2b8; color: white;",
        h4(style = "margin: 0;", paste("System:", input$selected_system_card4))
      ),
      div(class = "card-body",
        # Explanatory text
        div(class = "interpretation-text",
          HTML(explanation)
        ),

        # Results
        div(class = "results-card",
          h4("Regional Contrast Results"),
          tags$ul(
            tags$li(tags$strong(sprintf("P(%s|mašriq) > P(%s|maġrib):",
                                       contrast$system, contrast$system)),
                   sprintf(" %.3f", contrast$prob_east_greater)),
            tags$li(tags$strong("Mean difference (mašriq - maġrib):"),
                   sprintf(" %.3f", contrast$mean_diff)),
            tags$li(tags$strong("95% Credible Interval:"),
                   sprintf(" [%.3f, %.3f]", contrast$ci_lower, contrast$ci_upper))
          ),
          tags$hr(),
          p(tags$strong("Interpretation: "),
            get_interpretation(contrast$prob_east_greater, contrast$mean_diff, contrast$system))
        ),

        # Histogram
        plotOutput(paste0("contrast_plot_", gsub("\\+", "plus", input$selected_system_card4)),
                  height = "350px"),
        p(class = "text-muted", style = "font-size: 0.85em;",
          "The histogram shows the posterior distribution of the difference in probabilities. The dark vertical line marks the mean difference, while the gray line at zero represents no regional effect. The subtle gray shaded region indicates the 95% credible interval. When the distribution is clearly shifted away from zero, this provides evidence of systematic regional preference for this reading system.")
      )
    )
  })

  # Get system-specific explanation
  get_system_explanation <- function(system) {
    explanations <- list(
      "7" = "<h5>The Seven-Reading System and Regional Canons</h5>
<p>The seven-reading system represents Ibn Muǧāhid's (d. 324/936) canonical selection, which became the foundation of qirāʾāt pedagogy. Regional preferences for this system indicate adoption of Ibn Muǧāhid's framework as the standard pedagogical model.</p>
<p><strong>If Mašriq shows higher probability:</strong> This aligns with historical expectations, as Ibn Muǧāhid worked in Baghdad and the seven-reading system initially spread through Mašriqī scholarly networks.</p>
<p><strong>If Maġrib shows higher probability:</strong> This indicates that Maġribī scholars particularly embraced Ibn Muǧāhid's canonical framework, perhaps as a means of standardizing instruction.</p>",

      "7+1" = "<h5>The 7+1 System and Regional Variation</h5>
<p>The 7+1 system adds the reading of Yaʿqūb al-Ḥaḍramī (d. 205/821) to Ibn Muǧāhid's seven, representing an expansion of the canonical framework. Production patterns for this system reveal regional attitudes toward canonical boundaries and acceptable variation.</p>
<p><strong>If Mašriq shows higher probability:</strong> This suggests Mašriqī scholars were more willing to expand beyond Ibn Muǧāhid's original framework, perhaps reflecting continued engagement with pre-canonical reading traditions.</p>
<p><strong>If Maġrib shows higher probability:</strong> This is particularly significant, suggesting Maġribī scholars developed a distinct pedagogical tradition that systematically included Yaʿqūb's reading alongside the canonical seven, potentially indicating a Maġribī counter-canon.</p>",

      "10+" = "<h5>The 10+ System and Comprehensive Pedagogical Approaches</h5>
<p>Works describing ten or more reading traditions represent the most comprehensive approach to qirāʾāt instruction, often including the 'three additional' readings beyond the seven (Yaʿqūb, Ḫalaf, al-Ḥasan al-Baṣrī, etc.) or engaging with even broader reading traditions.</p>
<p><strong>If Mašriq shows higher probability:</strong> This suggests Mašriqī scholars maintained stronger interest in preserving and transmitting the full diversity of reading traditions beyond canonical selections, perhaps reflecting the region's role as the original site of reading tradition development.</p>
<p><strong>If Maġrib shows higher probability:</strong> This indicates that Maġribī scholars, despite geographic distance from the original centers of qirāʾāt development, sought comprehensive knowledge of reading traditions, possibly as a strategy for scholarly authority.</p>"
    )
    return(explanations[[system]])
  }

  # Get interpretation text
  get_interpretation <- function(prob_east_greater, mean_diff, system) {
    if (prob_east_greater > 0.95) {
      sprintf("Strong evidence that Mašriq has higher probability for the %s system across all centuries. The mean difference of %.3f indicates Mašriq scholars had substantially higher preference for this system.",
              system, mean_diff)
    } else if (prob_east_greater < 0.05) {
      sprintf("Strong evidence that Maġrib has higher probability for the %s system across all centuries. The mean difference of %.3f indicates Maġrib scholars had substantially higher preference for this system.",
              system, abs(mean_diff))
    } else if (prob_east_greater > 0.75) {
      sprintf("Moderate evidence that Mašriq has higher probability for the %s system (%.1f%% confidence). The mean difference of %.3f suggests a Mašriqī preference, but with some uncertainty.",
              system, prob_east_greater * 100, mean_diff)
    } else if (prob_east_greater < 0.25) {
      sprintf("Moderate evidence that Maġrib has higher probability for the %s system (%.1f%% confidence). The mean difference of %.3f suggests a Maġribī preference, but with some uncertainty.",
              system, (1 - prob_east_greater) * 100, abs(mean_diff))
    } else {
      sprintf("Weak or no evidence of regional difference for the %s system. The posterior probability is %.3f, suggesting relatively balanced preferences between regions.",
              system, prob_east_greater)
    }
  }

  # Contrast plots (one for each system)
  output$contrast_plot_7 <- renderPlot({
    req(rv$contrast_results[["7"]])
    plot_contrast_histogram(rv$contrast_results[["7"]])
  })

  output$contrast_plot_7plus1 <- renderPlot({
    req(rv$contrast_results[["7+1"]])
    plot_contrast_histogram(rv$contrast_results[["7+1"]])
  })

  output$contrast_plot_10plus <- renderPlot({
    req(rv$contrast_results[["10+"]])
    plot_contrast_histogram(rv$contrast_results[["10+"]])
  })

  # Helper function for histogram
  plot_contrast_histogram <- function(contrast_result) {
    df <- data.frame(diff = contrast_result$diff)

    # Get appropriate color based on system
    hist_color <- COLORS$system[contrast_result$system]
    if (is.na(hist_color)) hist_color <- COLORS$system["7"]

    # Create base plot with Tufte principles
    p <- ggplot(df, aes(x = diff)) +
      # Histogram colored by system
      geom_histogram(bins = 50, fill = hist_color, color = "white",
                     alpha = 0.75, linewidth = 0.25) +
      # Subtle shaded region for 95% credible interval
      annotate("rect",
               xmin = contrast_result$ci_lower,
               xmax = contrast_result$ci_upper,
               ymin = 0, ymax = Inf,
               fill = hist_color, alpha = 0.15) +
      # Thin reference lines - minimal ink
      geom_vline(xintercept = 0,
                 linetype = "solid", color = "gray40", linewidth = 0.5) +
      geom_vline(xintercept = contrast_result$mean_diff,
                 color = "gray10", linewidth = 0.9) +
      # CI bounds - subtle with system color
      geom_vline(xintercept = contrast_result$ci_lower,
                 linetype = "dashed", color = hist_color, linewidth = 0.6) +
      geom_vline(xintercept = contrast_result$ci_upper,
                 linetype = "dashed", color = hist_color, linewidth = 0.6) +
      # Direct labels instead of legend
      annotate("text", x = contrast_result$mean_diff,
               y = Inf, label = "mean",
               vjust = 1.5, hjust = -0.1,
               color = "gray10", size = 3, family = "sans") +
      annotate("text", x = 0,
               y = Inf, label = "null",
               vjust = 1.5, hjust = 1.1,
               color = "gray40", size = 3, family = "sans") +
      # Tufte theme
      theme_tufte_custom(base_size = 11) +
      labs(
        title = sprintf("P(%s|Mašriq) − P(%s|Maġrib)",
                       contrast_result$system, contrast_result$system),
        x = "Difference in Probability",
        y = NULL,  # Remove y-axis label for cleaner look
        caption = sprintf("Mean difference: %.3f  |  95%% CI: [%.3f, %.3f]",
                         contrast_result$mean_diff,
                         contrast_result$ci_lower,
                         contrast_result$ci_upper)
      ) +
      theme(
        axis.line.y = element_blank(),
        axis.text.y = element_text(color = "gray50", size = 8),
        axis.ticks.y = element_blank()
      )

    p
  }

  # PPC data (reactive) - DEPRECATED, kept for compatibility
  ppc_data <- reactive({
    preds <- posterior_preds()
    df <- rv$clean_data
    req(preds, df)

    pp <- preds$pp
    S <- preds$S
    K <- preds$K
    N <- nrow(df)

    geo01 <- as.integer(df$geo == levels(df$geo)[2])
    sim_counts_mat <- matrix(NA, nrow = S, ncol = K)

    century_idx <- match(df$century, preds$centuries)

    for (s in 1:S) {
      sim_counts <- integer(K)
      for (i in 1:N) {
        c_idx <- century_idx[i]
        probs_i <- pp[s, 1 + geo01[i], c_idx, ]
        draw_i <- sample(1:K, size = 1, prob = probs_i)
        sim_counts[draw_i] <- sim_counts[draw_i] + 1
      }
      sim_counts_mat[s, ] <- sim_counts
    }

    observed_counts <- as.numeric(table(factor(df$outcome, levels = preds$levels)))
    sim_mean <- colMeans(sim_counts_mat)
    sim_low <- apply(sim_counts_mat, 2, quantile, prob = 0.025)
    sim_high <- apply(sim_counts_mat, 2, quantile, prob = 0.975)

    data.frame(
      Category = preds$levels,
      Observed = observed_counts,
      SimMean = round(sim_mean, 1),
      SimLow = round(sim_low, 0),
      SimHigh = round(sim_high, 0)
    )
  })

  # ============================================================================
  # COMPREHENSIVE DIAGNOSTIC VISUALIZATIONS
  # ============================================================================

  # Helper: Extract draws and compute diagnostics - uses pre-computed data
  diagnostics_data <- reactive({
    req(rv$fit_obj)

    pre <- rv$fit_obj  # This is PRECOMPUTED

    # Return pre-computed diagnostics data
    list(
      draws_array = pre$draws_array,
      draws_mat = pre$draws_mat,
      param_cols = pre$param_cols,
      alpha_cols = pre$alpha_cols,
      beta_geo_cols = pre$beta_geo_cols,
      beta_cent_cols = pre$beta_cent_cols,
      has_century = pre$has_century,
      n_chains = pre$n_chains,
      n_iter = pre$n_iter,
      S = pre$S,
      diag_stats = pre$diag_stats
    )
  })

  # R-hat Plot - threshold at ~1/3 height
  output$diag_rhat <- renderPlot({
    diag <- diagnostics_data()
    req(diag)

    df <- diag$diag_stats
    df$variable <- factor(df$variable, levels = df$variable)

    # Fixed y-axis: 0.99 to 1.05 puts threshold (1.01) at ~1/3 up
    ggplot(df, aes(x = variable, y = rhat)) +
      geom_point(size = 3, color = COLORS$system["7"], shape = 16) +
      geom_hline(yintercept = 1.0, linetype = "solid",
                 color = "gray60", linewidth = 0.3) +
      geom_hline(yintercept = 1.01, linetype = "dashed",
                 color = COLORS$system["10+"], linewidth = 0.6) +
      scale_y_continuous(limits = c(0.99, 1.05), breaks = seq(0.99, 1.05, 0.01)) +
      annotate("text", x = 1, y = 1.01, label = "threshold = 1.01",
               vjust = -0.5, hjust = 0, size = 2.5, color = COLORS$system["10+"]) +
      labs(x = NULL, y = expression(hat(R))) +
      theme_tufte_custom(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  })

  # ESS Plot - threshold at ~1/3 height to match R-hat
  output$diag_ess <- renderPlot({
    diag <- diagnostics_data()
    req(diag)

    df <- diag$diag_stats

    ess_long <- rbind(
      data.frame(parameter = df$variable, ESS = df$ess_bulk, type = "Bulk"),
      data.frame(parameter = df$variable, ESS = df$ess_tail, type = "Tail")
    )
    ess_long$parameter <- factor(ess_long$parameter, levels = df$variable)

    # Calculate y-max to put threshold (400) at ~1/3 up (matching R-hat)
    # If threshold is at 1/3, then max = threshold * 3 = 1200
    y_max <- max(1200, max(ess_long$ESS) * 1.1)

    ggplot(ess_long, aes(x = parameter, y = ESS, fill = type)) +
      geom_col(position = position_dodge(0.8), width = 0.7,
               color = NA, alpha = 0.85) +
      geom_hline(yintercept = 400, linetype = "dashed",
                 color = COLORS$system["10+"], linewidth = 0.6) +
      scale_fill_manual(values = COLORS$ess, name = NULL) +
      scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
      annotate("text", x = 0.5, y = 400, label = "threshold = 400",
               vjust = -0.5, size = 2.5, color = COLORS$system["10+"]) +
      labs(x = NULL, y = "Effective Sample Size") +
      theme_tufte_custom(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
  })

  # Diagnostic Summary Table with individual colored checkmarks
  output$diag_summary_table <- renderUI({
    diag <- diagnostics_data()
    req(diag)

    df <- diag$diag_stats

    # Create HTML table rows with individual checkmarks
    rows <- lapply(1:nrow(df), function(i) {
      rhat_val <- df$rhat[i]
      ess_bulk_val <- df$ess_bulk[i]
      ess_tail_val <- df$ess_tail[i]

      # Individual checkmarks (green ✓ if pass, red ✗ if fail)
      rhat_check <- if (rhat_val < 1.01) {
        span(style = "color: #009E73; font-weight: bold;", " ✓")
      } else if (rhat_val < 1.05) {
        span(style = "color: #E69F00;", " ~")
      } else {
        span(style = "color: #D55E00; font-weight: bold;", " ✗")
      }

      ess_bulk_check <- if (ess_bulk_val > 400) {
        span(style = "color: #009E73; font-weight: bold;", " ✓")
      } else if (ess_bulk_val > 100) {
        span(style = "color: #E69F00;", " ~")
      } else {
        span(style = "color: #D55E00; font-weight: bold;", " ✗")
      }

      ess_tail_check <- if (ess_tail_val > 400) {
        span(style = "color: #009E73; font-weight: bold;", " ✓")
      } else if (ess_tail_val > 100) {
        span(style = "color: #E69F00;", " ~")
      } else {
        span(style = "color: #D55E00; font-weight: bold;", " ✗")
      }

      tags$tr(
        tags$td(df$variable[i]),
        tags$td(sprintf("%.4f", rhat_val), rhat_check),
        tags$td(round(ess_bulk_val), ess_bulk_check),
        tags$td(round(ess_tail_val), ess_tail_check)
      )
    })

    tags$table(class = "table table-striped table-hover",
      style = "width: 100%;",
      tags$thead(
        tags$tr(
          tags$th("Parameter"),
          tags$th("R-hat", tags$small(style = "color: gray;", " (< 1.01)")),
          tags$th("ESS-Bulk", tags$small(style = "color: gray;", " (> 400)")),
          tags$th("ESS-Tail", tags$small(style = "color: gray;", " (> 400)"))
        )
      ),
      tags$tbody(rows)
    )
  })

  # Trace Plots
  output$diag_trace <- renderPlot({
    diag <- diagnostics_data()
    req(diag)

    draws_array <- diag$draws_array
    param_cols <- diag$param_cols
    n_chains <- diag$n_chains
    n_iter <- diag$n_iter

    # Build trace data
    trace_data <- do.call(rbind, lapply(param_cols, function(p) {
      do.call(rbind, lapply(1:n_chains, function(ch) {
        data.frame(
          iteration = 1:n_iter,
          value = as.vector(draws_array[, ch, p]),
          chain = factor(ch),
          parameter = p
        )
      }))
    }))
    trace_data$parameter <- factor(trace_data$parameter, levels = param_cols)

    # Okabe-Ito colors for chains
    chain_colors <- COLORS$chains[1:n_chains]

    ggplot(trace_data, aes(x = iteration, y = value, color = chain)) +
      geom_line(alpha = 0.7, linewidth = 0.3) +
      facet_wrap(~parameter, scales = "free_y", ncol = 3) +
      scale_color_manual(values = chain_colors, name = "Chain") +
      labs(x = "Iteration", y = NULL) +
      theme_tufte_custom(base_size = 9) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_text(size = 7)
      )
  })

  # Reactive to store which parameter was clicked
  selected_param <- reactiveVal(NULL)

  # Generate grid of individual posterior plots (each clickable)
  output$posteriors_grid <- renderUI({
    diag <- diagnostics_data()
    req(diag)

    param_cols <- diag$param_cols
    n_params <- length(param_cols)

    # Create a grid of clickable plot containers
    # 3 columns layout
    rows <- ceiling(n_params / 3)

    row_divs <- lapply(1:rows, function(row_idx) {
      start_idx <- (row_idx - 1) * 3 + 1
      end_idx <- min(row_idx * 3, n_params)

      col_divs <- lapply(start_idx:end_idx, function(i) {
        param <- param_cols[i]
        param_safe <- gsub("\\[|\\]", "_", param)  # Safe ID for HTML

        div(
          style = "flex: 1; min-width: 280px; max-width: 33%; padding: 5px;",
          div(
            style = "cursor: pointer; border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; transition: box-shadow 0.2s;",
            class = "posterior-card",
            onclick = sprintf("Shiny.setInputValue('clicked_param', '%s', {priority: 'event'})", param),
            plotOutput(paste0("posterior_plot_", param_safe), height = "150px")
          )
        )
      })

      div(style = "display: flex; flex-wrap: wrap; justify-content: flex-start;", col_divs)
    })

    tagList(
      tags$style(HTML("
        .posterior-card:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          border-color: #3498db;
        }
      ")),
      row_divs
    )
  })

  # Dynamically render individual parameter plots
  observe({
    diag <- diagnostics_data()
    req(diag)

    draws_array <- diag$draws_array
    param_cols <- diag$param_cols
    n_chains <- diag$n_chains
    chain_colors <- COLORS$chains[1:n_chains]

    lapply(param_cols, function(param) {
      param_safe <- gsub("\\[|\\]", "_", param)
      output_id <- paste0("posterior_plot_", param_safe)

      output[[output_id]] <- renderPlot({
        # Combine all chain values for overall statistics
        all_vals <- as.vector(draws_array[, , param])
        q025 <- quantile(all_vals, 0.025)
        q975 <- quantile(all_vals, 0.975)
        zero_in_ci <- (0 >= q025 && 0 <= q975)
        zero_line_color <- if (zero_in_ci) "#e74c3c" else "gray50"

        # Build data for this parameter
        plot_data <- do.call(rbind, lapply(1:n_chains, function(ch) {
          vals <- as.vector(draws_array[, ch, param])
          dens <- density(vals, n = 256)
          data.frame(
            Chain = factor(ch),
            x = dens$x,
            y = dens$y
          )
        }))

        # Get max y for CI shading
        max_y <- max(plot_data$y)

        ggplot(plot_data, aes(x = x, y = y, color = Chain, fill = Chain)) +
          # 95% CI shading (background)
          annotate("rect", xmin = q025, xmax = q975, ymin = 0, ymax = max_y * 1.05,
                   fill = "#3498db", alpha = 0.15) +
          geom_line(linewidth = 0.8) +
          geom_area(alpha = 0.3, position = "identity") +
          geom_vline(xintercept = 0, linetype = "dashed", color = zero_line_color, linewidth = 0.7) +
          scale_color_manual(values = chain_colors) +
          scale_fill_manual(values = chain_colors) +
          labs(title = param, x = NULL, y = NULL) +
          theme_tufte_custom(base_size = 9) +
          theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 8),
            plot.margin = margin(5, 10, 5, 10)
          )
      })
    })
  })

  # Observer to handle click on specific parameter
  observeEvent(input$clicked_param, {
    param <- input$clicked_param
    selected_param(param)

    showModal(modalDialog(
      title = paste0("Posterior Distribution: ", param),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      p("Hover over density curves to see detailed chain statistics.", style = "color: gray; margin-bottom: 15px;"),
      plotlyOutput("posterior_modal_single", height = "400px"),
      br(),
      div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 15px;",
        h5(style = "margin-bottom: 12px;", "Chain Statistics"),
        uiOutput("posterior_modal_stats_single")
      )
    ))
  }, ignoreInit = TRUE)

  # Single parameter interactive modal plot with hover stats
  output$posterior_modal_single <- renderPlotly({
    diag <- diagnostics_data()
    req(diag)

    param <- selected_param()
    req(param)

    draws_array <- diag$draws_array
    n_chains <- diag$n_chains
    chain_colors <- COLORS$chains[1:n_chains]

    # Calculate overall 95% CI from combined chains
    all_vals <- as.vector(draws_array[, , param])
    overall_q025 <- quantile(all_vals, 0.025)
    overall_q975 <- quantile(all_vals, 0.975)
    zero_in_ci <- (0 >= overall_q025 && 0 <= overall_q975)
    zero_line_color <- if (zero_in_ci) "#e74c3c" else "gray50"

    # Get max y for shapes
    max_y <- max(sapply(1:n_chains, function(ch) max(density(draws_array[, ch, param])$y)))

    p <- plot_ly()

    # Add 95% CI shading first (behind other traces)
    p <- p %>% add_trace(
      x = c(overall_q025, overall_q975, overall_q975, overall_q025, overall_q025),
      y = c(0, 0, max_y * 1.05, max_y * 1.05, 0),
      type = 'scatter',
      mode = 'none',
      fill = 'toself',
      fillcolor = 'rgba(52, 152, 219, 0.15)',
      line = list(width = 0),
      name = '95% CI',
      hoverinfo = 'text',
      text = paste0("95% Credible Interval<br>[", round(overall_q025, 4), ", ", round(overall_q975, 4), "]"),
      showlegend = TRUE
    )

    for (ch in 1:n_chains) {
      chain_values <- as.vector(draws_array[, ch, param])

      stats <- list(
        mean = round(mean(chain_values), 4),
        median = round(median(chain_values), 4),
        sd = round(sd(chain_values), 4),
        q025 = round(quantile(chain_values, 0.025), 4),
        q975 = round(quantile(chain_values, 0.975), 4),
        n = length(chain_values)
      )

      dens <- density(chain_values, n = 512)

      hover_text <- paste0(
        "<b>Chain ", ch, "</b><br><br>",
        "<b>Location:</b><br>",
        "Mean: ", stats$mean, "<br>",
        "Median: ", stats$median, "<br><br>",
        "<b>Spread:</b><br>",
        "SD: ", stats$sd, "<br>",
        "95% CI: [", stats$q025, ", ", stats$q975, "]<br><br>",
        "<b>Samples:</b> ", format(stats$n, big.mark = ",")
      )

      p <- p %>% add_trace(
        x = dens$x,
        y = dens$y,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy',
        fillcolor = paste0(chain_colors[ch], "50"),
        line = list(color = chain_colors[ch], width = 2),
        name = paste("Chain", ch),
        text = hover_text,
        hoverinfo = 'text'
      )
    }

    # Add zero line (red if inside CI, gray otherwise)
    zero_label <- if (zero_in_ci) "Zero (inside CI)" else "Zero (outside CI)"
    p <- p %>% add_trace(
      x = c(0, 0), y = c(0, max_y * 1.05),
      type = 'scatter', mode = 'lines',
      line = list(color = zero_line_color, width = 2, dash = 'dash'),
      name = zero_label,
      hoverinfo = 'text',
      text = paste0("Zero reference line<br>", if(zero_in_ci) "Inside 95% CI (effect may be zero)" else "Outside 95% CI (significant effect)")
    )

    p %>% layout(
      xaxis = list(title = "Parameter Value", titlefont = list(size = 12)),
      yaxis = list(title = "Density", showticklabels = FALSE),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      hovermode = "closest",
      margin = list(t = 20, b = 60)
    ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })

  # Modal stats table for single parameter
  output$posterior_modal_stats_single <- renderUI({
    diag <- diagnostics_data()
    req(diag)

    param <- selected_param()
    req(param)

    draws_array <- diag$draws_array
    n_chains <- diag$n_chains
    chain_colors <- COLORS$chains[1:n_chains]

    # Build stats for each chain
    chain_stats <- lapply(1:n_chains, function(ch) {
      vals <- as.vector(draws_array[, ch, param])
      list(
        chain = ch,
        mean = round(mean(vals), 4),
        median = round(median(vals), 4),
        sd = round(sd(vals), 4),
        q025 = round(quantile(vals, 0.025), 4),
        q975 = round(quantile(vals, 0.975), 4)
      )
    })

    # Create compact table
    div(style = "display: flex; flex-wrap: wrap; gap: 15px;",
      lapply(chain_stats, function(s) {
        div(
          style = paste0(
            "flex: 1; min-width: 180px; padding: 12px; border-radius: 6px; ",
            "border-left: 4px solid ", chain_colors[s$chain], "; background: white;"
          ),
          h6(style = paste0("color: ", chain_colors[s$chain], "; margin: 0 0 8px 0;"),
             paste("Chain", s$chain)),
          p(style = "margin: 3px 0; font-size: 0.9em;",
            tags$b("Mean: "), s$mean, " | ", tags$b("Median: "), s$median),
          p(style = "margin: 3px 0; font-size: 0.9em;",
            tags$b("SD: "), s$sd),
          p(style = "margin: 3px 0; font-size: 0.9em;",
            tags$b("95% CI: "), paste0("[", s$q025, ", ", s$q975, "]"))
        )
      })
    )
  })

  # Legacy - keep for any references (now unused)
  output$posterior_modal_stats_full <- renderUI({
    diag <- diagnostics_data()
    req(diag)

    draws_array <- diag$draws_array
    param_cols <- diag$param_cols
    n_chains <- diag$n_chains

    chain_colors <- COLORS$chains[1:n_chains]

    # Calculate per-chain stats across all parameters
    stats_rows <- lapply(1:n_chains, function(ch) {
      # Aggregate stats across parameters
      all_means <- sapply(param_cols, function(p) mean(draws_array[, ch, p]))
      all_sds <- sapply(param_cols, function(p) sd(draws_array[, ch, p]))

      tags$tr(
        tags$td(style = paste0("color: ", chain_colors[ch], "; font-weight: bold;"),
                paste("Chain", ch)),
        tags$td(format(nrow(draws_array), big.mark = ",")),
        tags$td(round(mean(all_sds), 4)),
        tags$td(round(sd(all_means), 4))
      )
    })

    tags$table(
      class = "table table-sm table-striped",
      style = "font-size: 13px; margin: 0;",
      tags$thead(
        tags$tr(
          tags$th("Chain"),
          tags$th("Samples"),
          tags$th("Avg. SD"),
          tags$th("Mean Variability")
        )
      ),
      tags$tbody(stats_rows)
    )
  })

  # Chain Information Content Stats
  output$chain_info_stats <- renderUI({
    diag <- diagnostics_data()
    req(diag)

    draws_array <- diag$draws_array
    param_cols <- diag$param_cols
    n_chains <- diag$n_chains
    n_iter <- diag$n_iter

    chain_colors <- COLORS$chains[1:n_chains]

    # Calculate information metrics for each chain
    chain_info <- lapply(1:n_chains, function(ch) {
      # Effective sample size contribution (using variance of means)
      chain_means <- sapply(param_cols, function(p) mean(draws_array[, ch, p]))
      chain_vars <- sapply(param_cols, function(p) var(draws_array[, ch, p]))

      # Overall mean variance for this chain
      avg_var <- mean(chain_vars)

      # Calculate chain's contribution to posterior precision
      # Lower variance = more informative
      precision_contribution <- 1 / avg_var

      # Calculate autocorrelation (lower = better mixing)
      avg_autocorr <- mean(sapply(param_cols, function(p) {
        vals <- draws_array[, ch, p]
        if (length(vals) > 1) {
          acf_val <- acf(vals, lag.max = 1, plot = FALSE)$acf[2]
          if (is.na(acf_val)) 0.5 else abs(acf_val)
        } else 0.5
      }))

      # Effective samples per iteration (efficiency)
      efficiency <- 1 / (1 + 2 * avg_autocorr)

      list(
        chain = ch,
        samples = n_iter,
        avg_variance = avg_var,
        precision = precision_contribution,
        autocorr = avg_autocorr,
        efficiency = efficiency
      )
    })

    # Normalize precision contributions to percentages
    total_precision <- sum(sapply(chain_info, function(x) x$precision))

    # Build table
    info_rows <- lapply(chain_info, function(info) {
      pct_contribution <- round(100 * info$precision / total_precision, 1)
      efficiency_pct <- round(100 * info$efficiency, 0)

      # Color code efficiency
      eff_color <- if (efficiency_pct >= 50) "#009E73" else if (efficiency_pct >= 25) "#E69F00" else "#D55E00"

      tags$tr(
        tags$td(style = paste0("color: ", chain_colors[info$chain], "; font-weight: bold;"),
                paste("Chain", info$chain)),
        tags$td(format(info$samples, big.mark = ",")),
        tags$td(round(info$avg_variance, 4)),
        tags$td(round(info$autocorr, 3)),
        tags$td(style = paste0("color: ", eff_color, "; font-weight: bold;"),
                paste0(efficiency_pct, "%")),
        tags$td(style = "font-weight: bold;",
                paste0(pct_contribution, "%"))
      )
    })

    div(
      tags$table(
        class = "table table-sm",
        style = "font-size: 12px; margin: 0;",
        tags$thead(
          tags$tr(
            tags$th("Chain"),
            tags$th("Samples"),
            tags$th("Avg. Variance"),
            tags$th("Autocorr."),
            tags$th("Efficiency"),
            tags$th("Info. Contrib.")
          )
        ),
        tags$tbody(info_rows)
      ),
      tags$p(style = "font-size: 11px; color: #666; margin-top: 10px;",
        tags$b("Efficiency:"), " % of samples providing independent information (higher = better mixing). ",
        tags$b("Info. Contrib.:"), " Relative contribution to posterior precision."
      )
    )
  })

  # Parameter Correlation Heatmap
  output$diag_correlation <- renderPlot({
    diag <- diagnostics_data()
    req(diag)

    draws_mat <- diag$draws_mat
    param_cols <- diag$param_cols

    # Compute correlation matrix
    param_draws <- draws_mat[, param_cols, drop = FALSE]
    cor_mat <- cor(param_draws)

    # Convert to long format
    cor_long <- data.frame(
      Var1 = rep(colnames(cor_mat), each = length(param_cols)),
      Var2 = rep(colnames(cor_mat), length(param_cols)),
      value = as.vector(cor_mat)
    )
    cor_long$Var1 <- factor(cor_long$Var1, levels = param_cols)
    cor_long$Var2 <- factor(cor_long$Var2, levels = rev(param_cols))

    ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", value)),
                size = 2.5, color = "gray20") +
      scale_fill_gradient2(low = COLORS$correlation["low"],
                           mid = COLORS$correlation["mid"],
                           high = COLORS$correlation["high"],
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(x = NULL, y = NULL) +
      theme_tufte_custom(base_size = 9) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"  # Values shown directly
      )
  })

  # Posterior Predictive Check (Interactive Plotly Version)
  output$diag_ppc <- renderPlotly({
    preds <- posterior_preds()
    df <- rv$clean_data
    req(preds, df)

    pp <- preds$pp
    S <- preds$S
    K <- preds$K
    N <- nrow(df)
    lvl <- preds$levels

    # Get observed counts by region
    geo_levels <- levels(df$geo)

    # Simulate counts from posterior
    n_sims <- min(500, S)
    set.seed(123)
    sim_idx <- sample(1:S, n_sims)

    geo01 <- as.integer(df$geo == geo_levels[2])

    # Handle century model vs simple model
    if (preds$has_century) {
      century_idx <- match(df$century, preds$centuries)

      sim_counts <- array(NA, dim = c(n_sims, 2, K))  # sims x regions x categories
      for (i in 1:n_sims) {
        s <- sim_idx[i]
        for (r in 1:2) {
          obs_in_region <- which(geo01 == (r - 1))
          counts <- integer(K)
          for (j in obs_in_region) {
            c_idx <- century_idx[j]
            probs_j <- pp[s, r, c_idx, ]
            draw_j <- sample(1:K, size = 1, prob = probs_j)
            counts[draw_j] <- counts[draw_j] + 1
          }
          sim_counts[i, r, ] <- counts
        }
      }
    } else {
      sim_counts <- array(NA, dim = c(n_sims, 2, K))
      for (i in 1:n_sims) {
        s <- sim_idx[i]
        for (r in 1:2) {
          obs_in_region <- which(geo01 == (r - 1))
          counts <- integer(K)
          for (j in obs_in_region) {
            probs_j <- pp[s, r, ]
            draw_j <- sample(1:K, size = 1, prob = probs_j)
            counts[draw_j] <- counts[draw_j] + 1
          }
          sim_counts[i, r, ] <- counts
        }
      }
    }

    # Observed counts
    obs_counts <- table(df$outcome, df$geo)

    # Calculate predicted means (for hover info)
    n_region <- c(sum(geo01 == 0), sum(geo01 == 1))
    if (preds$has_century) {
      pred_means <- matrix(NA, K, 2)
      for (r in 1:2) {
        region_obs <- which(geo01 == (r - 1))
        century_idx_local <- match(df$century[region_obs], preds$centuries)
        for (k in 1:K) {
          mean_prob <- mean(sapply(1:S, function(s) {
            mean(pp[s, r, century_idx_local, k])
          }))
          pred_means[k, r] <- mean_prob * length(region_obs)
        }
      }
    } else {
      pred_means <- matrix(NA, K, 2)
      for (r in 1:2) {
        for (k in 1:K) {
          pred_means[k, r] <- mean(pp[, r, k]) * n_region[r]
        }
      }
    }

    # Build plot data with hover info
    region_names <- c("Maġrib", "Mašriq")
    ppc_plot_data <- do.call(rbind, lapply(1:2, function(r) {
      do.call(rbind, lapply(1:K, function(k) {
        sim_vals <- sim_counts[, r, k]
        obs_val <- obs_counts[k, r]
        pred_val <- round(pred_means[k, r], 1)
        diff_val <- obs_val - pred_val
        sim_mean <- round(mean(sim_vals), 1)
        sim_sd <- round(sd(sim_vals), 1)
        sim_q025 <- round(quantile(sim_vals, 0.025), 0)
        sim_q975 <- round(quantile(sim_vals, 0.975), 0)

        data.frame(
          Region = region_names[r],
          Category = lvl[k],
          Simulated = sim_vals,
          Observed = obs_val,
          Predicted = pred_val,
          Difference = diff_val,
          SimMean = sim_mean,
          SimSD = sim_sd,
          SimQ025 = sim_q025,
          SimQ975 = sim_q975
        )
      }))
    }))
    ppc_plot_data$Category <- factor(ppc_plot_data$Category, levels = lvl)

    # Create hover text for histograms
    ppc_plot_data$hover_text <- sprintf(
      paste0(
        "<b>%s - %s</b><br><br>",
        "<b>Model Fit:</b><br>",
        "Observed: %d<br>",
        "Predicted: %.1f<br>",
        "Difference: %s%.1f<br><br>",
        "<b>Simulation Stats:</b><br>",
        "Mean: %.1f<br>",
        "SD: %.1f<br>",
        "95%% Interval: [%d, %d]"
      ),
      ppc_plot_data$Category, ppc_plot_data$Region,
      ppc_plot_data$Observed, ppc_plot_data$Predicted,
      ifelse(ppc_plot_data$Difference >= 0, "+", ""), ppc_plot_data$Difference,
      ppc_plot_data$SimMean, ppc_plot_data$SimSD,
      ppc_plot_data$SimQ025, ppc_plot_data$SimQ975
    )

    # Map categories to colors (orange=7, green=7+1, blue=10+)
    category_colors <- COLORS$system[lvl]
    if (any(is.na(category_colors))) {
      category_colors <- setNames(
        c("#E69F00", "#009E73", "#0072B2")[1:length(lvl)],
        lvl
      )
    }

    p <- ggplot(ppc_plot_data, aes(x = Simulated, fill = Category, text = hover_text)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20,
                     alpha = 0.75, color = "white",
                     linewidth = 0.25) +
      geom_vline(aes(xintercept = Observed),
                 color = "gray10", linewidth = 0.7) +
      facet_grid(Region ~ Category, scales = "free_x") +
      scale_fill_manual(values = category_colors, guide = "none") +
      labs(x = "Simulated Count", y = NULL) +
      theme_tufte_custom(base_size = 9) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 11)),
        showlegend = FALSE
      )
  })

  # Temporal Predictions with Spaghetti
  output$diag_temporal <- renderPlot({
    diag <- diagnostics_data()
    preds <- posterior_preds()
    req(diag, preds, diag$has_century)

    draws_mat <- diag$draws_mat
    alpha_cols <- diag$alpha_cols
    beta_geo_cols <- diag$beta_geo_cols
    beta_cent_cols <- diag$beta_cent_cols

    S <- diag$S
    lvl <- preds$levels
    K <- length(lvl)

    # Extract parameter matrices
    alpha <- draws_mat[, alpha_cols, drop = FALSE]
    beta_geo <- draws_mat[, beta_geo_cols, drop = FALSE]
    beta_cent <- draws_mat[, beta_cent_cols, drop = FALSE]

    # Get century info from data
    cent_mean <- mean(rv$clean_data$century)
    century_seq <- seq(min(rv$clean_data$century), max(rv$clean_data$century), by = 0.1)

    # Vectorized softmax
    softmax_vec <- function(eta_matrix) {
      exp_eta <- exp(eta_matrix - apply(eta_matrix, 1, max))
      exp_eta / rowSums(exp_eta)
    }

    # Sample draws for spaghetti
    n_draws_show <- 50
    set.seed(42)
    draw_idx <- sample(1:S, n_draws_show)

    # Compute predictions
    results <- data.frame()
    spaghetti <- data.frame()

    for (cent_val in century_seq) {
      cent_dev <- cent_val - cent_mean

      for (geo in 0:1) {
        region <- ifelse(geo == 0, "Maghrib", "Mashriq")

        # All draws
        eta_all <- cbind(0,
                         alpha[,1] + beta_geo[,1] * geo + beta_cent[,1] * cent_dev,
                         alpha[,2] + beta_geo[,2] * geo + beta_cent[,2] * cent_dev)
        probs_all <- softmax_vec(eta_all)

        for (k in 1:K) {
          results <- rbind(results, data.frame(
            Century = cent_val,
            Region = region,
            Category = lvl[k],
            Mean = mean(probs_all[, k]),
            CI50_low = quantile(probs_all[, k], 0.25),
            CI50_high = quantile(probs_all[, k], 0.75)
          ))

          # Spaghetti draws
          for (d in seq_along(draw_idx)) {
            spaghetti <- rbind(spaghetti, data.frame(
              Century = cent_val,
              Region = region,
              Category = lvl[k],
              Draw = d,
              Prob = probs_all[draw_idx[d], k]
            ))
          }
        }
      }
    }

    results$Category <- factor(results$Category, levels = lvl)
    spaghetti$Category <- factor(spaghetti$Category, levels = lvl)

    # Colors for categories (orange=7, green=7+1, blue=10+)
    category_colors <- COLORS$system[lvl]
    # If some categories don't match, use default colors
    if (any(is.na(category_colors))) {
      category_colors <- setNames(
        c("#E69F00", "#009E73", "#0072B2")[1:K],
        lvl
      )
    }

    ggplot() +
      # Spaghetti - very subtle
      geom_line(data = spaghetti,
                aes(x = Century, y = Prob, color = Category,
                    group = interaction(Category, Draw)),
                alpha = 0.08, linewidth = 0.2) +
      # Ribbon - subtle shading
      geom_ribbon(data = results,
                  aes(x = Century, ymin = CI50_low, ymax = CI50_high,
                      fill = Category),
                  alpha = 0.25) +
      # Mean line - primary data ink
      geom_line(data = results,
                aes(x = Century, y = Mean, color = Category),
                linewidth = 0.8) +
      facet_wrap(~Region, ncol = 2) +
      scale_color_manual(values = category_colors, name = NULL) +
      scale_fill_manual(values = category_colors, name = NULL) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      labs(x = "Century", y = "Probability") +
      theme_tufte_custom(base_size = 10) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 8)
      )
  })

  # Data visualizations
  output$data_dist_outcome <- renderPlotly({
    req(rv$clean_data)
    # Calculate counts and percentages for hover
    counts <- rv$clean_data %>%
      count(outcome) %>%
      mutate(pct = round(100 * n / sum(n), 1),
             hover_text = paste0(outcome, "<br>Count: ", n, "<br>", pct, "% of total"))

    p <- ggplot(counts, aes(x = outcome, y = n, fill = outcome, text = hover_text)) +
      geom_col(color = NA, alpha = 0.85, width = 0.7) +
      scale_fill_manual(values = COLORS$system, guide = "none") +
      scale_x_discrete(limits = c("7", "7+1", "10+")) +
      theme_tufte_custom(base_size = 11) +
      labs(title = "Reading Systems", x = NULL, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = "none")
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(categoryorder = "array", categoryarray = c("7", "7+1", "10+"))
      )
  })

  output$data_dist_geo <- renderPlotly({
    req(rv$clean_data)
    # Calculate counts and percentages for hover
    counts <- rv$clean_data %>%
      count(geo) %>%
      mutate(pct = round(100 * n / sum(n), 1),
             hover_text = paste0(geo, "<br>Count: ", n, "<br>", pct, "% of total"))

    p <- ggplot(counts, aes(x = geo, y = n, fill = geo, text = hover_text)) +
      geom_col(color = NA, alpha = 0.85, width = 0.6) +
      scale_fill_manual(values = COLORS$region, guide = "none") +
      theme_tufte_custom(base_size = 11) +
      labs(title = "Regional Distribution", x = NULL, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = "none")
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  output$data_dist_century <- renderPlotly({
    req(rv$clean_data)
    # Calculate counts and percentages for hover
    counts <- rv$clean_data %>%
      mutate(century_label = paste0(century, "th c. AH")) %>%
      count(century_label, century) %>%
      mutate(pct = round(100 * n / sum(n), 1),
             hover_text = paste0(century_label, "<br>Count: ", n, "<br>", pct, "% of total"))

    # Order the centuries properly
    counts$century_label <- factor(counts$century_label, levels = paste0(4:7, "th c. AH"))

    p <- ggplot(counts, aes(x = century_label, y = n, fill = century_label, text = hover_text)) +
      geom_col(color = NA, alpha = 0.85, width = 0.7) +
      scale_fill_manual(values = c(
        "4th c. AH" = "#8B4513",  # Saddle brown
        "5th c. AH" = "#CD853F",  # Peru
        "6th c. AH" = "#D2691E",  # Chocolate
        "7th c. AH" = "#A0522D"   # Sienna
      ), guide = "none") +
      theme_tufte_custom(base_size = 11) +
      labs(title = "Century Distribution", x = NULL, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
            legend.position = "none")
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })

  output$contingency_table <- renderTable({
    req(rv$clean_data)
    tab <- table(rv$clean_data$outcome, rv$clean_data$geo)
    result <- as.data.frame.matrix(tab)
    result$System <- rownames(result)
    result <- result[, c(ncol(result), 1:(ncol(result)-1))]
    result$Total <- rowSums(result[, -1])
    result
  }, rownames = FALSE)

  output$data_table <- renderDT({
    req(rv$raw_data)

    if (input$toggle_arabic) {
      display_df <- rv$raw_data %>%
        select(any_of(c("work_id", "title", "title_arabic", "author_name", "author_name_arabic",
                       "system", "type", "regionality")))
    } else {
      display_df <- rv$raw_data %>%
        select(any_of(c("work_id", "title", "author_name", "system", "type", "regionality")))
    }

    datatable(
      display_df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ========== Reference Annotation Server Logic (DISABLED - all 38 references validated) ==========
  # This section is commented out because all text_reuse references have been validated.
  # The text_reuse symbols are now displayed in the Corpus Explorer based on validated data.
  # To re-enable the annotation interface, uncomment this section and the UI tab above.
  #
  # NOTE: Reference Annotation server logic commented out
  # Contains: status outputs, navigation, search, matching, and export functionality
  # All 38 text_reuse references have been validated - see text_reuse_references table
  #
  # To re-enable, restore the server functions for:
  # - ref_status_*, ref_source_work, ref_bracket_text (UI outputs)
  # - ref_auto_suggestions, ref_search_results (search/matching)
  # - ref_prev/next/skip, ref_save_match, ref_no_match (actions)
  # - ref_all_table, ref_export_csv, ref_export_sql (table/export)

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

# Run app
shinyApp(ui = ui, server = server)
