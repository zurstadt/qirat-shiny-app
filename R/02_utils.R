# Utility functions — theme, formatting, badge helpers

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
# 2. FIRST POSITION: B./b. -> Ibn, ibn -> Ibn, al- always lowercase
# 3. MID-NAME PATRONYMICS: ibn -> b., bint -> bt.
# 4. KUNYAS: Capitalize kunya AND following name component
# 5. al- PREFIX: Always lowercase, capitalize word after hyphen
# 6. PARTICLES: Always lowercase
# 7. ALL OTHER WORDS: Capitalize first real letter (after hamza/ayn)

format_camel_case <- function(text, type = "author") {
  if (is.na(text) || text == "" || is.null(text)) return(text)

  # For author names, first extract canonical form from slash variants
  if (type == "author") {
    text <- gsub("([^/\\s]+)/[^\\s]+", "\\1", as.character(text))
  }

  # Split into words
  words <- strsplit(as.character(text), "\\s+")[[1]]
  if (length(words) == 0) return(text)

  # Capitalize first "real" letter, accounting for hamza and ayn
  capitalize_first_real <- function(word) {
    if (nchar(word) == 0) return(word)
    chars <- strsplit(word, "")[[1]]
    if (length(chars) == 0) return(word)
    for (j in seq_along(chars)) {
      ch <- chars[j]
      if (!(ch %in% c("\u02be", "\u02bf"))) {
        chars[j] <- toupper(ch)
        break
      }
    }
    paste0(chars, collapse = "")
  }

  # Check if a word is a kunya
  is_kunya <- function(word) {
    word_clean <- tolower(gsub("^[\u02be\u02bf]+", "", word))
    word_clean %in% c("ab\u016b", "abu", "ab\u012b", "abi", "umm")
  }

  # ===== TITLE FORMATTING =====
  if (type == "title") {
    in_bracket <- FALSE
    after_bracket_open <- FALSE
    after_k_dot <- FALSE

    formatted <- character(length(words))
    for (i in seq_along(words)) {
      word <- words[i]
      word_lower <- tolower(word)

      starts_with_bracket <- grepl("^\\[", word)
      if (starts_with_bracket) {
        in_bracket <- TRUE
        after_bracket_open <- TRUE
        word_no_bracket <- sub("^\\[", "", word)
      } else {
        word_no_bracket <- word
      }

      ends_with_bracket <- grepl("\\]$", word_no_bracket)
      if (ends_with_bracket) {
        word_no_bracket <- sub("\\]$", "", word_no_bracket)
      }

      word_lower_nb <- tolower(word_no_bracket)

      should_capitalize <- FALSE

      if (i == 1) should_capitalize <- TRUE
      if (i == 2 && tolower(words[1]) == "k.") should_capitalize <- TRUE

      if (after_bracket_open) {
        should_capitalize <- TRUE
        after_bracket_open <- FALSE
      }

      if (after_k_dot) {
        should_capitalize <- TRUE
        after_k_dot <- FALSE
      }

      if (word_lower_nb == "k." || word_lower_nb == "k") {
        after_k_dot <- TRUE
      }

      is_name_in_bracket <- in_bracket && is_kunya(word_no_bracket)

      if (word_lower_nb == "k." || word_lower_nb == "k") {
        result <- "K."
      } else if (grepl("^k\\.", word_no_bracket, ignore.case = TRUE)) {
        remainder <- sub("^k\\.", "", word_no_bracket, ignore.case = TRUE)
        if (grepl("^al-", remainder, ignore.case = TRUE)) {
          after_al <- sub("^al-", "", remainder, ignore.case = TRUE)
          result <- paste0("K. al-", capitalize_first_real(after_al))
        } else {
          result <- paste0("K. ", capitalize_first_real(remainder))
        }
      } else if (should_capitalize || is_name_in_bracket) {
        if (grepl("^al-", word_no_bracket, ignore.case = TRUE)) {
          after_al <- sub("^al-", "", word_no_bracket, ignore.case = TRUE)
          result <- paste0("al-", capitalize_first_real(after_al))
        } else {
          result <- capitalize_first_real(word_no_bracket)
        }
      } else {
        result <- tolower(word_no_bracket)
      }

      if (starts_with_bracket) result <- paste0("[", result)
      if (ends_with_bracket) {
        result <- paste0(result, "]")
        in_bracket <- FALSE
      }

      formatted[i] <- result
    }
    return(paste(formatted, collapse = " "))
  }

  # ===== AUTHOR FORMATTING =====
  particles <- c("f\u012b", "fi", "min", "\u02bfan", "\u02bfal\u0101", "\u02bfala", "il\u0101", "ila", "ma\u02bfa", "bi", "wa", "li")

  prev_was_kunya <- FALSE

  formatted <- character(length(words))
  for (i in seq_along(words)) {
    word <- words[i]
    word_lower <- tolower(word)

    # === FIRST WORD RULES ===
    if (i == 1) {
      if (word_lower %in% c("b.", "b")) {
        formatted[i] <- "Ibn"
        prev_was_kunya <- FALSE
        next
      }
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
      if (grepl("^al-", word, ignore.case = TRUE)) {
        after_al <- sub("^al-", "", word, ignore.case = TRUE)
        formatted[i] <- paste0("al-", capitalize_first_real(after_al))
        prev_was_kunya <- FALSE
        next
      }
      if (is_kunya(word)) {
        formatted[i] <- capitalize_first_real(word)
        prev_was_kunya <- TRUE
        next
      }
      formatted[i] <- capitalize_first_real(word)
      prev_was_kunya <- FALSE
      next
    }

    # === MID-NAME RULES ===
    if (word_lower == "ibn") {
      formatted[i] <- "b."
      prev_was_kunya <- FALSE
      next
    }
    if (word_lower == "bint") {
      formatted[i] <- "bt."
      prev_was_kunya <- FALSE
      next
    }
    if (word_lower %in% c("b.", "b", "bt.", "bt")) {
      formatted[i] <- paste0(gsub("\\.$", "", word_lower), ".")
      prev_was_kunya <- FALSE
      next
    }
    if (word_lower %in% particles) {
      formatted[i] <- word_lower
      prev_was_kunya <- FALSE
      next
    }
    if (grepl("^al-", word, ignore.case = TRUE)) {
      after_al <- sub("^al-", "", word, ignore.case = TRUE)
      formatted[i] <- paste0("al-", capitalize_first_real(after_al))
      prev_was_kunya <- FALSE
      next
    }
    if (is_kunya(word)) {
      formatted[i] <- capitalize_first_real(word)
      prev_was_kunya <- TRUE
      next
    }

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
extract_canonical_author <- function(author_str) {
  if (is.na(author_str) || author_str == "") return(author_str)
  gsub("([^/\\s]+)/[^\\s]+", "\\1", as.character(author_str))
}

# Helper: Extract all variants from author name
parse_author_variants <- function(author_str) {
  if (is.na(author_str) || author_str == "") {
    return(list(canonical = author_str, variants = character(0)))
  }

  words <- strsplit(as.character(author_str), "\\s+")[[1]]
  has_slash <- grepl("/", words)

  if (!any(has_slash)) {
    return(list(canonical = author_str, variants = character(0)))
  }

  canonical_words <- sapply(words, function(w) {
    if (grepl("/", w)) strsplit(w, "/")[[1]][1] else w
  })
  canonical <- paste(canonical_words, collapse = " ")

  variant_words <- sapply(words, function(w) {
    if (grepl("/", w)) strsplit(w, "/")[[1]][2] else w
  })
  variant <- paste(variant_words, collapse = " ")

  list(canonical = canonical, variants = variant)
}

# Helper: Create HTML badge with color coding
create_color_badge <- function(value, type = "set") {
  if (is.na(value) || value == "") return("")

  colors <- switch(type,
    "set" = c(
      "7"   = "#E69F00",
      "7+1" = "#009E73",
      "10+" = "#0072B2"
    ),
    "region" = c(
      "ma\u0121rib" = "#56B4E9",
      "ma\u0161riq" = "#E69F00",
      "maghrib" = "#56B4E9",
      "mashriq" = "#E69F00",
      "inter-regional" = "#CC79A7",
      "Ma\u0121rib" = "#56B4E9",
      "Ma\u0161riq" = "#E69F00",
      "Inter-regional" = "#CC79A7",
      "Ma\u0121rib visits Ma\u0161riq" = "#56B4E9",
      "Ma\u0161riq visits Ma\u0121rib" = "#E69F00"
    ),
    "type" = c(
      "compression" = "#9467bd",
      "expansion" = "#8c564b",
      "descriptive_catalogue" = "#e377c2",
      "didactic_poem" = "#7f7f7f",
      "mufradah" = "#bcbd22",
      "rasm" = "#2ca02c",
      "\u02bead\u0101\u02be" = "#17becf"
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
