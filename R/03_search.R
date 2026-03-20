# Search normalization and elastic matching functions

# Helper: Elastic search - normalize text for flexible matching
# Supports: digraphs (dh->d), dediacritics, Arabic script, b.->ibn
normalize_for_search <- function(text) {
  if (is.na(text) || text == "") return("")
  text <- tolower(text)

  # Normalize initial "b." to "ibn" for author searches
  text <- gsub("(^|\\s)b\\.\\s*", "\\1ibn ", text)

  # Normalize apostrophe variations (all become empty for matching)
  text <- gsub("['`\u2018\u02bf\u02be\u02bc]", "", text)

  # Digraph mappings (both directions for flexibility)
  text <- gsub("\u1e0f", "dh", text)
  text <- gsub("\u1e0d", "d", text)
  text <- gsub("\u0121", "gh", text)
  text <- gsub("\u1e2b", "kh", text)
  text <- gsub("\u0161", "sh", text)
  text <- gsub("\u1e6f", "th", text)
  text <- gsub("\u01e7", "j", text)

  # Dediacriticize emphatic/velarized consonants
  text <- gsub("\u1e63", "s", text)
  text <- gsub("\u1e6d", "t", text)
  text <- gsub("\u1e93", "z", text)
  text <- gsub("\u1e25", "h", text)

  # Dediacriticize long vowels
  text <- gsub("\u0101", "a", text)
  text <- gsub("\u012b", "i", text)
  text <- gsub("\u016b", "u", text)

  # Remove common prefixes for more flexible matching
  text <- gsub("^k\\.?\\s*", "", text)
  text <- gsub("^kitab\\s+", "", text)

  text
}

# Helper: Normalize Arabic text for flexible matching
normalize_arabic_for_search <- function(text) {
  if (is.na(text) || text == "") return("")

  # Remove Arabic diacritics (tashkeel)
  text <- gsub("[\u064B-\u065F\u0670]", "", text)

  # Normalize alif variations
  text <- gsub("[\u0622\u0623\u0625\u0627]", "\u0627", text)

  # Normalize taa marbuta to haa
  text <- gsub("\u0629", "\u0647", text)

  # Normalize yaa variations
  text <- gsub("\u0649", "\u064A", text)

  text
}

# Helper: Elastic search match
# Returns TRUE if query matches target using elastic normalization
elastic_match <- function(query, target_latin, target_arabic = NULL) {
  if (is.na(query) || query == "") return(TRUE)

  query_trimmed <- trimws(query)

  # Check if query is Arabic
  is_arabic_query <- grepl("[\u0600-\u06FF]", query_trimmed)

  if (is_arabic_query) {
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
