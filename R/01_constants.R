# Constants and configuration
# Loaded first (alphabetical) — makes DB_PATH, PRECOMPUTED, CONFOUND, etc. available to all R/ files

source("db_config.R")

# Pre-computed Bayesian results (replaces live MCMC)
PRECOMPUTED_PATH <- "data/precomputed_bayesian_results.rds"
PRECOMPUTED <- if (file.exists(PRECOMPUTED_PATH)) {
  readRDS(PRECOMPUTED_PATH)
} else {
  NULL
}

# Pre-computed confound analysis results
CONFOUND_PATH <- "data/precomputed_confound_results.rds"
CONFOUND <- if (file.exists(CONFOUND_PATH)) {
  readRDS(CONFOUND_PATH)
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

# Okabe-Ito colorblind-friendly palette for academic publication
# JSD method colors (model-based = blue, Dirichlet-smoothed = vermillion);
# shared by the Bayesian server panels and the Card 5 UI labels.
JSD_METHOD_COLORS <- c("Model-based" = "#0072B2", "Dirichlet-smoothed" = "#D55E00")

COLORS <- list(
  # Reading systems (sets)
  set = c(
    "7"   = "#E69F00",   # Orange
    "7+1" = "#009E73",   # Green
    "10+" = "#0072B2"    # Blue
  ),
  # Regions
  region = c(
    "ma\u0121rib" = "#56B4E9",  # Sky Blue (cool, western)
    "ma\u0161riq" = "#E69F00"   # Orange (warm, eastern)
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

# Cache-buster for the embedded paper. www/paper.html is 7.5 MB and www/manuscript.pdf 4.9 MB;
# browsers cache both indefinitely, so a redeployed paper is invisible to anyone who has already
# opened the app. Keying the URL to the file's mtime makes a new render a new URL.
PAPER_VERSION <- if (file.exists("www/paper.html")) {
  format(as.integer(as.POSIXct(file.mtime("www/paper.html"))))
} else {
  "0"
}
