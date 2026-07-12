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

# ─────────────────────────────────────────────────────────────────────────────────────────────
# CREDIBLE LEVEL — DERIVED FROM THE ARTIFACT. DO NOT HARDCODE A NUMBER HERE.
#
# The level is authored in the SOURCE repo (scripts/credible_interval.R) and stamped into the
# .rds by the extract scripts. This app reads it back off the very artifact it plots.
#
# Why not just write 0.99 here? Because deploy/ is a separate repo with its own remote, and a
# second authored copy of the level is a copy free to drift. The failure it produces is the nasty
# kind: the app keeps drawing the ribbons the .rds gives it, while confidently LABELLING them at
# whatever level this file happens to say. Values from one place, label from another, both
# insisting they agree. Deriving the label from the data makes that unrepresentable.
#
# Raised 95% -> 99% on 2026-07-12; the Region coefficient still excludes zero in all five models.
# ─────────────────────────────────────────────────────────────────────────────────────────────
CI_LEVEL <- if (!is.null(PRECOMPUTED) && !is.null(PRECOMPUTED$ci_level)) {
  PRECOMPUTED$ci_level
} else if (!is.null(PRECOMPUTED)) {
  stop("precomputed_bayesian_results.rds carries no `ci_level`. It was written by an extract ",
       "script that predates the transport contract — re-run scripts/extract_bayesian_results.R ",
       "in the source repo and commit the regenerated .rds. Refusing to guess a credible level.")
} else {
  NULL  # no artifact at all: the app already degrades gracefully elsewhere on PRECOMPUTED = NULL
}

if (!is.null(CI_LEVEL)) {
  # The two artifacts must agree, or the Bayesian tab and the confound tab would silently show
  # intervals at different levels under a single heading.
  if (!is.null(CONFOUND) && !is.null(CONFOUND$ci_level) &&
      !isTRUE(all.equal(CONFOUND$ci_level, CI_LEVEL))) {
    stop(sprintf("Artifact level mismatch: bayesian .rds = %g%%, confound .rds = %g%%. ",
                 CI_LEVEL * 100, CONFOUND$ci_level * 100),
         "Re-run BOTH extract scripts and commit them together.")
  }
  CI_PCT   <- CI_LEVEL * 100
  CI_LABEL <- sprintf("%g%%", CI_PCT)
  CI_TAIL  <- (1 - CI_LEVEL) / 2
} else {
  CI_PCT <- NA_real_; CI_LABEL <- "credible"; CI_TAIL <- NA_real_
}

CI_INNER_LABEL <- "50%"

ci_lo <- function(v) unname(quantile(v, CI_TAIL,     na.rm = TRUE))
ci_hi <- function(v) unname(quantile(v, 1 - CI_TAIL, na.rm = TRUE))

# NOT the credible level. The posterior-predictive band in the diagnostics tab asks whether the
# OBSERVED counts fall inside the model's predictive spread — a model-ADEQUACY check, not a claim
# about a parameter. Widening a credible interval makes a claim HARDER to assert; widening a
# predictive band makes a fit check EASIER to pass. They pull opposite ways, so they get separate
# constants and must never be swept together. Held at the conventional 95%; the model passes 6/6
# cells there with room to spare (checked 2026-07-12).
PPC_LEVEL <- 0.95
PPC_LABEL <- sprintf("%g%%", PPC_LEVEL * 100)
PPC_TAIL  <- (1 - PPC_LEVEL) / 2

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

# The embedded paper. Its filename carries a CONTENT HASH (paper-<hash>.html), so discover it by
# pattern rather than naming it. See scripts/install_paper_into_app.R for why: a fixed path meant
# readers were served a JUNE 11 copy of the paper for hours after the corrected one was deployed,
# because nothing in the URL had changed. A `?v=` query string does not help — a path-keyed cache
# ignores it. The version has to be in the filename, and then no cache can substitute one for the
# other.
.find_asset <- function(stem, ext) {
  f <- list.files("www", pattern = sprintf("^%s-[0-9a-f]+\\.%s$", stem, ext))
  if (length(f)) f[[1]] else NA_character_          # NA -> the UI hides the tab rather than 404
}
PAPER_HTML <- .find_asset("paper", "html")
PAPER_PDF  <- .find_asset("manuscript", "pdf")
