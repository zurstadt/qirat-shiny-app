#!/usr/bin/env Rscript
# ═════════════════════════════════════════════════════════════════════════════════════════════
#  ⚠️  THIS SCRIPT DOES NOT DEPLOY THE APP. GIT DOES.
#
#      cd deploy && git push origin main        <-- THIS is the deployment
#
#  connect.posit.cloud is Posit Connect CLOUD, and it is GIT-BACKED: it redeploys from
#  github.com/zurstadt/qirat-shiny-app on every push. It does NOT serve bundles uploaded by
#  rsconnect::deployApp() — those go nowhere, and deployApp() still cheerfully reports
#  "Successfully deployed".
#
#  On 2026-07-12 that cost hours. Four commits — the corrected posterior, the fixed Confound
#  card, the re-rendered paper — sat unpushed on deploy/ while deployApp() was run three times
#  and reported success three times. Readers kept downloading the June 11 paper, odds ratio 7.5,
#  because the repo Connect actually reads had never changed.
#
#  `make check` now refuses to pass with unpushed commits on deploy/.
#
#  What this script IS for: keeping manifest.json honest and keeping junk out of the repo (see
#  the .rscignore note below). Run it, commit, THEN PUSH.
# ═════════════════════════════════════════════════════════════════════════════════════════════
#
#     cd deploy && Rscript deploy.R --manifest  # refresh manifest.json (then commit + PUSH)
#     cd deploy && Rscript deploy.R             # ...and also push a bundle, which Connect Cloud
#                                               #    ignores. Harmless; not the deployment.
#
# ─────────────────────────────────────────────────────────────────────────────────────────────
# WHY THIS FILE EXISTS.
#
# `.rscignore` DOES NOT SUPPORT WILDCARDS. rsconnect matches its entries as exact file or
# directory names. `data/*.bak-*` therefore matched NOTHING, and a bare
#
#     rsconnect::deployApp(appDir = ".")
#
# walks the directory and ships everything it finds — on 2026-07-12 that was 73 files and a
# 26.5 MB bundle, including ~40 MB of `iqsa_deploy.db.bak-*` snapshots and .claude/. Worse,
# `writeManifest()` and `deployApp()` DO NOT SHARE A FILE LIST: fixing the manifest does not fix
# the deploy. The only thing that controls what ships is an explicit `appFiles`.
#
# So the file list lives here, once, and both the manifest and the deploy read it.
# ─────────────────────────────────────────────────────────────────────────────────────────────

APP_ID  <- "019cdf36-3a62-2fb8-e316-3b0a7716e0cb"
ACCOUNT <- "jeremyfarrell"
SERVER  <- "connect.posit.cloud"

# Take everything, then EXCLUDE — which is precisely the globbing .rscignore cannot do.
#
# Do NOT replace this with a hand-curated allow-list. I tried; it silently dropped nine files the
# app actually serves (routes/*.json, the map animation, the .ris export, README.md), and a
# too-SMALL deployment surface breaks the app just as surely as a too-large one ships junk.
# An exclude-list fails safe: a new app file ships by default, and only known garbage is dropped.
EXCLUDE <- paste(
  "\\.bak",                 # every data/iqsa_deploy.db.bak-* and *.rds.bak-* snapshot (~40 MB)
  "_backup_",
  "^\\.claude/",            # local agent settings
  "^prompts/",              # scratch prompts
  "^rsconnect/",            # deployment metadata, not app content
  "^renv/", "^\\.git/",
  "\\.DS_Store$", "\\.Rhistory$", "\\.Rproj",
  "^\\.gitignore$", "^\\.rscignore$", "^deploy\\.R$",   # repo/deploy machinery, not app content
  "^data/.*\\.(md|txt)$",   # scratch notes in data/ — the app reads only .db/.rds/.csv/.json/.ris
  sep = "|"
)

APP_FILES <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
APP_FILES <- APP_FILES[!grepl(EXCLUDE, APP_FILES)]
APP_FILES <- unique(APP_FILES[file.exists(APP_FILES)])

leaked <- grep("\\.bak|^\\.claude|^prompts/|^rsconnect/", APP_FILES, value = TRUE)
if (length(leaked))
  stop("refusing to deploy backups or scratch files:\n  ", paste(leaked, collapse = "\n  "))
if (!all(c("app.R", "data/iqsa_deploy.db",
           "data/precomputed_bayesian_results.rds",
           "data/precomputed_confound_results.rds") %in% APP_FILES))
  stop("the app's own startup files are missing from the deployment surface — refusing")

cat(sprintf("Deployment surface: %d files, %.1f MB\n",
            length(APP_FILES), sum(file.size(APP_FILES)) / 1e6))

rsconnect::writeManifest(appDir = ".", appFiles = setdiff(APP_FILES, "manifest.json"))
cat("manifest.json refreshed\n")

if ("--manifest" %in% commandArgs(trailingOnly = TRUE)) quit(status = 0)

rsconnect::deployApp(
  appDir = ".", appFiles = APP_FILES,
  appId = APP_ID, account = ACCOUNT, server = SERVER,
  forceUpdate = TRUE, launch.browser = FALSE
)
