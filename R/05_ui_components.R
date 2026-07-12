# UI component functions — CSS, header, tabs, footer

ui_css <- function() {
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

      /* Fast hover tooltip for text reuse symbols */
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
  )
}

ui_header <- function() {
  div(class = "app-header",
      h2("The Ma\u0161riq\u012b and Ma\u0121rib\u012b Pedagogical Canons of Qur\u02beanic Reading Traditions"),
      p("Exploring the Development of the Classical ", tags$em("qir\u0101\u02be\u0101t"), " Corpus")
  )
}

ui_tab_home <- function() {
  tabPanel(
    title = "Home",
    value = "home",
    br(),

    div(class = "home-section",
      h3("Introduction"),
      p(class = "home-blurb",
        "This study reframes the development of the ", tags$em("qir\u0101\u02be\u0101t"),
        " discipline between the 4th and 7th centuries AH as the simultaneous emergence of two ",
        "geographically delimited pedagogical canons\u2014Ma\u0161riq\u012b and Ma\u0121rib\u012b\u2014rather than a single ",
        "tradition punctuated by the interventions of exceptional individuals. The argument rests on a ",
        "survey of documented ", tags$em("qir\u0101\u02be\u0101t"), " works composed between the activity of ",
        "Ibn Mu\u01e7\u0101hid (d. 324) and ca. 650 AH, together with the authors who produced them; the results ",
        "are stored as a database and reproduced in the Appendices. Rigorous statistical analysis demonstrates ",
        "that an author’s region of origin, far more than scholarly mobility or work format, is the decisive ",
        "predictor of preference between the Set of 7 and the Set of 10+ Readings. A century-by-century ",
        "narrative reconstruction traces the divergent regional trajectories and correlates major inflection ",
        "points with documented geopolitical events. An analysis of pedagogical practice shows the same ",
        "divergence reproduced in titular vocabulary, in the architecture of paired curricular works, and in ",
        "distinctive regional orientations toward didactic poetry and grammatical theory. Two appendices ",
        "catalogue the underlying corpus: Appendix A presents the prosopography of ", tags$em("qir\u0101\u02be\u0101t"),
        " authors with full biographical references, and Appendix B inventories the works on each Set of ",
        "Readings."),
      p("Use the tabs above to:",
        tags$ul(
          tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'paper', {priority: 'event'});", "Read and download the full paper"),
            " (contents updated with each deployment)"),
          tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'corpus_explorer', {priority: 'event'});", "Corpus Explorer"),
            " \u2014 search, filter, and download the full bibliography"),
          tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'methodology', {priority: 'event'});", "Methodology"),
            " \u2014 a complete walk-through of the data, model, and interpretation"),
          tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'bayesian_analysis', {priority: 'event'});", "Bayesian Analysis"),
            " \u2014 interactive results of the multinomial regression model")
        )
      )
    ),

    div(class = "home-section",
      h3(HTML("Distribution of Literary Production (<em>qirāʾāt</em>)")),
      p("The animated map below shows the geographic distribution of qirāʾāt literary production across centuries."),
      imageOutput("home_animation", height = "auto")
    )
  )
}

ui_tab_paper <- function() {
  tabPanel(
    title = tagList(icon("file-lines"), "Paper"),
    value = "paper",
    br(),
    div(class = "home-section",
      p(tags$a(href = "manuscript.pdf", target = "_blank", download = NA,
               icon("file-pdf"), " Download the paper (PDF)"),
        " — the full manuscript, updated with each deployment."),
      tags$iframe(
        src = "paper.html",
        style = "width:100%; height:1100px; border:1px solid #ddd; border-radius:4px;",
        title = "The Rise of Mašriqī and Maġribī Pedagogical Canons"
      )
    )
  )
}

ui_tab_corpus <- function() {
  tabPanel(
    title = tagList(icon("book"), "Corpus Explorer"),
    value = "corpus_explorer",
    br(),

    uiOutput("data_status_badge"),

    p("Showing ", textOutput("corpus_works_count", inline = TRUE),
      " works on Qur\u02beanic Reading Traditions (4th\u20137th c. AH). ",
      "For definitions of the Sets of Readings, regions, and the statistical model, see the ",
      tags$a(href = "#", onclick = "Shiny.setInputValue('nav_to', 'methodology', {priority: 'event'});", "Methodology"),
      " tab."),
    br(),

    h4(icon("chart-bar"), " Data Distribution"),
    fluidRow(
      column(4, plotlyOutput("data_dist_outcome", height = "280px")),
      column(4, plotlyOutput("data_dist_geo", height = "280px")),
      column(4, plotlyOutput("data_dist_century", height = "280px"))
    ),

    br(),

    div(class = "card",
      div(class = "card-header", icon("search"), " Corpus Search"),
      div(class = "card-body",
        fluidRow(
          column(12, textInput("search_all", "Search Title or Author:",
            placeholder = "tadhkirah, Shatibi, \u062a\u0630\u0643\u0631\u0629, \u0627\u0644\u0634\u0627\u0637\u0628\u064a, or \u0161\u0101\u1e6dib\u012b..."))
        ),
        tags$small(class = "text-muted", style = "display: block; margin-bottom: 10px;",
          "Flexible search: use Arabic script, digraphs (dh, gh, sh, th, kh), or simplified Latin (tadhkira = ta\u1e0fkira = \u062a\u0630\u0643\u0631\u0629)"
        ),
        fluidRow(
          column(3, selectizeInput("filter_system", "Reading Set:",
            choices = c("7", "7+1", "10+"),
            multiple = TRUE,
            options = list(placeholder = "All sets..."))),
          column(3, selectizeInput("filter_region", "Origin:",
            choices = c("Ma\u0121rib" = "ma\u0121rib", "Ma\u0161riq" = "ma\u0161riq", "Inter-regional" = "inter-regional"),
            multiple = TRUE,
            options = list(placeholder = "All regions..."))),
          column(3, selectizeInput("filter_type", "Work Type:",
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "All types..."))),
          column(3, sliderInput("filter_century", "Century (AH):", min = 4, max = 7, value = c(4, 7), step = 1))
        ),
        fluidRow(
          column(12, actionButton("clear_filters", "Clear Filters", icon = icon("times"), class = "btn-secondary"))
        ),
        hr(),
        uiOutput("corpus_results_count"),
        DTOutput("enhanced_data_table"),
        br(),
        fluidRow(
          column(4, downloadButton("download_csv", "Download CSV", class = "btn-primary btn-block")),
          column(4, downloadButton("download_json", "Download JSON", class = "btn-info btn-block")),
          column(4, downloadButton("download_ris", "Full bibliography (RIS)", class = "btn-success btn-block"))
        ),
        p(class = "text-muted", style = "font-size: 11px; margin-top: 8px;",
          "CSV and JSON export the works currently shown by your filters; the RIS file is the complete curated bibliography for Zotero/reference managers.")
      )
    )
  )
}

ui_tab_methodology <- function() {
  tabPanel(
    title = tagList(icon("graduation-cap"), "Methodology"),
    value = "methodology",
    br(),
    div(class = "card",
      div(class = "card-header", "Bayesian Multinomial Model & Jensen-Shannon Divergence"),
      div(class = "card-body",
        hr(),

        p("We estimate the association between regional scholarly affiliation (Ma\u0161riq / Ma\u0121rib) and ",
          "Qur\u02beanic Set of Readings (7, 7+1, 10+) using Bayesian multinomial logistic regression with an ",
          "optional century covariate. Regional divergence across time is quantified via Jensen-Shannon ",
          "Divergence (JSD) computed from posterior predictive distributions. The model is fit in ",
          strong("Stan"), " via ", strong("cmdstanr"), "; results are pre-computed and serialized as RDS ",
          "for deployment."),
        hr(),

        h3("1. Data Structure"),
        p(strong("N"), " = 173 works on Qur\u02beanic reading traditions, 4th\u20137th c. AH. ",
          strong("Outcome:"), " categorical Set of Readings ", em("y"), " \u2208 {7, 7+1, 10+}. ",
          strong("Predictor:"), " region (geo \u2208 {0 = Ma\u0121rib, 1 = Ma\u0161riq}). ",
          strong("Covariate:"), " author's death century (continuous, mean-centered)."),

        h4("Observed Counts"),
        tags$table(class = "table table-bordered table-sm", style = "max-width: 500px;",
          tags$thead(tags$tr(
            tags$th("Set"), tags$th("Ma\u0121rib"), tags$th("Ma\u0161riq"), tags$th("Total")
          )),
          tags$tbody(
            tags$tr(tags$td("7"), tags$td("49"), tags$td("39"), tags$td("88")),
            tags$tr(tags$td("7+1"), tags$td("10"), tags$td("17"), tags$td("27")),
            tags$tr(tags$td("10+"), tags$td("5"), tags$td("53"), tags$td("58")),
            tags$tr(tags$td(strong("Total")), tags$td(strong("64")), tags$td(strong("109")), tags$td(strong("173")))
          )
        ),
        p("Notable imbalance: 109 Ma\u0161riq vs. 64 Ma\u0121rib works; the Ma\u0121rib \u00d7 10+ cell contains only 5 observations. ",
          strong("Assumption:"), " works are treated as independent observations. No author-level hierarchy is modeled; ",
          "this is an acknowledged limitation given that some authors contribute multiple works."),
        hr(),

        h3("2. Model Specification"),
        p("Multinomial logistic regression with ", em("K"), " = 3 response categories and the ",
          strong("10+ Set"), " as the reference (\u03b7_K = 0). For each non-reference category ",
          em("k"), " \u2208 {7, 7+1}:"),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
          "\u03b7_k = \u03b1_k + \u03b2_k \u00d7 geo + \u03b3_k \u00d7 (century \u2212 c\u0304)"),
        p("where \u03b1_k is the intercept (log-odds of Set ", em("k"), " vs. 10+ at geo = 0, century = c\u0304), ",
          "\u03b2_k is the regional effect, and \u03b3_k is the century slope (included only in the century model). ",
          "Category probabilities are obtained via the softmax link:"),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
          "P(y = k) = exp(\u03b7_k) / \u03a3\u2c7c exp(\u03b7\u2c7c),    \u03b7_K = 0"),
        p("The region-only model has 4 free parameters (\u03b1\u2081, \u03b1\u2082, \u03b2\u2081, \u03b2\u2082); the century model has 6 ",
          "(adding \u03b3\u2081, \u03b3\u2082). The choice of reference category is arbitrary and does not affect predicted probabilities."),

        h4("Stan Code (Century Model)"),
        p("The simple model omits ", code("beta_cent"), " and the ", code("century"), " data."),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 0.95em; overflow-x: auto;",
"data {
  int<lower=1> N;                          // number of works (173)
  int<lower=2> K;                          // number of categories (3)
  array[N] int<lower=1,upper=K> y;         // observed system for each work
  array[N] int<lower=0,upper=1> geo;       // region: 0 = Ma\u0121rib, 1 = Ma\u0161riq
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

        h3("3. Priors & Prior Predictive Checks"),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
          "\u03b1_k ~ Normal(0, 5)     intercepts\n\u03b2_k ~ Normal(0, 2)     regional effects\n\u03b3_k ~ Normal(0, 2)     century effects"),
        p("These are ", strong("weakly informative priors"), " following McElreath (2020, Ch. 4). On the log-odds scale, ",
          "Normal(0, 5) spans probabilities from ~0.007 to ~0.993; Normal(0, 2) permits shifts large enough to accommodate any ",
          "plausible regional or temporal effect while regularizing against extreme values in sparse cells ",
          "(e.g., Ma\u0121rib \u00d7 10+ with 4 observations)."),
        p(strong("Prior predictive simulation:"), " Drawing parameter vectors from the priors and passing them through ",
          "the softmax yields approximately uniform distributions over category probabilities \u2014 confirming that the priors ",
          "do not inadvertently favor any particular allocation. This follows the prior predictive checking workflow ",
          "recommended by McElreath (2020, Ch. 4) and Gelman et al. (2013, Ch. 6) as a quality control step before ",
          "conditioning on data."),
        hr(),

        h3("4. Estimation & Convergence Diagnostics"),
        p("The posterior is sampled via the ", strong("No-U-Turn Sampler (NUTS)"),
          " (Hoffman & Gelman, 2014), a variant of Hamiltonian Monte Carlo with automatic trajectory-length ",
          "adaptation. Stan compiles the model to C++ with automatic differentiation for gradient evaluation."),
        tags$ul(
          tags$li(strong("Chains:"), " 4 independent chains from dispersed initial values"),
          tags$li(strong("Iterations:"), " 2,000 per chain (1,000 warmup + 1,000 sampling) = 4,000 posterior draws"),
          tags$li(strong("Convergence criteria"), " (per McElreath, 2020, Ch. 9):",
            tags$ul(
              tags$li("R\u0302 < 1.01 for all parameters"),
              tags$li("ESS", tags$sub("bulk"), " > 400 and ESS", tags$sub("tail"), " > 400"),
              tags$li("Zero divergent transitions")
            ))
        ),
        p(strong("Posterior predictive check:"), " For each posterior draw, simulated counts per region \u00d7 Set cell ",
          "are compared to observed counts. Adequate fit requires observed values to fall within the 95% posterior predictive interval."),
        hr(),

        h3("5. Jensen-Shannon Divergence"),
        p("The multinomial model yields per-category probability vectors for each region, but does not directly quantify the ",
          em("magnitude"), " of distributional divergence between Ma\u0161riq and Ma\u0121rib across time. ",
          "Jensen-Shannon Divergence (JSD; Lin, 1991) provides a single scalar summary in bits."),
        p(strong("Scope:"), " JSD operates on the 3-category Set distribution alone \u2014 it does not incorporate genre, format, ",
          "mobility, or other variables coded in this study."),

        h4("Definition"),
        p("For discrete distributions ", em("P"), " and ", em("Q"), " over ", em("K"), " categories:"),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
          "JSD(P \u2016 Q) = \u00bd KL(P \u2016 M) + \u00bd KL(Q \u2016 M)\n\nwhere  M = \u00bd(P + Q)\n       KL(A \u2016 B) = \u03a3_k  a_k log\u2082(a_k / b_k)\n\nwith convention 0 \u00b7 log\u2082(0 / b) = 0"),
        p("JSD is symmetric, bounded in [0, 1] bits (for the base-2 logarithm), and defined even when one distribution ",
          "assigns zero probability to a category (unlike raw KL divergence)."),

        h4("Model-Based JSD"),
        p("For each posterior draw ", em("s"), " \u2208 {1, \u2026, 4000} and each century ", em("c"), ", the softmax-transformed ",
          "probability vectors ", em("P"), tags$sub("Ma\u0121rib"), tags$sup("(s,c)"), " and ",
          em("P"), tags$sub("Ma\u0161riq"), tags$sup("(s,c)"), " are extracted from the multinomial model. ",
          "JSD", tags$sup("(s,c)"), " is computed for each draw; summaries report the posterior mean, CI", tags$sub("50"),
          ", and CI", tags$sub("95"), " per century. This variant inherits full model uncertainty, including covariance between ",
          "parameters."),

        h4("Dirichlet-Smoothed JSD (Model-Free Robustness Check)"),
        p("As a specification check independent of the multinomial model, we compute JSD from Dirichlet-smoothed empirical ",
          "distributions. For each century \u00d7 region cell, 4,000 probability vectors are drawn from:"),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
          "P ~ Dirichlet(1 + n\u2081, 1 + n\u2082, 1 + n\u2083)"),
        p("where ", em("n_k"), " are the observed counts \u2014 a conjugate update with a uniform (flat Dirichlet) prior. ",
          "JSD is computed pairwise across draws, yielding mean, CI", tags$sub("50"), ", and CI", tags$sub("95"),
          " per century. Agreement between the model-based and Dirichlet-smoothed JSD trajectories validates the multinomial ",
          "specification; divergence would signal model misfit."),
        p(strong("Implementation:"), " ", code("compute_jsd()"), " and ", code("draw_dirichlet()"), " functions in ",
          code("scripts/extract_bayesian_results.R"), "."),

        h4("Constrained Maximum and Normalization"),
        p("The theoretical JSD maximum of 1 bit is unattainable when both regions produce works on a shared category \u2014 ",
          "as is the case with the Set of 7 throughout the period under study. To provide an interpretable scale, we compute ",
          "a ", em("constrained"), " theoretical maximum for each posterior draw: the JSD that would obtain if both regions ",
          "allocated their observed Set of 7 share exactly as predicted, but placed all remaining probability mass in ",
          "non-overlapping categories:"),
        tags$pre(style = "background: #f8f9fa; padding: 15px; border-radius: 4px; font-size: 1.05em;",
          "Ma\u0121rib_max = (p7_magh, 1 - p7_magh, 0)\nMa\u0161riq_max = (p7_mash, 0, 1 - p7_mash)"),
        p("Normalized JSD = JSD / JSD", tags$sub("max"), " expresses the observed divergence as a proportion of the ",
          "maximum possible divergence given the shared Set of 7 baseline. A value of 100% would mean the regions are ",
          "as different as they could possibly be, given their shared commitment to the Set of 7."),
        p("The constrained maximum itself rises over the period (from ~0.38 to ~0.62 bits, model-based) because the ",
          "Set of 7 shrinks as a share of total production \u2014 particularly in the Ma\u0161riq \u2014 which mechanically widens the ",
          "ceiling for divergence."),
        hr(),

        h3("6. Software & Reproducibility"),
        tags$table(class = "table table-bordered table-sm",
          tags$thead(tags$tr(tags$th("Component"), tags$th("Package / Tool"), tags$th("Role"))),
          tags$tbody(
            tags$tr(tags$td("Model fitting"), tags$td(code("Stan"), " via ", code("cmdstanr")),
              tags$td(code("categorical_logit"), " likelihood; NUTS sampling")),
            tags$tr(tags$td("Diagnostics"), tags$td(code("posterior")),
              tags$td("R\u0302, ESS", tags$sub("bulk"), ", ESS", tags$sub("tail"))),
            tags$tr(tags$td("Visualization"), tags$td(code("ggplot2"), " + ", code("plotly")),
              tags$td("Static and interactive plots")),
            tags$tr(tags$td("Data access"), tags$td(code("DBI"), " / ", code("RSQLite")),
              tags$td("SQLite database interface")),
            tags$tr(tags$td("Application"), tags$td(code("shiny"), " / ", code("shinyjs")),
              tags$td("Interactive web framework"))
          )
        ),
        p("Development followed McElreath (2020) ", em("Statistical Rethinking"), " (2nd ed.); the prototype was built with ",
          code("rethinking::ulam()"), " and subsequently ported to ", code("cmdstanr"), " for deployment flexibility. ",
          "Pre-computed posterior draws and derived quantities are serialized as RDS files, enabling deployment without ",
          "a Stan toolchain."),
        hr(),

        h3("7. References"),
        tags$ul(
          tags$li("Carpenter, B., et al. (2017). Stan: A Probabilistic Programming Language. ",
            em("Journal of Statistical Software"), ", 76(1)."),
          tags$li("Gelman, A., et al. (2013). ", em("Bayesian Data Analysis"), " (3rd ed.). CRC Press."),
          tags$li("Hoffman, M. D. & Gelman, A. (2014). The No-U-Turn Sampler: Adaptively Setting Path Lengths in Hamiltonian Monte Carlo. ",
            em("JMLR"), ", 15, 1593\u20131623."),
          tags$li("Lin, J. (1991). Divergence Measures Based on the Shannon Entropy. ",
            em("IEEE Transactions on Information Theory"), ", 37(1), 145\u2013151."),
          tags$li("McElreath, R. (2020). ", em("Statistical Rethinking: A Bayesian Course with Examples in R and Stan"),
            " (2nd ed.). CRC Press. Companion package: ",
            tags$a(href = "https://github.com/rmcelreath/rethinking", target = "_blank", "github.com/rmcelreath/rethinking"), ".")
        )
      )
    )
  )
}

ui_tab_bayesian <- function() {
  tabPanel(
    title = tagList(icon("chart-line"), "Bayesian Analysis"),
    value = "bayesian_analysis",
    br(),

    div(class = "card-navigation",
      actionButton("bayes_prev", icon("arrow-left"), class = "btn-secondary"),
      uiOutput("bayes_card_indicator"),
      actionButton("bayes_next", icon("arrow-right"), class = "btn-secondary")
    ),

    uiOutput("bayes_current_card"),

    conditionalPanel(
      condition = "output.model_fitted",
      downloadButton("save_model", "Save Current Model", class = "btn-success", style = "margin-top: 10px;")
    )
  )
}

ui_tab_acknowledgements <- function() {
  tabPanel(
    title = "Acknowledgements",
    value = "acknowledgements",
    br(),
    div(class = "card",
      div(class = "card-header", "Acknowledgements"),
      div(class = "card-body",
        h4("Funding"),
        p("This research is supported by the ",
          tags$a(href = "https://erc.europa.eu/", target = "_blank", rel = "noopener",
                 "European Research Council (ERC)"),
          " under the European Union's ",
          tags$a(href = "https://research-and-innovation.ec.europa.eu/funding/funding-opportunities/funding-programmes-and-open-calls/horizon-europe_en",
                 target = "_blank", rel = "noopener", "Horizon Europe"),
          " research and innovation programme (",
          tags$a(href = "https://cordis.europa.eu/project/id/101044127", target = "_blank", rel = "noopener",
                 "Grant Agreement No. 101044127"),
          ", QurCan)."),
        br(),
        h4("Institutional Support"),
        p("This project is based at the ",
          tags$a(href = "https://www.universiteitleiden.nl/en/humanities/leiden-university-centre-for-linguistics",
                 target = "_blank", rel = "noopener",
                 "Leiden University Centre for Linguistics (LUCL)"),
          ", ",
          tags$a(href = "https://www.universiteitleiden.nl/en", target = "_blank", rel = "noopener",
                 "Leiden University"),
          ", ",
          tags$a(href = "https://www.universiteitleiden.nl/en/humanities", target = "_blank", rel = "noopener",
                 "Faculty of Humanities"),
          "."),
        br(),
        h4("Data Sources"),
        p("Geographic coordinate data is derived from the ",
          tags$a(href = "https://althurayya.github.io/", target = "_blank", rel = "noopener",
                 "Thurayya Gazetteer"),
          " project. Bibliographic data has been compiled from primary sources and existing scholarly catalogues."),
        br(),
        h4("Technical"),
        p("This application was built using ",
          tags$a(href = "https://shiny.posit.co/", target = "_blank", rel = "noopener", "R Shiny"),
          " with Bayesian modeling via ",
          tags$a(href = "https://mc-stan.org/", target = "_blank", rel = "noopener", "CmdStan"),
          ". Geographic visualizations use ",
          tags$a(href = "https://leafletjs.com/", target = "_blank", rel = "noopener", "Leaflet.js"),
          ".")
      )
    )
  )
}

ui_footer <- function() {
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
        tags$img(src = "images/qurxan_logo.png", height = "60px", alt = "QurCan Project",
                 style = "opacity: 0.9; transition: opacity 0.2s;",
                 onmouseover = "this.style.opacity='1'",
                 onmouseout = "this.style.opacity='0.9'")
      )
    ),
    p(
      style = "margin-top: 15px; font-size: 0.85em; color: #666;",
      "Funded by the European Research Council (ERC) under the European Union's Horizon Europe programme (Grant Agreement No. 101044127, QurCan)"
    ),
    p(
      style = "margin-top: 10px; font-size: 0.8em; color: #888;",
      HTML('This work is licensed under a <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank" style="color: #888;">CC BY-NC-SA 4.0</a> license.')
    )
  )
}
