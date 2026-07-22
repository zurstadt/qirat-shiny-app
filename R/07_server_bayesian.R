# Bayesian Analysis server logic
# Returns list(posterior_preds = <reactive>) for use by diagnostics module

server_bayesian <- function(input, output, session, rv) {

  # Card navigation buttons
  observeEvent(input$bayes_prev, {
    if (rv$bayes_current_card > 1) {
      rv$bayes_current_card <- rv$bayes_current_card - 1
    }
  })

  observeEvent(input$bayes_next, {
    if (rv$bayes_current_card < 6) {
      rv$bayes_current_card <- rv$bayes_current_card + 1
    }
  })

  observeEvent(input$bayes_next_from_card1, { rv$bayes_current_card <- 2 })
  observeEvent(input$bayes_next_from_card2, { rv$bayes_current_card <- 3 })
  observeEvent(input$bayes_next_from_card3, { rv$bayes_current_card <- 4 })
  observeEvent(input$bayes_next_from_card4, { rv$bayes_current_card <- 5 })
  observeEvent(input$bayes_next_from_card5, { rv$bayes_current_card <- 6 })

  # Card indicator
  output$bayes_card_indicator <- renderUI({
    span(class = "card-indicator",
      sprintf("Card %d of 6", rv$bayes_current_card)
    )
  })

  # Card content renderer
  output$bayes_current_card <- renderUI({
    card_num <- rv$bayes_current_card
    switch(card_num,
      render_bayes_card_1(),
      render_bayes_card_2(),
      render_bayes_card_3(),
      render_bayes_card_4(),
      render_bayes_card_5(),
      render_bayes_card_6()
    )
  })

  # Card 1: Introduction
  render_bayes_card_1 <- function() {
    div(class = "card",
      div(class = "card-header", "Introduction to Bayesian Analysis"),
      div(class = "card-body",
        h4("Why Bayesian?"),
        p("Bayesian analysis provides a principled framework for quantifying uncertainty in our estimates. Unlike traditional frequentist approaches that give point estimates with confidence intervals, Bayesian methods produce full probability distributions that directly answer questions like: 'Given the data, what is the probability that regional affiliation influenced the choice of Set of Readings?'"),

        h4("Research Questions"),
        tags$ol(
          tags$li("Did regional affiliation (Ma\u0121rib vs. Ma\u0161riq) influence which Sets of Readings scholars chose to document?"),
          tags$li("How confident can we be in observed regional differences?"),
          tags$li("Did patterns of regional preference change over the study period (4th-7th centuries AH)?")
        ),

        h4("Key Advantages"),
        tags$ul(
          tags$li(tags$strong("Full posterior distributions"), " - Not just point estimates, but complete probability distributions for each parameter"),
          tags$li(tags$strong("Natural uncertainty quantification"),
                  sprintf(" - Credible intervals have intuitive interpretation: 'There is a %s probability the true value lies in this range'", CI_LABEL)),
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
        h4(class = "section-header-bold", "Understanding the Model"),
        p("This analysis uses Bayesian multinomial logistic regression to model the probability that a work describes a particular Set of Readings (7, 7+1, or 10+) based on the author's regional location and the century of production."),

        tags$h5("Model Structure:"),
        tags$ul(
          tags$li(tags$strong("Outcome:"), " Reading set (7, 7+1, or 10+) - the dependent variable"),
          tags$li(tags$strong("Predictor:"), " Regional affiliation (Ma\u0121rib vs. Ma\u0161riq) - the main independent variable"),
          tags$li(tags$strong("Covariate:"), " Death century (4th-7th c. AH, centered) - temporal control variable")
        ),

        hr(),

        h4(class = "section-header-bold", "Current Configuration"),
        uiOutput("model_config_summary"),

        hr(),

        conditionalPanel(
          condition = "output.model_fitted",
          h4(class = "section-header-bold", "Model Parameter Estimates"),
          div(class = "info-box",
            p("The table below shows the posterior distributions of the model's regression coefficients:"),
            tags$ul(
              tags$li(tags$strong("Alpha (Intercepts):"), " Baseline log-odds for each set vs. the reference (10+ readings)"),
              tags$li(tags$strong("Beta_geo (Regional Effects):"), " How being in Ma\u0161riq changes the log-odds. Positive = Ma\u0161riq preference; negative = Ma\u0121rib preference."),
              tags$li(tags$strong("Beta_cent (Temporal Effects):"), " How each additional century changes the log-odds.")
            ),
            p(sprintf("When the %s credible interval excludes zero, we have strong evidence for that effect.",
                      CI_LABEL))
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

          div(class = "info-box",
            tags$h5("Reading the Plot:"),
            tags$ul(
              tags$li("Each panel represents a different century (4th through 7th AH)"),
              tags$li("Bar height shows the predicted probability (0-1 scale)"),
              tags$li("Colors distinguish regions: ", span(style = "color: #56B4E9;", "Ma\u0121rib"), " vs. ", span(style = "color: #E69F00;", "Ma\u0161riq")),
              tags$li(sprintf("Error bars show %s credible intervals - ranges of plausible values", CI_LABEL))
            ),
            p(tags$strong("Interpreting differences:"), " When error bars for Ma\u0121rib and Ma\u0161riq do not overlap for a given Set, this is suggestive of a regional difference. Note that non-overlapping marginal intervals are a conservative cue, not a formal test \u2014 the posterior contrast on the Regional Contrasts card provides the rigorous comparison.")
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

          uiOutput("contrast_display"),

          hr(),

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
              p("High correlations between \u03b1 and \u03b2_geo are structural (expected); very high correlations (>0.95) elsewhere may indicate identifiability issues", style = "color: gray;"),
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
        ),

        div(style = "text-align: right; margin-top: 20px;",
          actionButton("bayes_next_from_card4", "Next \u2192", class = "btn-primary")
        )
      )
    )
  }

  # Card 5: Regional Divergence (JSD)
  render_bayes_card_5 <- function() {
    div(class = "card",
      div(class = "card-header", "Regional Divergence (JSD)"),
      div(class = "card-body",
        h4("How Different Are Ma\u0121rib and Ma\u0161riq?"),
        p("Jensen-Shannon Divergence (JSD) summarizes the overall dissimilarity between two probability distributions into a single number between 0 (identical) and 1 (maximally distinct) bits. Here we compute JSD between the regional distributions of Sets of Readings per century using two complementary approaches:"),
        tags$ul(
          tags$li(tags$strong("Model-based:"), " Derived from the Bayesian multinomial regression posterior. Each MCMC draw yields a JSD value, producing a full posterior distribution."),
          tags$li(tags$strong("Dirichlet-smoothed:"), " A model-free robustness check using Dirichlet(1 + counts) posteriors directly from the raw data. Wider credible intervals where data is sparse (e.g., 7th century).")
        ),
        p("Two views are shown. ", tags$strong("Absolute divergence"), " (top) plots JSD in bits against the ",
          tags$strong("constrained theoretical maximum"), " (dashed) — the highest JSD achievable given that both regions ",
          "continue to produce Set of 7 works. ", tags$strong("Normalized divergence"), " (bottom) re-expresses the same ",
          "trajectories as a percentage of that constrained maximum. Agreement between the model-based and Dirichlet-smoothed ",
          "curves validates the multinomial specification. See the Methodology tab for details."),

        h5(class = "section-header-bold", "Absolute divergence (bits)"),
        fluidRow(
          column(6,
            p(tags$strong("Model-based"), style = paste0("text-align:center; margin-bottom:2px; color:", JSD_METHOD_COLORS[["Model-based"]], ";")),
            plotlyOutput("jsd_absolute_model_plot", height = "340px")),
          column(6,
            p(tags$strong("Dirichlet-smoothed"), style = paste0("text-align:center; margin-bottom:2px; color:", JSD_METHOD_COLORS[["Dirichlet-smoothed"]], ";")),
            plotlyOutput("jsd_absolute_dir_plot", height = "340px"))
        ),
        br(),
        h5(class = "section-header-bold", "Normalized divergence (% of constrained maximum)"),
        fluidRow(
          column(6,
            p(tags$strong("Model-based"), style = paste0("text-align:center; margin-bottom:2px; color:", JSD_METHOD_COLORS[["Model-based"]], ";")),
            plotlyOutput("jsd_normalized_model_plot", height = "340px")),
          column(6,
            p(tags$strong("Dirichlet-smoothed"), style = paste0("text-align:center; margin-bottom:2px; color:", JSD_METHOD_COLORS[["Dirichlet-smoothed"]], ";")),
            plotlyOutput("jsd_normalized_dir_plot", height = "340px"))
        ),
        br(),
        h5(class = "section-header-bold", "Summary Table"),
        DT::dataTableOutput("jsd_table"),
        div(style = "text-align: right; margin-top: 20px;",
          actionButton("bayes_next_from_card5", "Next \u2192", class = "btn-primary")
        )
      )
    )
  }

  # Card 6: Confound Analysis
  render_bayes_card_6 <- function() {
    div(class = "card",
      div(class = "card-header", "Confound Analysis"),
      div(class = "card-body",
        h4("Is the Regional Effect Robust?"),
        p("The base model identifies region as a strong predictor of Set preference. ",
          "Four extended models test whether this effect survives after controlling for plausible confounds:"),
        tags$ol(
          tags$li(tags$strong("Sub-region model:"), " Adds a varying (partial-pooling) intercept for city/province-level milieu ",
                  "(e.g., Iraq, al-Andalus, \u0160\u0101m, Egypt). Tests whether a single sub-region (e.g., Baghdad) drives the Ma\u0161riq effect."),
          tags$li(tags$strong("Mobility model:"), " Adds a binary predictor for inter-regional travel. ",
                  "Tests whether mobile scholars blur the regional signal."),
          tags$li(tags$strong("Format model:"), " Adds a categorical predictor for work genre ",
                  "(catalogue, expansion, compression, mufradah, poem, \u02bead\u0101\u02be). ",
                  "Tests whether genre explains Set preference independently of region."),
          tags$li(tags$strong("Mobility \u00d7 Century model:"), " Adds an interaction between mobility and century. ",
                  "Tests whether the behaviour of inter-regional scholars changed over time\u2014specifically, whether later mobile scholars ",
                  "adhered more rigidly to region-of-origin norms than their earlier counterparts.")
        ),
        p("Each extended model is compared to the base (Region + Century) model via:"),
        tags$ul(
          tags$li("LOO-CV (leave-one-out cross-validation) for predictive accuracy"),
          tags$li("Change in the Region coefficient (does it shrink?)"),
          tags$li("Posterior probability that Region > 0 (does it remain decisive?)")
        ),

        hr(),

        if (!is.null(CONFOUND)) {
          tagList(
            h4(class = "section-header-bold", "Region Coefficient Stability"),
            p("The forest plot shows the Region coefficient (log-odds of Ma\u0161riq effect) across all five models. ",
              sprintf("Thick bars = %s credible interval; thin bars = %s CI. Stability across models means the regional effect is not an artifact of confounds.",
                      CI_INNER_LABEL, CI_LABEL)),
            plotOutput("confound_forest_plot", height = "400px"),

            hr(),

            h4(class = "section-header-bold", "LOO-CV Model Comparison"),
            p("Leave-one-out cross-validation compares predictive accuracy. ",
              "Lower LOOIC is better; \u0394LOOIC shows improvement relative to the best model."),
            DT::dataTableOutput("confound_loo_table"),

            hr(),

            h4(class = "section-header-bold", "Coefficient Stability"),
            p("How much does the Region coefficient change when each confound is added?"),
            DT::dataTableOutput("confound_stability_table"),

            hr(),

            h4(class = "section-header-bold", "Mobility \u00d7 Century Interaction"),
            # THE VERDICT IS COMPUTED, NOT TYPED.
            #
            # This card used to assert, in prose, that the interaction model was "the best-fitting
            # by LOO-CV" and that the mobility effect "intensifies over time". Both were true only
            # of a model whose REGION COVARIATE WAS MIS-CODED: an unanchored grepl() put every
            # eastward-travelling Ma\u0121rib\u012b (al-D\u0101n\u012b included) on the Ma\u0161riq side, so the mobile
            # authors were partly DEFINING the Ma\u0161riq category and the interaction was partly
            # circular. With region coded as origin it collapses to a null (see the 2026-07-12
            # refit, root commit 66dc132). The footnote in the paper now reports it as a tested
            # null, and so does this card \u2014 by reading the posterior instead of describing it.
            if (!is.null(CONFOUND$interaction)) {
              int_row  <- CONFOUND$interaction[CONFOUND$interaction$category == "7", ][1, ]
              decisive <- (int_row$interaction_ci_lower > 0) || (int_row$interaction_ci_upper < 0)
              best_loo <- as.character(
                CONFOUND$loo_table$model[which.min(CONFOUND$loo_table$looic)])
              tagList(
                p("Does the compositional behaviour of inter-regional scholars change over time? ",
                  "This model adds an interaction between mobility and century to test exactly that. ",
                  sprintf("The best-fitting model by LOO-CV is in fact the %s model.", best_loo)),
                DT::dataTableOutput("confound_interaction_table"),
                br(),
                if (decisive) {
                  p(tags$strong("The interaction is decisive."),
                    sprintf(" Its %s credible interval excludes zero: ", CI_LABEL),
                    "in later centuries, inter-regional scholars are measurably ", tags$em("more"),
                    " likely to produce works on the Set of 7 (relative to 10+) than their sedentary peers.")
                } else {
                  p(tags$strong("The interaction is not supported."),
                    sprintf(" The posterior leans toward a Set-of-7 skew among later mobile scholars (P = %.0f%%), but its %s credible interval spans zero [%.2f, %.2f], so the data do not settle the question. ",
                            100 * int_row$interaction_prob_positive, CI_LABEL,
                            int_row$interaction_ci_lower, int_row$interaction_ci_upper),
                    "It is reported here as a null rather than omitted.")
                }
              )
            } else {
              NULL
            },

            hr(),

            h4(class = "section-header-bold", "Interpretation"),
            div(class = "info-box",
              p(CONFOUND$interpretation$summary)
            )
          )
        } else {
          div(class = "alert alert-info",
            icon("info-circle"),
            " Confound analysis results not yet available. Run scripts/confound_models.R and scripts/extract_confound_results.R to generate them."
          )
        }
      )
    )
  }

  # JSD_METHOD_COLORS is defined globally in R/01_constants.R (shared with the UI).

  # NB: do NOT put `text = hover_text` in the global aes — ggplotly splits a
  # geom_line into separate traces when the inherited text aesthetic varies
  # per row, and the connecting line vanishes. Apply `text` only to geom_point
  # and explicitly `group` the lines by method so the curve renders continuously.

  # --- Panel A: absolute divergence (bits), with constrained-max ceiling ---
  # --- JSD plots, split by method so each panel carries a single series ---
  # (Previously model-based + Dirichlet were overlaid on one axis, which the
  #  ribbons made too noisy to read. One method per panel, matching the
  #  manuscript's separate Figs 7-8.)
  jsd_abs_plotly <- function(d, dmax, method_label) {
    col <- unname(JSD_METHOD_COLORS[method_label]); if (is.na(col)) col <- "#0072B2"
    d$hover_text <- sprintf(
      paste0("<b>%s</b><br>Century: %sth c. AH<br>Mean JSD: %.3f<br>", CI_LABEL_FMT, " CI: [%.3f, %.3f]"),
      method_label, d$century, d$mean, d$ci_low, d$ci_high)
    dmax$hover_text <- sprintf(
      "<b>Constrained max</b><br>Century: %sth c. AH<br>Ceiling JSD: %.3f",
      dmax$century, dmax$mean)
    y_top <- max(d$ci_high, dmax$mean, na.rm = TRUE) * 1.08
    p <- ggplot(d, aes(x = century)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = col, alpha = 0.12) +
      geom_ribbon(aes(ymin = ci50_low, ymax = ci50_high), fill = col, alpha = 0.25) +
      geom_line(data = dmax, aes(y = mean), linewidth = 0.7,
                linetype = "dashed", color = "gray45") +
      geom_point(data = dmax, aes(y = mean, text = hover_text), size = 1.4,
                 shape = 21, stroke = 0.4, color = "gray45", fill = "white") +
      geom_line(aes(y = mean), color = col, linewidth = 0.9) +
      geom_point(aes(y = mean, text = hover_text), color = col, size = 2.5) +
      scale_x_continuous(breaks = PRECOMPUTED$centuries,
                         labels = paste0(PRECOMPUTED$centuries, "th c.")) +
      scale_y_continuous(limits = c(0, y_top)) +
      labs(x = "Century (AH)", y = "JSD (bits)") +
      theme_tufte_custom(base_size = 11)
    ggplotly(p, tooltip = "text") %>% plotly::layout(showlegend = FALSE)
  }

  jsd_norm_plotly <- function(d, method_label) {
    col <- unname(JSD_METHOD_COLORS[method_label]); if (is.na(col)) col <- "#0072B2"
    d$hover_text <- sprintf(
      paste0("<b>%s</b><br>Century: %sth c. AH<br>Normalized JSD: %.0f%%<br>", CI_LABEL_FMT, " CI: [%.0f%%, %.0f%%]"),
      method_label, d$century, d$mean * 100, d$ci_low * 100, d$ci_high * 100)
    p <- ggplot(d, aes(x = century)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = col, alpha = 0.12) +
      geom_ribbon(aes(ymin = ci50_low, ymax = ci50_high), fill = col, alpha = 0.25) +
      geom_line(aes(y = mean), color = col, linewidth = 0.9) +
      geom_point(aes(y = mean, text = hover_text), color = col, size = 2.5) +
      scale_x_continuous(breaks = PRECOMPUTED$centuries,
                         labels = paste0(PRECOMPUTED$centuries, "th c.")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                         labels = function(x) paste0(x * 100, "%")) +
      labs(x = "Century (AH)", y = "JSD (% of constrained max)") +
      theme_tufte_custom(base_size = 11)
    ggplotly(p, tooltip = "text") %>% plotly::layout(showlegend = FALSE)
  }

  output$jsd_absolute_model_plot <- renderPlotly({
    req(PRECOMPUTED, PRECOMPUTED$jsd_model_summary, PRECOMPUTED$jsd_max_model_summary)
    jsd_abs_plotly(PRECOMPUTED$jsd_model_summary, PRECOMPUTED$jsd_max_model_summary, "Model-based")
  })
  output$jsd_absolute_dir_plot <- renderPlotly({
    req(PRECOMPUTED, PRECOMPUTED$jsd_dir_summary, PRECOMPUTED$jsd_max_dir_summary)
    jsd_abs_plotly(PRECOMPUTED$jsd_dir_summary, PRECOMPUTED$jsd_max_dir_summary, "Dirichlet-smoothed")
  })
  output$jsd_normalized_model_plot <- renderPlotly({
    req(PRECOMPUTED, PRECOMPUTED$jsd_norm_model_summary)
    jsd_norm_plotly(PRECOMPUTED$jsd_norm_model_summary, "Model-based")
  })
  output$jsd_normalized_dir_plot <- renderPlotly({
    req(PRECOMPUTED, PRECOMPUTED$jsd_norm_dir_summary)
    jsd_norm_plotly(PRECOMPUTED$jsd_norm_dir_summary, "Dirichlet-smoothed")
  })

  # JSD summary table
  output$jsd_table <- DT::renderDataTable({
    req(PRECOMPUTED, PRECOMPUTED$jsd_model_summary, PRECOMPUTED$jsd_dir_summary)

    jsd_combined <- rbind(PRECOMPUTED$jsd_model_summary, PRECOMPUTED$jsd_dir_summary)
    jsd_max_combined <- rbind(PRECOMPUTED$jsd_max_model_summary, PRECOMPUTED$jsd_max_dir_summary)
    jsd_norm_combined <- rbind(PRECOMPUTED$jsd_norm_model_summary, PRECOMPUTED$jsd_norm_dir_summary)

    display_df <- data.frame(
      Method = jsd_combined$method,
      Century = paste0(jsd_combined$century, "th c. AH"),
      `Mean JSD` = sprintf("%.3f", jsd_combined$mean),
      `CI` = sprintf("[%.3f, %.3f]", jsd_combined$ci_low, jsd_combined$ci_high),
      `Constrained Max` = sprintf("%.3f", jsd_max_combined$mean),
      `Normalized` = sprintf("%.0f%%", jsd_norm_combined$mean * 100),
      check.names = FALSE
    )

    DT::datatable(display_df, rownames = FALSE, options = list(
      pageLength = 10, dom = "t", ordering = FALSE
    ))
  })

  # Confound forest plot
  output$confound_forest_plot <- renderPlot({
    req(CONFOUND)

    df <- CONFOUND$beta_geo_summary
    df$label <- paste0(df$category, " vs. 10+")
    df$label <- factor(df$label, levels = c("7 vs. 10+", "7+1 vs. 10+"))
    df$model <- factor(df$model, levels = rev(CONFOUND$model_names))

    SYSTEM_COLORS <- c("7" = "#0072B2", "7+1" = "#D55E00", "10+" = "#009E73")

    ggplot(df, aes(x = mean, y = model, color = label)) +
      geom_linerange(aes(xmin = ci_lower, xmax = ci_upper),
                     linewidth = 0.5, alpha = 0.7,
                     position = position_dodge(width = 0.5)) +
      geom_linerange(aes(xmin = ci50_lower, xmax = ci50_upper),
                     linewidth = 1.5, alpha = 0.9,
                     position = position_dodge(width = 0.5)) +
      geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
      scale_color_manual(
        values = c("7 vs. 10+" = unname(SYSTEM_COLORS["7"]),
                   "7+1 vs. 10+" = unname(SYSTEM_COLORS["7+1"])),
        name = NULL
      ) +
      labs(
        x = "Region Coefficient (log-odds, Mashriq effect)",
        y = NULL
      ) +
      theme_tufte_custom(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_text(size = 11),
        panel.grid.major.x = element_line(color = "gray90", linewidth = 0.25)
      )
  })

  # LOO-CV comparison table
  output$confound_loo_table <- DT::renderDataTable({
    req(CONFOUND)

    loo_df <- CONFOUND$loo_table
    best_looic <- min(loo_df$looic)

    display_df <- data.frame(
      Model = loo_df$model,
      `ELPD LOO` = sprintf("%.1f", loo_df$elpd_loo),
      `SE` = sprintf("%.1f", loo_df$se_elpd_loo),
      `LOOIC` = sprintf("%.1f", loo_df$looic),
      `Delta LOOIC` = sprintf("%.1f", loo_df$looic - best_looic),
      `p_loo` = sprintf("%.1f", loo_df$p_loo),
      check.names = FALSE
    )

    DT::datatable(display_df, rownames = FALSE, options = list(
      pageLength = 10, dom = "t", ordering = FALSE
    ))
  })

  # Coefficient stability table
  output$confound_stability_table <- DT::renderDataTable({
    req(CONFOUND)

    stab <- CONFOUND$stability
    display_df <- data.frame(
      `Extended Model` = stab$model,
      Category = stab$category,
      `Base Mean` = sprintf("%.3f", stab$base_mean),
      `Extended Mean` = sprintf("%.3f", stab$extended_mean),
      `Abs. Change` = sprintf("%.3f", stab$abs_change),
      `% Change` = sprintf("%.1f%%", stab$pct_change),
      `P(Region>0) Base` = sprintf("%.1f%%", stab$base_prob_pos * 100),
      `P(Region>0) Extended` = sprintf("%.1f%%", stab$extended_prob_pos * 100),
      check.names = FALSE
    )

    DT::datatable(display_df, rownames = FALSE, options = list(
      pageLength = 10, dom = "t", ordering = FALSE
    ))
  })

  # Mobility x Century interaction table
  output$confound_interaction_table <- DT::renderDataTable({
    req(CONFOUND, CONFOUND$interaction)

    ix <- CONFOUND$interaction
    display_df <- data.frame(
      `Category` = paste0(ix$category, " vs. 10+"),
      `Interaction Mean` = sprintf("%.3f", ix$interaction_mean),
      `CI` = sprintf("[%.3f, %.3f]", ix$interaction_ci_lower, ix$interaction_ci_upper),
      `P(> 0)` = sprintf("%.1f%%", ix$interaction_prob_positive * 100),
      `Mobility Main Effect` = sprintf("%.3f [%.3f, %.3f]", ix$mob_main_mean, ix$mob_main_ci_lower, ix$mob_main_ci_upper),
      check.names = FALSE
    )

    DT::datatable(display_df, rownames = FALSE, options = list(
      pageLength = 10, dom = "t", ordering = FALSE
    ))
  })

  # Compute posterior predictions (reactive) - uses pre-computed data
  posterior_preds <- reactive({
    req(rv$fit_obj, rv$fit_info)

    pre <- rv$fit_obj

    list(
      pp = pre$pp,
      S = pre$S,
      K = pre$fit_info$K,
      levels = pre$fit_info$levels,
      has_century = pre$has_century,
      centuries = pre$centuries
    )
  })

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
      lower <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = CI_TAIL)
      upper <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 1 - CI_TAIL)

      df_cent <- data.frame(
        Category = rep(lvl, times = 2),
        Region = rep(c("Ma\u0121rib", "Ma\u0161riq"), each = K),
        Century = paste0(cent, "th c."),
        mean = as.vector(t(mean_probs)),
        low = as.vector(t(lower)),
        high = as.vector(t(upper))
      )
      plot_data_list[[c_idx]] <- df_cent
    }
    df_plot <- do.call(rbind, plot_data_list)
    df_plot$Century <- factor(df_plot$Century, levels = paste0(centuries, "th c."))
    df_plot$Region <- factor(df_plot$Region, levels = c("Ma\u0121rib", "Ma\u0161riq"))
    df_plot$Category <- factor(df_plot$Category, levels = c("7", "7+1", "10+"))

    df_plot$hover_text <- sprintf(
      paste0("<b>%s</b><br>Region: %s<br>Century: %s<br>Probability: %.1f%%<br>", CI_LABEL_FMT, " CI: [%.1f%%, %.1f%%]"),
      df_plot$Category, df_plot$Region, df_plot$Century,
      df_plot$mean * 100, df_plot$low * 100, df_plot$high * 100
    )

    p <- ggplot(df_plot, aes(x = Category, y = mean, fill = Region, text = hover_text)) +
      geom_col(position = position_dodge(0.8), width = 0.7, color = NA, alpha = 0.7) +
      geom_errorbar(aes(ymin = low, ymax = high),
                    position = position_dodge(0.8), width = 0.15, linewidth = 0.4, color = "gray30") +
      facet_wrap(~ Century, ncol = 2) +
      scale_fill_manual(values = setNames(COLORS$region, c("Ma\u0121rib", "Ma\u0161riq"))) +
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

  # Tab navigation observer for automatic computation
  observeEvent(input$tabs, {
    if (input$tabs == "bayesian_analysis" && !rv$bayesian_analysis_visited) {
      rv$bayesian_analysis_visited <- TRUE
      if (!is.null(rv$fit_obj)) {
        showNotification("Pre-computed Bayesian model results loaded", type = "message", duration = 3)
      } else if (!is.null(rv$clean_data)) {
        showNotification(
          "Pre-computed model results not found. Please ensure precomputed_bayesian_results.rds is in data/ folder.",
          type = "warning", duration = 10)
      }
    }
    if (input$tabs == "model_summary" && !rv$model_summary_visited) {
      rv$model_summary_visited <- TRUE
      if (!is.null(rv$fit_obj)) {
        showNotification("Pre-computed Bayesian model results loaded", type = "message", duration = 3)
      }
    }
    if (input$tabs == "analysis_results" && !rv$analysis_results_visited) {
      rv$analysis_results_visited <- TRUE
      if (!rv$contrasts_computed && !is.null(rv$fit_obj)) {
        showNotification("Computing posterior contrasts for all Sets...",
          type = "message", duration = NULL, id = "auto_contrasts")
        compute_all_contrasts()
        removeNotification(id = "auto_contrasts")
        showNotification("\u2713 All contrasts computed! Select a Set to view results.",
          type = "message", duration = 5)
      }
    }
  })

  # Auto-fit function (disabled for cloud deployment)
  trigger_auto_fit <- function() {
    if (!is.null(rv$fit_obj)) {
      showNotification("Pre-computed Bayesian model results already loaded.", type = "message", duration = 3)
      return()
    }
    showNotification(
      HTML(paste0(
        "<strong>Model Not Available</strong><br/>",
        "Pre-computed results not found. Live MCMC is disabled for cloud deployment.<br/>",
        "Please ensure data/precomputed_bayesian_results.rds exists."
      )),
      type = "error", duration = NULL)
  }

  # Compute all contrasts
  compute_all_contrasts <- function() {
    req(rv$fit_obj, rv$fit_info)
    pre <- rv$fit_obj
    if (!is.null(pre$contrasts)) {
      for (system in names(pre$contrasts)) {
        rv$contrast_results[[system]] <- pre$contrasts[[system]]
      }
      rv$contrasts_computed <- TRUE
      return()
    }
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
      p_east <- apply(pp[, 2, , k_idx, drop = FALSE], 1, mean)
      p_west <- apply(pp[, 1, , k_idx, drop = FALSE], 1, mean)
    } else {
      p_east <- pp[, 2, k_idx]
      p_west <- pp[, 1, k_idx]
    }
    diff <- p_east - p_west
    prob_east_greater <- mean(diff > 0)
    list(
      set = target_system,
      p_east = p_east, p_west = p_west, diff = diff,
      prob_east_greater = prob_east_greater,
      mean_diff = mean(diff),
      ci_lower = ci_lo(diff),
      ci_upper = ci_hi(diff)
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
          tags$li(tags$strong("Outcome:"), " set (3 levels: 7, 7+1, 10+)"),
          tags$li(tags$strong("Predictor:"), " regionality (Ma\u0121rib, Ma\u0161riq)"),
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

  # Prediction plot (static ggplot version)
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
      lower <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = CI_TAIL)
      upper <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 1 - CI_TAIL)

      df_cent <- data.frame(
        Category = rep(lvl, times = 2),
        Region = rep(c("ma\u0121rib", "ma\u0161riq"), each = K),
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
      geom_col(position = position_dodge(0.8), width = 0.7, color = NA, alpha = 0.7) +
      geom_errorbar(aes(ymin = low, ymax = high),
                    position = position_dodge(0.8), width = 0.15, linewidth = 0.4, color = "gray30") +
      facet_wrap(~ Century, ncol = 2) +
      scale_fill_manual(values = COLORS$region, labels = c("Ma\u0121rib", "Ma\u0161riq")) +
      scale_x_discrete(limits = c("7", "7+1", "10+")) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                        labels = scales::percent_format(accuracy = 1)) +
      theme_tufte_custom(base_size = 11) +
      labs(title = "Predicted Probabilities by Century", y = "Probability", x = NULL, fill = NULL) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "bottom", legend.direction = "horizontal",
            panel.spacing = unit(1, "lines"))
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
      lower <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = CI_TAIL)
      upper <- apply(pp[, , c_idx, ], c(2, 3), quantile, probs = 1 - CI_TAIL)
      df_cent <- data.frame(
        Century = paste0(cent, "th"),
        Region = rep(c("ma\u0121rib", "ma\u0161riq"), each = K),
        Category = rep(lvl, times = 2),
        MeanProb = round(as.vector(t(mean_probs)), 3),
        CI_low = round(as.vector(t(lower)), 3),
        CI_high = round(as.vector(t(upper)), 3)
      )
      table_list[[c_idx]] <- df_cent
    }
    do.call(rbind, table_list)
  })

  # Contrast display
  output$contrast_display <- renderUI({
    req(rv$contrasts_computed, input$selected_system_card4)
    contrast <- rv$contrast_results[[input$selected_system_card4]]
    req(contrast)
    explanation <- get_system_explanation(input$selected_system_card4)

    div(class = "card",
      div(class = "card-header", style = "background-color: #17a2b8; color: white;",
        h4(style = "margin: 0;", paste("Set:", input$selected_system_card4))
      ),
      div(class = "card-body",
        div(class = "interpretation-text", HTML(explanation)),
        div(class = "results-card",
          h4("Regional Contrast Results"),
          tags$ul(
            tags$li(tags$strong(sprintf("P(%s|ma\u0161riq) > P(%s|ma\u0121rib):", contrast$set, contrast$set)),
                   sprintf(" %.3f", contrast$prob_east_greater)),
            tags$li(tags$strong("Mean difference (ma\u0161riq - ma\u0121rib):"),
                   sprintf(" %.3f", contrast$mean_diff)),
            tags$li(tags$strong(sprintf("%s Credible Interval:", CI_LABEL)),
                   sprintf(" [%.3f, %.3f]", contrast$ci_lower, contrast$ci_upper))
          ),
          tags$hr(),
          p(tags$strong("Interpretation: "),
            get_interpretation(contrast$prob_east_greater, contrast$mean_diff, contrast$set))
        ),
        plotOutput(paste0("contrast_plot_", gsub("\\+", "plus", input$selected_system_card4)),
                  height = "350px"),
        p(class = "text-muted", style = "font-size: 0.85em;",
          sprintf("The histogram shows the posterior distribution of the difference in probabilities. The dark vertical line marks the mean difference, while the gray line at zero represents no regional effect. The subtle gray shaded region indicates the %s credible interval. When the distribution is clearly shifted away from zero, this provides evidence of systematic regional preference for this Set of Readings.", CI_LABEL))
      )
    )
  })

  # Get system-specific explanation
  get_system_explanation <- function(system) {
    explanations <- list(
      "7" = "<h5>The Set of 7 and Regional Canons</h5>
<p>The Set of 7 represents Ibn Mu\u01e7\u0101hid's (d. 324/936) canonical selection, which became the foundation of qir\u0101\u02be\u0101t pedagogy. Regional preferences for this Set indicate adoption of Ibn Mu\u01e7\u0101hid's framework as the standard pedagogical model.</p>
<p><strong>If Ma\u0161riq shows higher probability:</strong> This aligns with historical expectations, as Ibn Mu\u01e7\u0101hid worked in Baghdad and the Set of 7 initially spread through Ma\u0161riq\u012b scholarly networks.</p>
<p><strong>If Ma\u0121rib shows higher probability:</strong> This indicates that Ma\u0121rib\u012b scholars particularly embraced Ibn Mu\u01e7\u0101hid's canonical framework, perhaps as a means of standardizing instruction.</p>",

      "7+1" = "<h5>The Set of 7+1 and Regional Variation</h5>
<p>The Set of 7+1 adds the reading of Ya\u02bfq\u016bb al-\u1e24a\u1e0dram\u012b (d. 205/821) to Ibn Mu\u01e7\u0101hid's seven, representing an expansion of the canonical framework. Production patterns for this Set reveal regional attitudes toward canonical boundaries and acceptable variation.</p>
<p><strong>If Ma\u0161riq shows higher probability:</strong> This suggests Ma\u0161riq\u012b scholars were more willing to expand beyond Ibn Mu\u01e7\u0101hid's original framework, perhaps reflecting continued engagement with pre-canonical reading traditions.</p>
<p><strong>If Ma\u0121rib shows higher probability:</strong> This is particularly significant, suggesting Ma\u0121rib\u012b scholars developed a distinct pedagogical tradition that systematically included Ya\u02bfq\u016bb's reading alongside the canonical seven, potentially indicating a Ma\u0121rib\u012b counter-canon.</p>",

      "10+" = "<h5>The Set of 10+ and Comprehensive Pedagogical Approaches</h5>
<p>Works describing ten or more reading traditions represent the most comprehensive approach to qir\u0101\u02be\u0101t instruction, often including the 'three additional' readings beyond the seven (Ya\u02bfq\u016bb, \u1e2aalaf, al-\u1e24asan al-Ba\u1e63r\u012b, etc.) or engaging with even broader reading traditions.</p>
<p><strong>If Ma\u0161riq shows higher probability:</strong> This suggests Ma\u0161riq\u012b scholars maintained stronger interest in preserving and transmitting the full diversity of reading traditions beyond canonical selections, perhaps reflecting the region's role as the original site of reading tradition development.</p>
<p><strong>If Ma\u0121rib shows higher probability:</strong> This indicates that Ma\u0121rib\u012b scholars, despite geographic distance from the original centers of qir\u0101\u02be\u0101t development, sought comprehensive knowledge of reading traditions, possibly as a strategy for scholarly authority.</p>"
    )
    return(explanations[[system]])
  }

  # Get interpretation text
  get_interpretation <- function(prob_east_greater, mean_diff, system) {
    if (prob_east_greater > 0.95) {
      sprintf("Strong evidence that Ma\u0161riq has higher probability for the %s Set across all centuries. The mean difference of %.3f indicates Ma\u0161riq scholars had substantially higher preference for this Set.",
              system, mean_diff)
    } else if (prob_east_greater < 0.05) {
      sprintf("Strong evidence that Ma\u0121rib has higher probability for the %s Set across all centuries. The mean difference of %.3f indicates Ma\u0121rib scholars had substantially higher preference for this Set.",
              system, abs(mean_diff))
    } else if (prob_east_greater > 0.75) {
      sprintf("Moderate evidence that Ma\u0161riq has higher probability for the %s Set (%.1f%% confidence). The mean difference of %.3f suggests a Ma\u0161riq\u012b preference, but with some uncertainty.",
              system, prob_east_greater * 100, mean_diff)
    } else if (prob_east_greater < 0.25) {
      sprintf("Moderate evidence that Ma\u0121rib has higher probability for the %s Set (%.1f%% confidence). The mean difference of %.3f suggests a Ma\u0121rib\u012b preference, but with some uncertainty.",
              system, (1 - prob_east_greater) * 100, abs(mean_diff))
    } else {
      sprintf("Weak or no evidence of regional difference for the %s Set. The posterior probability is %.3f, suggesting relatively balanced preferences between regions.",
              system, prob_east_greater)
    }
  }

  # Contrast plots
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
    hist_color <- COLORS$set[contrast_result$set]
    if (is.na(hist_color)) hist_color <- COLORS$set["7"]

    ggplot(df, aes(x = diff)) +
      geom_histogram(bins = 50, fill = hist_color, color = "white", alpha = 0.75, linewidth = 0.25) +
      annotate("rect", xmin = contrast_result$ci_lower, xmax = contrast_result$ci_upper,
               ymin = 0, ymax = Inf, fill = hist_color, alpha = 0.15) +
      geom_vline(xintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
      geom_vline(xintercept = contrast_result$mean_diff, color = "gray10", linewidth = 0.9) +
      geom_vline(xintercept = contrast_result$ci_lower, linetype = "dashed", color = hist_color, linewidth = 0.6) +
      geom_vline(xintercept = contrast_result$ci_upper, linetype = "dashed", color = hist_color, linewidth = 0.6) +
      annotate("text", x = contrast_result$mean_diff, y = Inf, label = "mean",
               vjust = 1.5, hjust = -0.1, color = "gray10", size = 3, family = "sans") +
      annotate("text", x = 0, y = Inf, label = "null",
               vjust = 1.5, hjust = 1.1, color = "gray40", size = 3, family = "sans") +
      theme_tufte_custom(base_size = 11) +
      labs(
        title = sprintf("P(%s|Ma\u0161riq) \u2212 P(%s|Ma\u0121rib)", contrast_result$set, contrast_result$set),
        x = "Difference in Probability", y = NULL,
        caption = sprintf("Mean difference: %.3f  |  %s CI: [%.3f, %.3f]",
                         contrast_result$mean_diff, CI_LABEL, contrast_result$ci_lower, contrast_result$ci_upper)
      ) +
      theme(axis.line.y = element_blank(), axis.text.y = element_text(color = "gray50", size = 8),
            axis.ticks.y = element_blank())
  }

  # PPC data (reactive)
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
    # PPC_TAIL, not CI_TAIL: this band is compared against the OBSERVED counts, so it is a
    # model-adequacy check. Widening it would only make the check easier to pass. Held at 95%.
    sim_low <- apply(sim_counts_mat, 2, quantile, prob = PPC_TAIL)
    sim_high <- apply(sim_counts_mat, 2, quantile, prob = 1 - PPC_TAIL)
    data.frame(
      Category = preds$levels,
      Observed = observed_counts,
      SimMean = round(sim_mean, 1),
      SimLow = round(sim_low, 0),
      SimHigh = round(sim_high, 0)
    )
  })

  # Return posterior_preds so diagnostics can use it
  list(posterior_preds = posterior_preds)
}
