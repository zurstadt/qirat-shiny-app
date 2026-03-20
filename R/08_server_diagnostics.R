server_diagnostics <- function(input, output, session, rv, posterior_preds) {

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
      geom_point(size = 3, color = COLORS$set["7"], shape = 16) +
      geom_hline(yintercept = 1.0, linetype = "solid",
                 color = "gray60", linewidth = 0.3) +
      geom_hline(yintercept = 1.01, linetype = "dashed",
                 color = COLORS$set["10+"], linewidth = 0.6) +
      scale_y_continuous(limits = c(0.99, 1.05), breaks = seq(0.99, 1.05, 0.01)) +
      annotate("text", x = 1, y = 1.01, label = "threshold = 1.01",
               vjust = -0.5, hjust = 0, size = 2.5, color = COLORS$set["10+"]) +
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
                 color = COLORS$set["10+"], linewidth = 0.6) +
      scale_fill_manual(values = COLORS$ess, name = NULL) +
      scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
      annotate("text", x = 0.5, y = 400, label = "threshold = 400",
               vjust = -0.5, size = 2.5, color = COLORS$set["10+"]) +
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
    category_colors <- COLORS$set[lvl]
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
    category_colors <- COLORS$set[lvl]
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
      scale_fill_manual(values = COLORS$set, guide = "none") +
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
                       "set", "type", "regionality")))
    } else {
      display_df <- rv$raw_data %>%
        select(any_of(c("work_id", "title", "author_name", "set", "type", "regionality")))
    }

    datatable(
      display_df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

}
