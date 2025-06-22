require(ggplot2)
require(dplyr)

show_group_distribution <- function(data, vars, group_var,
                                    p = 0.01, tail_wt = 0.5,
                                    bins = 100, binwidth = NULL,
                                    dens_x_min = NULL, dens_x_max = NULL) {
  
  # --- Harmonize input lengths ---
  n_vars <- length(vars)
  
  if (n_vars > 1 && length(p) == 1) {
    p <- rep(p, n_vars)
    message("Note: same 'p' applied to all variables.")
  } else if (length(p) != n_vars) {
    stop("Length of 'p' must be 1 or match length of 'vars'.")
  }
  
  if (n_vars > 1 && length(tail_wt) == 1) {
    tail_wt <- rep(tail_wt, n_vars)
    message("Note: same 'tail_wt' applied to all variables.")
  } else if (length(tail_wt) != n_vars) {
    stop("Length of 'tail_wt' must be 1 or match length of 'vars'.")
  }
  
  if (!is.null(bins)) {
    if (length(bins) == 1) {
      bins <- rep(bins, n_vars)
      message("Note: same 'bins' applied to all variables.")
    } else if (length(bins) != n_vars) {
      stop("Length of 'bins' must be 1 or match length of 'vars'.")
    }
  }
  
  if (!is.null(binwidth)) {
    if (length(binwidth) == 1) {
      binwidth <- rep(binwidth, n_vars)
      message("Note: same 'binwidth' applied to all variables.")
    } else if (length(binwidth) != n_vars) {
      stop("Length of 'binwidth' must be 1 or match length of 'vars'.")
    }
  }
  
  ## Validate x limits if provided
  if (!is.null(dens_x_min) && length(dens_x_min) != length(vars)) {
    stop("Length of 'dens_x_min' must be NULL or match length of 'vars'.")
  }
  if (!is.null(dens_x_max) && length(dens_x_max) != length(vars)) {
    stop("Length of 'dens_x_max' must be NULL or match length of 'vars'.")
  }
  
  # --- Output lists ---
  density_plots <- list()
  histogram_plots <- list()
  
  for (i in seq_along(vars)) {
    var <- vars[i]
    pi <- p[i]
    wt <- tail_wt[i]
    p_low <- pi * (1 - wt)
    p_high <- 1 - pi * wt
    
    if (!is.numeric(data[[var]])) {
      stop(paste0("Variable '", var, "' must be numeric."))
    }
    
    # --- Density plot ---
    density_plot <- ggplot(data, aes(x = .data[[var]], fill = .data[[group_var]])) +
      geom_density(alpha = 0.4) +
      labs(
        title = paste("Density Distribution -", var, "by", group_var),
        x = var,
        y = "density"
      ) +
      theme_minimal()
    
    # Add x limits if specified
    if (!is.null(dens_x_min) && !is.null(dens_x_max)) {
      density_plot <- density_plot + xlim(dens_x_min[i], dens_x_max[i])
    } else if (!is.null(dens_x_min)) {
      density_plot <- density_plot + xlim(dens_x_min[i], NA)
    } else if (!is.null(dens_x_max)) {
      density_plot <- density_plot + xlim(NA, dens_x_max[i])
    }
    
    density_plots[[var]] <- density_plot
    
    # --- Histogram + outlier threshold per group ---
    bounds_df <- data %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        lower = quantile(.data[[var]], probs = p_low, na.rm = TRUE),
        upper = quantile(.data[[var]], probs = p_high, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Merge group-level bounds into original data
    plot_data <- data %>%
      left_join(bounds_df, by = group_var)
    
    # Compute binning manually
    bin_data <- plot_data %>%
      mutate(bin = if (!is.null(binwidth)) {
        cut(.data[[var]], breaks = seq(0, #min(.data[[var]], na.rm = TRUE),
                                       max(.data[[var]], na.rm = TRUE) + binwidth[i],
                                       by = binwidth[i]))
      } else {
        cut(.data[[var]], breaks = bins[i])
      }) %>%
      filter(!is.na(bin)) %>%
      group_by(.data[[group_var]], bin) %>%
      summarise(
        count = n(),
        midpoint = mean(as.numeric(sub("\\((.+),.*", "\\1", bin)) +
                          as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", bin))) / 2,
        lower = first(lower),
        upper = first(upper),
        .groups = "drop"
      ) %>%
      mutate(outlier = midpoint < lower | midpoint > upper)
    
    histogram_plots[[var]] <- list()
    
    for (g in unique(bin_data[[group_var]])) {
      df_group <- bin_data %>% filter(.data[[group_var]] == g)
      
      caption <- paste0("Bounds set at [", round(df_group$lower[1], 2), ", ", round(df_group$upper[1], 2), "]. ",
                        "Outlier bins: ", sum(df_group$outlier))
      
      plot <- ggplot(df_group, aes(x = midpoint, y = count, fill = outlier)) +
        geom_col(color = "white") +
        geom_vline(xintercept = df_group$lower[1], linetype = "dashed", color = "red") +
        geom_vline(xintercept = df_group$upper[1], linetype = "dashed", color = "red") +
        scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray80")) +
        labs(
          title = paste("Histogram of", var, "-", g),
          subtitle = paste0("p = ", pi, ", tail_wt = ", wt),
          caption = caption,
          x = var,
          y = "Count"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      histogram_plots[[var]][[g]] <- plot
    }
  }
  
  return(list(
    density = density_plots,
    histogram = histogram_plots
  ))
}
