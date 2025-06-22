require(ggplot2)

show_distribution <- function(data, vars, p = 0.01, tail_wt = 0.5, 
                              bins = 100, binwidth = NULL) {
  # Check argument lengths
  if (length(vars) > 1 && length(p) == 1) {
    p <- rep(p, length(vars))
    message("Note: same 'p' applied to all variables.")
  } else if (length(vars) != length(p)) {
    stop("Length of 'p' must be 1 or match length of 'vars'.")
  }
  
  if (length(vars) > 1 && length(tail_wt) == 1) {
    tail_wt <- rep(tail_wt, length(vars)) # Apply same weight to all
    message("Note: same 'tail_wt' applied to all variables.")
  } else if (length(vars) != length(tail_wt)) {
    stop("Length of 'tail_wt' must be 1 or match length of 'vars'")
  }
  
  # Harmonize length of bins
  if (!is.null(bins)) {
    if (length(vars) > 1 && length(bins) == 1) {
      bins <- rep(bins, length(vars))
      message("Note: same 'bins' applied to all variables.")
    } else if (length(vars) != length(bins)) {
      stop("Length of 'bins' must be 1 or match length of 'vars'.")
    }
  }
  
  # Harmonize length of binwidth
  if (!is.null(binwidth)) {
    if (length(vars) > 1 && length(binwidth) == 1) {
      binwidth <- rep(binwidth, length(vars))
      message("Note: same 'binwidth' applied to all variables.")
    } else if (length(vars) != length(binwidth)) {
      stop("Length of 'binwidth' must be 1 or match length of 'vars'.")
    }
  }
  
  
  plot_list <- list()
  
  for (i in seq_along(vars)) {
    var <- vars[i]
    pi <- p[i]
    wt <- tail_wt[i]
    p_low <- pi * (1 - wt)
    p_high <- 1 - pi*wt
    
    if (!is.numeric(data[[var]])) {
      stop(paste0("Variable '", var, "' must be numeric."))
    }
    
    var_data <- data[[var]]
    bounds <- quantile(var_data, probs = c(p_low, p_high), na.rm = TRUE)
    
    # Count observations out of bounds
    out_of_bounds <- sum(var_data < bounds[1] | var_data > bounds[2], na.rm = TRUE)
    
    # Construct subtitle and caption text
    subtitle_text <- paste0("Trimming parameters: p = ", pi, ", tail_wt = ", wt)
    caption_text <- paste0(
      "Bounds set at [", round(bounds[1], 2), ", ", round(bounds[2], 2), "]. ",
      "Number of outlier observations: ", out_of_bounds
    )

    # Compute histogram manually ...
    if (!is.null(binwidth)) {
      hist_data <- hist(var_data, plot = FALSE, breaks = seq(min(var_data, na.rm = TRUE),
                                                             max(var_data, na.rm = TRUE) + binwidth[i],
                                                             by = binwidth[i]))
    } else {
      hist_data <- hist(var_data, plot = FALSE, breaks = bins[i])
    }
    
    hist_df <- data.frame(
      midpoint = hist_data$mids,
      count = hist_data$counts,
      outlier = hist_data$mids < bounds[1] | hist_data$mids > bounds[2]
    )
    
    plot <- ggplot(hist_df, aes(x = midpoint, y = count, fill = outlier)) +
      geom_col() +
      scale_fill_manual(values = c("FALSE" = "gray80", "TRUE" = "red")) +
      geom_vline(xintercept = bounds[1], linetype = "dashed", color = "red") +
      geom_vline(xintercept = bounds[2], linetype = "dashed", color = "red") +
      labs(
        title = paste0("Distribution of ", var),
        x = var,
        y = "Count",
        subtitle = subtitle_text,
        caption = caption_text
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    plot_list[[var]] <- plot
  }
  
  return(plot_list)
}