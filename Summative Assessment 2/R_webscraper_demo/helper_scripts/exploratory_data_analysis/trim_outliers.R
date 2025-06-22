trim_outliers <- function(data, vars, p = 0.01, tail_wt = 0.5) {
  # Check argument lengths
  if (length(vars) > 1 && length(p) == 1) {
    p <- rep(p, length(vars))
    message("Note: same 'p' applied to all variables.")
  } else if (length(vars) != length(p)) {
    stop("Length of 'p' must be 1 or match length of 'vars'.")
  }
  
  if (length(vars) > 1 && length(tail_wt) == 1) {
    tail_wt <- rep(tail_wt, length(vars)) # Apply same weight to all
    message("Note: same 'tail_wt' applied to all 'vars")
  } else if (length(vars) != length(tail_wt)) {
    stop("Length of 'tail_wt' must be 1 or match length of 'vars'")
  }
  
  df <- data
  n <- nrow(df)
  
  # Store bounds and outlier flags per variable
  outlier_flags <- matrix(FALSE, nrow = n, ncol = length(vars))
  colnames(outlier_flags) <- vars
  
  for (i in seq_along(vars)) {
    var <- vars[i]
    pi <- p[i]
    wt <- tail_wt[i]
    p_low <- pi * (1 - wt)
    p_high <- 1 - pi * wt
    
    if (!is.numeric(df[[var]])) {
      stop(paste0("Column '", var, "' must be numeric."))
    }
    
    bounds <- quantile(df[[var]], probs = c(p_low, p_high), na.rm = TRUE)
    outlier_flags[, i] <- df[[var]] < bounds[1] | df[[var]] > bounds[2]
  }
  
  # Identify rows that are outliers in at least one variable
  outlier_mask <- apply(outlier_flags, 1, any)
  kept_mask <- !outlier_mask
  
  # Generate outlier_var column
  outlier_var_labels <- apply(outlier_flags, 1, function(x) {
    paste(vars[x], collapse = ",")
  })
  
  removed_df <- df[outlier_mask, , drop = FALSE]
  removed_df$outlier_var <- outlier_var_labels[outlier_mask]
  
  trimmed_df <- df[kept_mask, , drop = FALSE]
  
  # Return full list
  result <- list(
    trimmed_df = trimmed_df,
    removed_df = removed_df
  )
  
  return(result)
}
