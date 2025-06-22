require(dplyr)
require(tidyr)

trim_group_outliers <- function(data, vars, p = 0.01, tail_wt = 0.5, group_var = NULL) {
  # Harmonize argument lengths
  if (length(vars) > 1 && length(p) == 1) {
    p <- rep(p, length(vars))
    message("Note: same 'p' applied to all variables.")
  } else if (length(vars) != length(p)) {
    stop("Length of 'p' must be 1 or match length of 'vars'.")
  }
  
  if (length(vars) > 1 && length(tail_wt) == 1) {
    tail_wt <- rep(tail_wt, length(vars))
    message("Note: same 'tail_wt' applied to all variables.")
  } else if (length(vars) != length(tail_wt)) {
    stop("Length of 'tail_wt' must be 1 or match length of 'vars'.")
  }
  
  # Core trimming logic per group
  trim_group <- function(df_sub) {
    n <- nrow(df_sub)
    flags <- matrix(FALSE, nrow = n, ncol = length(vars))
    colnames(flags) <- vars
    
    for (i in seq_along(vars)) {
      var <- vars[i]
      pi <- p[i]
      wt <- tail_wt[i]
      p_low <- pi * (1 - wt)
      p_high <- 1 - pi * wt
      
      if (!is.numeric(df_sub[[var]])) {
        stop(paste0("Column '", var, "' must be numeric."))
      }
      
      bounds <- quantile(df_sub[[var]], probs = c(p_low, p_high), na.rm = TRUE)
      flags[, i] <- df_sub[[var]] < bounds[1] | df_sub[[var]] > bounds[2]
    }
    
    outlier_mask <- apply(flags, 1, any)
    kept_mask <- !outlier_mask
    outlier_var_labels <- apply(flags, 1, function(x) paste(vars[x], collapse = ","))
    
    removed <- df_sub[outlier_mask, , drop = FALSE]
    removed$outlier_var <- outlier_var_labels[outlier_mask]
    
    trimmed <- df_sub[kept_mask, , drop = FALSE]
    
    list(trimmed = trimmed, removed = removed)
  }
  
  # Apply globally or by group
  if (is.null(group_var)) {
    result <- trim_group(data)
  } else {
    result_grouped <- data %>%
      group_by(.data[[group_var]]) %>%
      group_split() %>%
      lapply(trim_group)
    
    result <- list(
      trimmed = bind_rows(lapply(result_grouped, `[[`, "trimmed")),
      removed = bind_rows(lapply(result_grouped, `[[`, "removed"))
    )
  }
  
  # Build summary of removed records
  removed_df_all <- result$removed
  
  # Parse multiple vars in `outlier_var` into rows
  removed_long <- removed_df_all %>%
    separate_rows(outlier_var, sep = ",") %>%
    rename(trim_var = outlier_var)
  
  removed_summary <- removed_long %>%
    count(trim_var, .data[[group_var]], name = "n_removed") %>%
    group_by(trim_var) %>%
    mutate(prop_removed = n_removed / sum(n_removed)) %>%
    ungroup() %>%
    rename(group = !!group_var)
  
  
  # Return combined result
  return(list(
    trimmed_df = result$trimmed,
    removed_df = result$removed,
    removed_summary = removed_summary
  ))
}
