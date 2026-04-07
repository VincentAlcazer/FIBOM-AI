
# --- Helper: numericInput + space just after---
compact_num <- function(inputId, label, value = NA, step = NA, min = NA, max = NA, width = "100%", spacer = TRUE) {
  div(class = "sb-item",
    numericInput(
      inputId = inputId,  label   = label,   value   = value,
      min     = min,      max     = max,      step    = step,
      width   = width
    ),
    # le spacer à l'intérieur du même conteneur
    if (isTRUE(spacer)) div(class = "sb-spacer", style = "margin-top:-20px")  # style en CSS
  )
}


# --- Function to get prediction uncertainty
get_empirical_uncertainty <- function(pred_value, lookup_table) {
  row <- lookup_table %>%
    filter(pred_value >= lower, pred_value < upper)

  # Cas limite pred = 1
  if (nrow(row) == 0 && pred_value == 1) {
    row <- lookup_table %>%
      filter(upper == max(upper))
  }

  if (nrow(row) == 0) {
    return(NULL)
  }

  tibble(
    pred = pred_value,
    empirical_q025 = row$empirical_q025[[1]],
    empirical_q500 = row$empirical_q500[[1]],
    empirical_q975 = row$empirical_q975[[1]],
    empirical_iqr_low = row$empirical_iqr_low[[1]],
    empirical_iqr_high = row$empirical_iqr_high[[1]],
    n_bin = row$n_bin[[1]],
    pred_bin = as.character(row$pred_bin[[1]]),
    pred_sd_bin = row$pred_sd_bin[[1]]
  )
}
