
# --- Helper: numericInput + space just after---
# (espacement compact géré dans fibomai.css, sans marge négative qui ferait
# chevaucher la ligne suivante sur le bas du champ)
compact_num <- function(inputId, label, value = NA, step = NA, min = NA, max = NA, width = "100%") {
  div(class = "sb-item",
    numericInput(
      inputId = inputId,  label   = label,   value   = value,
      min     = min,      max     = max,      step    = step,
      width   = width
    )
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


# --- Lightweight modal (pure HTML/CSS, opened/closed by inst/app/www/fibomai.js)
# Rendered with the page, so it does not depend on the server connection.
fib_modal <- function(id, title, ..., open = FALSE, button = "OK", type = "info", extra_button = NULL) {
  div(
    id = id,
    class = paste("fib-modal", paste0("fib-modal-", type), if (open) "fib-open"),
    role = "dialog", `aria-modal` = "true", `aria-labelledby` = paste0(id, "-title"),
    div(class = "fib-modal-box",
        h2(id = paste0(id, "-title"), class = "fib-modal-title", title),
        div(class = "fib-modal-body", ...),
        div(class = "fib-modal-buttons",
            tags$button(type = "button", class = "btn btn-primary fib-modal-close", button),
            extra_button)
    )
  )
}


# --- Function to get the uncertainty interval around a prediction
# The lookup quantiles describe the spread of predictions among similar cases
# (same prediction bin), but they are not centred on the patient: at the
# extremes they are pulled towards 50%, so the patient's point could fall
# outside its own interval. We therefore keep only the spread of each bin
# (distance from its median to each quantile) and re-centre it on the
# patient's prediction. The spread is interpolated between bin centres to
# avoid jumps at bin boundaries.
get_prediction_interval <- function(pred_value, lookup_table,
                                    lower = "empirical_q025", upper = "empirical_q975") {
  lk <- as.data.frame(lookup_table)
  below <- lk$empirical_q500 - lk[[lower]]
  above <- lk[[upper]] - lk$empirical_q500

  d_low  <- stats::approx(lk$pred_mean_bin, below, xout = pred_value, rule = 2)$y
  d_high <- stats::approx(lk$pred_mean_bin, above, xout = pred_value, rule = 2)$y

  c(lower = max(0, pred_value - d_low), upper = min(1, pred_value + d_high))
}


# --- Probability of grade 2-3 fibrosis
# binary:logistic booster trained with "Yes" as the positive class, so the raw
# output is P(Yes). Missing values (NA) are handled natively by XGBoost.
predict_fibrosis <- function(newdata, booster = get_booster()) {
  x <- as.matrix(as.data.frame(newdata)[, model_features, drop = FALSE])
  storage.mode(x) <- "double"
  p <- predict(booster, xgb.DMatrix(x, missing = NA))
  data.frame(Yes = p, No = 1 - p)
}

# --- Booster, loaded once per R session from the JSON bytes
.model_cache <- new.env(parent = emptyenv())
get_booster <- function() {
  if (is.null(.model_cache$booster)) {
    f <- tempfile(fileext = ".json")
    on.exit(unlink(f))
    writeBin(fibrosis_model_json, f)
    .model_cache$booster <- xgb.load(f)
  }
  .model_cache$booster
}

# --- Confident zone: p below the sensitivity cutoff or above the specificity
# cutoff, and only for complete inputs (the cutoffs were set on complete CBCs)
is_confident <- function(p, n_missing = 0) {
  n_missing == 0 &&
    (p < as.numeric(params_df$sen_threshold) || p > as.numeric(params_df$spe_threshold))
}
