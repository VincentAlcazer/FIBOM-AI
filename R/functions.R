
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
fib_modal <- function(id, title, ..., open = FALSE, button = "OK", type = "info") {
  div(
    id = id,
    class = paste("fib-modal", paste0("fib-modal-", type), if (open) "fib-open"),
    role = "dialog", `aria-modal` = "true", `aria-labelledby` = paste0(id, "-title"),
    div(class = "fib-modal-box",
        h2(id = paste0(id, "-title"), class = "fib-modal-title", title),
        div(class = "fib-modal-body", ...),
        tags$button(type = "button", class = "btn btn-primary fib-modal-close", button)
    )
  )
}
