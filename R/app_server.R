#' The application server-side
#'
#' The disclaimer and the out-of-range warnings are handled in the browser
#' (see `fib_modal()` and inst/app/www/fibomai.js), so they do not wait for
#' the server connection.
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #################### ==================== Data loading  ====================  ####################

  ##### ===== New data

  newdata <- eventReactive(input$run, {
    # Helpers
    num <- function(x) {
      if (is.null(x)) return(NA_real_)
      if (is.character(x) && trimws(x) == "") return(NA_real_)
      out <- suppressWarnings(as.numeric(x))
      if (is.na(out)) NA_real_ else out
    }
    pct <- function(part, total) ifelse(is.na(part) | is.na(total) | total == 0, NA_real_, part * 100 / total)
    abs_from_pct <- function(p, total) ifelse(is.na(p) | is.na(total), NA_real_, p * total / 100)

    tibble::tibble(
      BM_biopsy_age = num(input$age),

      leucocytes = num(input$WBC),
      hematies   = num(input$hematies),

      hemoglobine = num(input$hemoglobin),
      hematocrite = num(input$hematocrite),
      vgm         = num(input$MCV),
      tcmh        = num(input$MCH),
      ccmh        = num(input$MCHC),

      idr_cv     = num(input$IDR),

      plaquettes = num(input$platelets),
      vpm        = num(input$mpv),

      poly_neutrophiles = num(input$ANC),
      lymphocytes       = num(input$ALC),
      monocytes         = num(input$mono),
      poly_eosinophiles = num(input$eosino),
      poly_basophiles   = num(input$baso),

      # Saisis en pourcentage si renseignés, sinon NA
      erythroblastes_percent = num(input$erythroblasts),
      myelemie_percent       = num(input$myelemia),
      myeloblastes_percent   = num(input$myeloblasts)
    ) %>%
      dplyr::mutate(
        poly_neutrophiles_percent = pct(poly_neutrophiles, leucocytes),
        poly_eosinophiles_percent = pct(poly_eosinophiles, leucocytes),
        poly_basophiles_percent   = pct(poly_basophiles, leucocytes),
        lymphocytes_percent       = pct(lymphocytes, leucocytes),
        monocytes_percent         = pct(monocytes, leucocytes),

        myelemie       = abs_from_pct(myelemie_percent, leucocytes),
        erythroblastes = abs_from_pct(erythroblastes_percent, leucocytes),
        myeloblastes   = abs_from_pct(myeloblastes_percent, leucocytes)
      )
  })


  #################### ==================== App modules ====================  ####################

  mod_prediction_server("prediction_1", newdata = newdata)
}
