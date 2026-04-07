#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  shinyalert(
    title = "Disclaimer",
    text = HTML('FIBOM-AI is provided for research use only and is not intended to replace clinical judgement or standard diagnosis work-up,
    or bone marrow evaluation when clinically indicated.
    </br> </br>
    This tool estimates the probability of grade 2–3 bone marrow fibrosis but does not establish MPN subtype.
    </br>
    Model outputs must be interpreted within the full clinical, biological, and pathological context.
    Any diagnostic or therapeutic decision made using this tool remains the sole responsibility of the treating physician.
       </br>'
  ),
    html = T,
    type = "info",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    showConfirmButton = TRUE,
    confirmButtonText = "I Understand"
  )



  #################### ==================== Data loading  ====================  ####################
  r <- reactiveValues(
    test = reactiveValues()
  )


  ##### ===== New data

  newdata <- reactive({

    req(input$run >= 1)
    input$run
    isolate({
      # Helpers
      num <- function(x) {
        if (is.null(x)) return(NA_real_)
        if (is.character(x) && trimws(x) == "") return(NA_real_)
        out <- suppressWarnings(as.numeric(x))
        if (is.na(out)) NA_real_ else out
      }
      pct <- function(part, total) ifelse(is.na(part) | is.na(total) | total == 0, NA_real_, part * 100 / total)
      abs_from_pct <- function(p, total) ifelse(is.na(p) | is.na(total), NA_real_, p * total / 100)

      dat <- tibble::tibble(
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

      return(dat)
    })
  })


  ##### ===== R list

  observe({
    r$test$newdata <- newdata()
  })



  ##### ===== Check values

  # Plages valides
  valid_ranges <- list(
    age              = c(7, 90),
    WBC                        = c(0.5, 250),
    hematies                   = c(1.33, 40),
    hemoglobin                = c(44, 230),
    hematocrite                = c(14, 70),
    MCV                        = c(50, 135),
    MCH                       = c(14, 47),
    MCHC                       = c(250, 380),
    IDR                      = c(8, 37),
    platelets                 = c(10, 2300),
    mpv                        = c(6, 17),
    ANC          = c(0.01, 150),
    eosino          = c(0.00, 30),
    baso            = c(0.00, 5),
    ALC                = c(0.00, 10),
    mono                  = c(0.00, 10),
    myelemia                   = c(0.00, 75),
    erythroblasts             = c(0.00, 40),
    myeloblasts               = c(0.00, 15)
  )

  # --- OpLabels pour l'alerte ---
  pretty_labels <- c(
    age            = "Age (years)",
    WBC            = "Leukocytes (10^9/L)",
    hematies       = "RBC (10^12/L)",
    hemoglobin     = "Hemoglobin (g/L)",
    hematocrite    = "Hematocrit (%)",
    MCV            = "MCV (fL)",
    MCH            = "MCH (pg)",
    MCHC           = "MCHC (g/L)",
    IDR            = "RDW-CV (%)",
    platelets      = "Platelets (10^9/L)",
    mpv            = "MPV (fL)",
    ANC            = "Absolute neutrophils (10^9/L)",
    eosino         = "Absolute eosinophils (10^9/L)",
    baso           = "Absolute basophils (10^9/L)",
    ALC            = "Absolute lymphocytes (10^9/L)",
    mono           = "Absolute monocytes (10^9/L)",
    myelemia       = "Myelemia (10^9/L)",
    erythroblasts  = "Absolute erythroblasts (10^9/L)",
    myeloblasts    = "Absolute myeloblasts (10^9/L)"
  )


  # --- Fonctions utilitaires  ---
  check_range <- function(input_value, range) {
    is.finite(input_value) && input_value >= range[1] && input_value <= range[2]
  }
  show_alert <- function(message) {
    shinyalert::shinyalert("Warning", message, type = "warning", html = T,
                           closeOnClickOutside = T, showConfirmButton = T)
  }

  # --- Observers automatiques pour toutes les variables listées ---
  lapply(names(valid_ranges), function(id) {
    local({
      var_id <- id
      observeEvent(input[[paste0(var_id, "_blur")]], ignoreInit = TRUE, {
        val <- as.numeric(input[[paste0(var_id, "_blur")]])
        if (is.null(val) || is.na(val) || !is.finite(val)) return(invisible())

        rng <- valid_ranges[[var_id]]
        if (val < rng[1] || val > rng[2]) {
          label <- pretty_labels[[var_id]] %||% var_id
          msg <- sprintf(
            "⚠️ <b>%s</b> is outside the training range (<b>%g – %g</b>).<br>
           Please verify input data: predictions using outlier data should be interpreted with caution.",
            label, rng[1], rng[2]
          )
          show_alert(msg)
        }
      }, priority = 100)
    })
  })


  # # Fonction pour vérifier si une valeur est dans la plage valide
  # check_range <- function(input_value, range) {
  #   return(input_value >= range[1] && input_value <= range[2])
  # }
  #
  # # Alert function
  # show_alert <- function(message) {
  #   shinyalert("Warning", message, type = "warning")
  # }


  # Outlier alert
  # observe({
  #   req(input$BM_biopsy_age)  # S'assurer que la valeur est disponible
  #   BM_biopsy_age <- input$BM_biopsy_age
  #   if (!check_range(BM_biopsy_age, valid_ranges$BM_biopsy_age)) {
  #     show_alert(paste0("Warning: CELFIB-AI was trained on patients with Age ranging from <b>5 to 90 years</b>. Please verify input data. Predictions using outlier data should be interpreted with extreme caution."))
  #   }
  # })
  #
  # observe({
  #   req(input$vgm)
  #   vgm <- input$vgm
  #   if (!check_range(vgm, valid_ranges$vgm)) {
  #     show_alert("Warning: CELFIB-AI was trained on patients with MCV ranging from <b>60 to 140 fL</b>. Please verify input data. Predictions using outlier data should be interpreted with extreme caution.")
  #   }
  # })


  #################### ==================== App modules ====================  ####################

  mod_prediction_server("prediction_1", r=r)
}
