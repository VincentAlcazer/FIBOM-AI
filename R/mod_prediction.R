#' prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_prediction_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(
      column(12,

             h1("Welcome to FIBOM-AI"),
             p(HTML("<b>FIBOM-AI (Artificial Intelligence-based Prediction of BOne Marrow FIBrosis)</b> is a free and open-source software
             designed to assist clinical hematologists and biologists in the diagnosis of Myeloproliferative Neoplasms using only routine biological parameters.
               The model has been trained and validated on more than 2,000 patients for the prediction of bone marrow biopsy fibrosis.")),
             p("FIBOM-AI is provided for research use only.
    Diagnosis or clinical decisions made based on FIBOM-AI predictions are solely the responsibility of the user."),

             h2("Main result"),
             htmlOutput(ns("prediction_text"),style = "font-size:120%;"),
             br(),
             p(HTML(
               paste0(
                 "<font color='#21bf88'><i><b>Confident prediction</b></font> has a sensitivity of 95.7% ",
                 "(with a negative predictive value of 97.2%) ",
                 "and a specificity of 99.7% (positive predictive value of 96.8%).</i>"
               )
             )),
             p(HTML(
               paste0(
                 "<font color='#E88B1A'><i><b>Overall prediction</b></font> has an accuracy of 89% [range: 82&ndash;93.4%] ",
                 "across all retrospective cohorts, ",
                 "and 85.7% [range: 85&ndash;87%] in the prospective cohorts.</i>"
               )
             )),

             plotOutput(ns("main_bar")),
             br(),

             box(width=6, title  = "Individual scores", collapsible = T,
                 p("Solid lines determine the overall prediction cutoff, dashed lines determine the confident zone."),
                 shinycssloaders::withSpinner(plotOutput(ns("prediction_indiv")),type = 6)),

             box(width=6, title = "Outlier detection", collapsible = T,
                 p("PCA of reference data (including: training (n=893), testing (n=292) and
                    validation (n=801) sets"),
                 shinycssloaders::withSpinner(plotOutput(ns("density_plot")),type = 6))

      ), #column,

      # column(2,
      #        absolutePanel(
      #          width = 200, right = 20, draggable = T,
      #          style = "opacity: 0.85",
      #          wellPanel(
      #
      #
      #
      #
      #          )# Wellpanel
      #
      #
      #        ) # Absolutepanel
      # ) # Column



    ) #fluidPage

  )
}

#' prediction Server Functions
#'
#' @noRd
mod_prediction_server <- function(id, newdata){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    newdata_pc <- reactive({

      req(newdata())
      dat <- predict(pca,
                      newdata=newdata())[1:2]
      return(dat)

    })

    # Sidebar fields left empty (labels), for traceability of the result
    missing_inputs <- reactive({
      req(newdata())
      empty <- is.na(unlist(newdata()[1, input_vars]))
      unname(input_labels[names(input_vars)[empty]])
    })

    output$density_plot <- renderPlot({
      req(newdata())

      p <- pca_plot + default_theme
      if (anyNA(newdata_pc())) {
        p + labs(subtitle = "Patient not shown: the PCA needs all values")
      } else {
        p + annotate("point", x = newdata_pc()[1], y = newdata_pc()[2], size = 4.5)
      }
      })


    ##### ===== Prediction

    fib_pred <- reactive({
      req(newdata())

      prediction <- predict_fibrosis(newdata())

      return(prediction)
    })


    # --- Seuils ---
    thr <- reactive(list(
      low_conf_no   = as.numeric(params_df$sen_threshold),       # p ≤ 0.10  => Confident No
      gray_lo       = 0.45,       # zone grise [0.30–0.70]
      gray_hi       = 0.55,
      low_conf_yes  = as.numeric(params_df$spe_threshold)        # p ≥ 0.90  => Confident Yes
    ))


    # --- Uncertainty
    pF_uncertainty <- reactive({

      get_prediction_interval(fib_pred()[["Yes"]], lookup_table = uncertainty_lookup)

    })




    # --- Graphique principal ---
    output$main_bar <- renderPlot({
      req(fib_pred())

      pF <- fib_pred()[["Yes"]]

      cols <- c(
        conf_no   = "#1e466e",  # rouge vif (confident NON)
        leans_no  = "#72bcd5",  # rouge clair / pastel
        gray_zone = "#b0bec5",  # gris bleuté clair
        leans_yes = "#f7aa58",  # vert doux / pastel
        conf_yes  = "#e76254"   # vert vif (confident OUI)
      )

      fib_back <- data.frame(
        xmin = c(0,thr()[["low_conf_no"]],
                 thr()[["gray_lo"]],thr()[["gray_hi"]],
                 thr()[["low_conf_yes"]]),
        xmax = c(thr()[["low_conf_no"]],
                 thr()[["gray_lo"]],thr()[["gray_hi"]],
                 thr()[["low_conf_yes"]],1),
        ymin = rep(0.6, 5),
        ymax = rep(1,5),
        zone = c("conf_no","leans_no","gray_zone","leans_yes","conf_yes")
      )


      ggplot() +
        # une seule bande de confiance pour toute la hauteur
        geom_rect(data = fib_back,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone),
                  alpha = 0.75, color = NA) +
        scale_fill_manual(values = cols, guide = "none") +

        # curseur fibrose
        geom_errorbar(aes(y = 0.8,
                          xmin = pF_uncertainty()[["lower"]],
                          xmax = pF_uncertainty()[["upper"]]),
                      orientation = "y", width = 0.02, linewidth = 1.5, color = "#263238") +
        geom_point(aes(x = pF, y = 0.8), size = 5, color = "#263238") +

        # étiquettes de lignes
        annotate("text", x = 0.02, y = 1.05, label = "Grade 2-3 fibrosis probability (%):", fontface = "bold",
                 hjust = 0, size = 6, color = "#263238") +
        annotate("text", x = 0.02, y = 1.025,
                 label = "Error bars represent the empirical 95% uncertainty range derived from prediction variability in similar cases.",
                 fontface = "italic", hjust = 0, size = 5, color = "black") +
        annotate("text", x = 0, y = 0.58, label = "g0-1 Fibrosis", fontface = "bold",
                 hjust = 0, size = 6, color = "#263238") +
        annotate("text", x = Inf, y = 0.58, label = "g2-3 Fibrosis", fontface = "bold",
                 hjust = 1, size = 6, color = "#263238") +

        # valeurs
        annotate("label",
                 x = min(max(pF, 0.03), 0.97), y = 0.85,  # garde l'étiquette dans le cadre
                 label = sprintf("%.1f%%", 100*pF),
                 size = 5, label.size = 0, fill = "#eceff1", color = "#263238", fontface = "bold") +
        # graduations 0–50–100
        geom_segment(aes(x = 0,   xend = 0,   y = 0.45, yend = 0.55), linewidth = 0.6, color = "#607d8b") +
        geom_segment(aes(x = 0.5, xend = 0.5, y = 0.45, yend = 0.55), linewidth = 0.6, color = "#607d8b") +
        geom_segment(aes(x = 1,   xend = 1,   y = 0.45, yend = 0.55), linewidth = 0.6, color = "#607d8b") +


        # labels sous les ticks
        annotate("text", x = 0,   y = 0.5, label = "0%", hjust=0,    size = 5) +
        annotate("text", x = 0.51, y = 0.5, label = "50%",   size = 5) +
        annotate("text", x = Inf,   y = 0.5, label = "100%", hjust = 1, size = 5) +


        coord_cartesian(xlim = c(0, 1), ylim = c(0.45, 1.1), expand = FALSE) +
        theme_void() +
        theme(plot.margin = margin(5, 12, 5, 12))
    })



    # ##### ===== Individual scores


    output$prediction_indiv <- renderPlot({

      req(fib_pred())

      pred_all %>%
        rbind(data.frame(Yes = fib_pred()[["Yes"]], No = NA, Fibrosis = "Prediction")) %>%
        ggplot(aes(x = Yes, y = "", color = Fibrosis,
                   shape = Fibrosis, alpha = Fibrosis, size = Fibrosis)) +
        geom_jitter(height = 0.15) +
        #ylim(0.8,1.2) +
        labs(x = "Predicted fibrosis probability (%)", y = "") +
        geom_vline(aes(xintercept = 0.5)) +
        geom_vline(aes(xintercept = as.numeric(params_df$sen_threshold)), linetype = 2) +
        geom_vline(aes(xintercept = as.numeric(params_df$spe_threshold)), linetype = 2) +
        default_theme +
        theme(legend.position = "top") +
        scale_color_manual(values = c("#f7aa58","#72bcd5","black")) +
        scale_alpha_manual(values = c(1,1,1)) +
        scale_size_manual(values = c(2,2,8)) +
        scale_shape_manual(values = c(15,16,18))


    })



    ##### ===== Prediction text

    output$prediction_text <- renderText({

      req(fib_pred())
      pF <- fib_pred()[["Yes"]]
      raw_pred <-   paste0(100*round(pF,4),"%")


      confident_pred <-   if(pF < as.numeric(params_df$sen_threshold) || pF > as.numeric(params_df$spe_threshold) ){

        paste0("<font color=\"#21bf88\">",
               paste0("Confident prediction"),
               " </font>")

      } else {

        paste0("<font color=\"#E88B1A\">",
               paste0("Overall prediction"),
               " </font>")
      }



      text <- paste(paste0("<b>Grade 2-3 fibrosis probability (Raw prediction): ",raw_pred,"</b><br>",
                           "<i>Prediction confidence: <b>",
                           #"Affined prediction using overall model: <b>", overall_pred,"</b><br>",
                            confident_pred,"</b></i><br><br>"

                   ),
      sep = "\n")

      if (length(missing_inputs()) > 0) {
        text <- paste0(text,
                       "<font color=\"#E88B1A\"><b>&#9888; Computed with missing values:</b> ",
                       htmltools::htmlEscape(paste(missing_inputs(), collapse = ", ")),
                       ". The prediction is less reliable.</font><br><br>")
      }

      return(text)

    })


  })
}

## To be copied in the UI
# mod_prediction_ui("prediction_1")

## To be copied in the server
# mod_prediction_server("prediction_1")
