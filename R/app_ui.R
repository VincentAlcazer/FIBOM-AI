#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(

      header = dashboardHeader(
        title = "FIBOM-AI v1.1",
        tags$li(
          class = "dropdown",
          shinyWidgets::dropMenu(
            shinyWidgets::dropdownButton(
              label = "Info",
              status = "success",
              icon = icon("info")
            ),
            h3(strong("Reference")),
            br(),
            h4(strong("FIBOM-AI: A machine learning model to predict bone marrow fibrosis")),
            br(),
            p("This work is currently not published"),
             placement = "bottom",
            arrow = TRUE
          )
        )
      ),

      #################### ==================== SIDEBAR ====================  ####################

      sidebar <- shinydashboard::dashboardSidebar(
        # CSS & JS externes
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "www/sidebar.css"),
          tags$script(src = "www/sidebar.js")
        ),

        # ----- Contenu -----
        div(class = "sidebar",

            # Bouton (plein largeur)
            div(class = "sb-grid-1",
                div(style = "margin-top:20px"),
                actionButton(
                  "run",
                  tagList(icon("play-circle"), span("Predict Fibrosis")),
                  class = "btn-primary"
                )
            ),

            # Titre
            h2("Variables", align = "center"),

            # Âge (1 colonne)
            div(style = "margin-top:-20px"),
            div(class = "sb-grid-1",
                compact_num("age", "Age", value = 40, step = 1, min = 10, max = 100),
                uiOutput("age_alert")
            ),

            # ---- Lignée rouge ----
            div(style = "margin-top:40px"),
            div(class = "param-section-title", "Red cells"),
            div(style = "margin-top:-10px"),
            div(class = "sb-grid-2",
                compact_num("hemoglobin",  "Hemoglobin (g/L)",     value = 156, step = 0.1),
                compact_num("hematocrite", "Hematocrit (%)",     value = 49.0, step = 0.1),
                compact_num("hematies", "RBC (T/L)",     value = 5.9, step = 0.1),
                compact_num("MCV",         "MCV (fL)",            value = 82.7, step = 0.1),
                compact_num("MCH",         "MCH (TCMH)",     value = 26.4, step = 0.1),
                compact_num("MCHC",        "MCHC (CCMH)",    value = 320, step = 0.1),
                compact_num("IDR",         "RDW (IDR)",      value = 15.4, step = 0.1)
            ),

            # ---- Plaquettes ----
            div(style = "margin-top:40px"),
            div(class = "param-section-title", "Platelets"),
            div(style = "margin-top:-10px"),
            div(class = "sb-grid-2",
                compact_num("platelets", "Platelets", value = 244, step = 1),
                compact_num("mpv",       "MPV",       value = 10.6, step = 0.1)
            ),

            # ---- Lignée blanche (2 par ligne) ----
            div(style = "margin-top:40px"),
            div(class = "param-section-title", "White cells (all in G/L)"),
            div(style = "margin-top:-10px"),
            div(class = "sb-grid-2",
                compact_num("WBC",    "WBC",         value = 5.84, step = 0.1),
                compact_num("ANC",    "ANC",         value = 3.99, step = 0.1),
                compact_num("ALC",    "ALC",         value = 1.10, step = 0.1),
                compact_num("mono",   "Monocytes",   value = 0.54, step = 0.1),
                compact_num("eosino", "Eosinophils", value = 0.16, step = 0.1),
                compact_num("baso",   "Basophils",   value = 0.06, step = 0.01)
            ),

            # ---- Cellules immatures ----
            div(style = "margin-top:40px"),
            div(class = "param-section-title", "Immature cells (all in %)"),
            div(style = "margin-top:-10px"),
            div(class = "sb-grid-2",
                compact_num("erythroblasts", "Erythroblasts", value = 0, step = 0.1),
                compact_num("myelemia",      "Myelemia",      value = 0.3, step = 0.1),
                compact_num("myeloblasts",   "Myeloblasts",   value = 0, step = 0.1),
                div()  # équilibrage (case vide)
            ),

            ###script pour alertes
            tags$script(HTML("
  $(document).on('blur', 'input[type=\"number\"]', function() {
    var inputId = this.id + '_blur';
    var inputVal = this.value;
    Shiny.setInputValue(inputId, inputVal, {priority: 'event'});
  });
"))

        )
      ), #sideboard dashbar

      #################### ==================== BODY ====================  ####################

      body = dashboardBody(

        mod_prediction_ui("prediction_1")
       #  tabItems(
       #
       #    ##### ===== Home
       #
       #    tabItem("prediction", mod_prediction_ui("prediction_1")),
       #
       #    ##### ===== Info / Help
       #
       #    tabItem("info", mod_info_ui("info_1"))
       #
       # )#tabItems

      ) #body

    ) #dashboard page
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FIBOMAI"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
