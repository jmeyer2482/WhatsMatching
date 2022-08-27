#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic

#set up fluidpage
    fluidPage(
  #title
      fluidRow(
        column(width = 12, align = "left",
               h1("The Propensity Score Paradox"))),
  #buttons
      fluidRow(
        column(width = 2, align = "center",
               actionButton("butMatchData","Random Matches")),
        column(width = 2, align = "center",
               actionButton("butGetData","Create Matches")),
        column(width = 2, align = "center",
               actionButton("butViewData","View Data")),
        column(width = 6)), br(),br(),

  #text description
      fluidRow(column(width = 6, align = "center", strong(textOutput("M1.text"))),
               column(width = 6, align = "center", strong(textOutput("M2.text")))),

  #pop up for selecting data
      bsModal(id="modGetData", title="Select Data", trigger="butGetData",
              size = "large", mod_SimData_ui("SimData_1"),
              tags$head(tags$style("#modGetData .modal-footer{ display:none}"))
      ),

  #pop up for match settings
      bsModal(id="modMatchSettings", title="Update Matching Settings",
              trigger="butMatchSettings", size = "large",
              mod_MatchSettings_ui("MatchSettings_1"),
              tags$head(tags$style("#modMatchSettings .modal-footer{ display:none}"))
      ),

  #pop up for viewing data
      bsModal(id="modViewData", title="View Data", titlePanel("Simulate Data"),
              trigger = "butViewData", size = "large", mod_ViewData_ui("ViewData_1"),
              tags$head(tags$style("#modViewData .modal-footer{ display:none}"))
      ),

  #matching plot panel
      mainPanel(width = 12,
        plotlyOutput("distPlot", height = "900px")
      )

    )#close fluidpage

  ) #close taglist
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
      app_title = "PSM.golex"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
