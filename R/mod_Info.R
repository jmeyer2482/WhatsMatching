#' Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
                    div.MathJax_Display{
                    text-align: left !important;
                    }"))
    ),

  fluidPage(
    tabsetPanel(type="pills",
      tabPanel("App Assumptions",withMathJax(), br(),
            includeMarkdown("inst/app/www/Assumptions.html")),
      tabPanel("Causal Inference",withMathJax(), br(),
            includeMarkdown("inst/app/www/Causal_Inference.html")),
      tabPanel("Matching", br(),
            htmltools::includeMarkdown("inst/app/www/Matching.html")),
      tabPanel("Propensity Score",withMathJax(),br(),
            includeHTML("inst/app/www/Propensity_Score.html")),
      tabPanel("Mahalanobis", br(),
            includeMarkdown("inst/app/www/Mahalanobis.html")),
      tabPanel("PSM Paradox", br(),
            includeMarkdown("inst/app/www/PSM_Paradox.html"))
     )#tabpanel

    )#fluidpage

  )#taglist
}

#' Info Server Functions
#'
#' @noRd
mod_Info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Info_ui("Info_1")

## To be copied in the server
# mod_Info_server("Info_1")
