#' ViewData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ViewData_ui <- function(id){
  ns <- NS(id)
  tagList(

    tabsetPanel(type = "pills",
                tabPanel("Plots", value="tabPlot",
                         plotOutput("dataPlot")),
                tabPanel("Table", value="tabTable",
                         tableOutput("dataTable")),
                tabPanel("Match Info", value="tabMInfo",
                         tableOutput("matchtable"))
    )

  )
}

#' ViewData Server Functions
#'
#' @noRd
mod_ViewData_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_ViewData_ui("ViewData_1")

## To be copied in the server
# mod_ViewData_server("ViewData_1")
