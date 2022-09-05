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
  fluidPage(
    tabsetPanel(type = "pills",

  #data plots
      tabPanel("Plots", value="tabPlot",
               plotlyOutput("dataPlot")),

  #data table
      tabPanel("Table", value="tabTable", br(),
               DTOutput("dataTable", height="700px")),

  #matching information
  tabPanel("Match Info", value="tabMInfo",
           fluidRow(
             "Matches for this simulation have occurred using the parameters below.",br(),
             span("Formula used for matching: ", htmlOutput("f.treat")),
             span("Formula used for estimating treatment effect: ", htmlOutput("f.outcome")),
             span("Treatment effect: ", textOutput("TE")),
             tableOutput("matchtable"))
  ))

  )
  )}

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
