#' ViewData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @importFrom DT DTOutput
#'

mod_ViewData_ui <- function(id){
  ns <- NS(id)
  tagList(
  fluidPage(
    tabsetPanel(type = "pills",

  #data plots
      tabPanel("Data Plots", value="tabPlot",
               plotlyOutput("dataPlot", height="600px")),

  #data table
      tabPanel("Data Table", value="tabTable", br(),
               DTOutput("dataTable", height="700px")),

  #matching information
      tabPanel("Match Info", value="tabMInfo",
               fluidRow(
                 "Matches for this simulation have occurred using the parameters below.",br(),
                 br(),
                 span("Formula used for estimating treatment effect: ",
                      htmlOutput("f.outcome", inline = T)),br(),
                 br(),
                 span("Treatment effect: ", textOutput("TE", inline = T)),
                 br(),
                 tableOutput("matchtable"))
      ),

  #distances
      tabPanel("Caliper Insights", value="tabDist",
               fluidRow("There are four plots below. The top two show the actual distances between each matched pair by both the order of the sequential matches (left side) and by the order of the distance (ride side). The bottom two plots show a standardised difference which is measured by the number of standard deviations from zero the distance of each match is. The purpose of this data is to give insights into where the application of calipers may have some utility.",
                 plotlyOutput("distsPlot", height="600px")
               )),
  #distances by matched sequence
      tabPanel("TBC", value="tabSeq",
               fluidRow("",
                        plotlyOutput("seqPlot", height="600px")
               ))

    )#close tabsetPanel

  )#close fluidPage
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
