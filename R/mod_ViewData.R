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
           "Matches for this simulation have occurred using the parameters below.",
           br(),
           br(),
           span("Formula used for estimating treatment effect: ",
                htmlOutput("f.outcome", inline = T)),br(),
           br(),
           span("Treatment effect: ", textOutput("TE", inline = T)),
           br(),
           tableOutput("matchtable"))
      ),

  #distances
      tabPanel("Calliper Insights", value="tabDist",
           fluidRow("The application of a calliper is generally to attempt to remove outlying matches that may not improve covariate balance. The plots below show the distance between pairs in an unadjusted and standardised form. It also shows them in both the order they were matched in, and the order of their distance from closest to furthest.",br()," To see how your estimate would be changed by removing the outliers, take note of the number of the first match you want to remove, then go back to the main screen and move the slider into number prior to that position. This will give you some insight into how callipers work. Did it improve your estimate?", br(),
             plotlyOutput("distsPlot", height="600px")
           )),

  #would like comparison of estimate using different formulas
      tabPanel("Treatment Effects", value="tabTE", br(),
         fluidRow("Below is a plot that summarises the possible treatment effects of the different possible methods and their confidence intervals. The dotted red line represents the true treatment effect if it is known."),br(),
         strong("Select the methods you would like to view the estimates for"),
         fluidRow(
           column(8,
                  selectizeInput("methods", NULL,
                                 choices=c("Method 1", "Method 2", "Stratified",
                                           "Unadjusted", "Weighted"),
                                 selected=c("Method 1", "Method 2", "Unadjusted"),
                                 multiple=T, width="100%")),
           column(4, actionButton("butMethods", "Apply"))),
                  plotlyOutput("ests.plot", height="700px")
               )

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
