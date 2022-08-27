#' SimData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SimData_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(titlePanel("Simulate Data"),
            "You have elected to simulate your data for your exploration of the ",
            "PSM Paradox.", br(), br(),
            "This will include a treatment and an outcome variable plus up to 5 ",
            "other covariates. Each variable can be specified as normal, ",
            "uniform, or binomial distribution. For ", br(), br(),
            "Causal relationships between these variables will",
            "be able to be established in the next section.",

        tabsetPanel(type = "pills",
###Simulation 1
          tabPanel("Simulation 1", value="Sim1",br(),
         sidebarLayout(
           sidebarPanel(

                             # create.sim1.data <- function(te=2)
                   "Enter the treatment effect:",
          sliderInput("sim1.TE", "Treatment Effect",
                      min=-25, max=25,value=2, step=1, ticks=F),
          actionButton("applySim1", "Apply")),
          mainPanel(shiny::plotOutput("sim1plot")))),
###Simulation 2
          tabPanel("Simulation 2", value="Sim2",
          sidebarLayout(
            sidebarPanel(br(),
                         "This simulation draws from a uniform distribution. ",
                         "blah blah",br(),br(),
          #create.sim2.data <- function(te=2, g1_min=0, g1_max=5,
          #                             g2_shift_X1=1, g2_shift_X2=1)
          # "Enter the treatment effect on the outcome:",
          sliderInput("sim2.TE", "Treatment Effect",
                      min=-25, max=25,value=2, step=1, ticks=F),br(),
          # "Enter the minimum value of X1:",
          # sliderInput("sim2.X1.min", "Control group min", min=-25, max=25,value=0),br(),
          # "Enter the maximum value of X1:",
          sliderInput("sim2.X1.val", "Range of uniform distribution",
                      min=-25, max=25,value=c(0,5), step=1, ticks=F),br(),
          # "Enter the difference between treated and control units for X1:",
          sliderInput("sim2.x.overlap", "X1 overlap between treated and control groups",
                      min=0, max=25,value=4, step=1, ticks=F),br(),
          # "Enter the difference between treated and control units for X2:",
          sliderInput("sim2.y.overlap", "X2 overlap between treated and control groups",
                      min=0, max=25,value=4, step=1, ticks=F),
          actionButton("applySim2", "Apply")
          ),
          mainPanel(plotOutput("sim2plot"))
          )),
###Simulation 3
          tabPanel("Simulation 3", value="Sim3",
           sidebarLayout(
             sidebarPanel(#width=6,
          #     create.sim3.data <- function(
          #       n = 200, mean1 = 0, mean2 = 0, sd1 = 1, sd2 = 1,
          #       rho = .3, #must be between -1 and 1 inclusive.
          #
          #       # Generating t
          #       weight_t1 = 0.5, #effect of X1 on t
          #       weight_t2 = 0.5,  #effect of X2 on t
          #
          #       # Generating y
          #       weight_y0 = 2, #base value of y
          #       weight_y1 = 1, #effect of X1 on y
          #       weight_y2 = -1, #effect of X2 on y
          #       te = 2 # True treatment effect
          # "Enter the treatment effect:",
          sliderInput("sim3.TE", "Treatment Effect",
                      min=-25, max=25,value=2, step=1, ticks=F),
          sliderInput("sim3.rho", "Correlation",
                      min=-1, max=1,value=0.5, step=0.1, ticks=F),
          sliderInput("sim3.y", "Base value of outcome",
                      min=-25, max=25,value=2, step=1, ticks=F),
          fluidRow(column(6, strong("X1 values")),
                   column(6, strong("X2 Values"))),
          fluidRow(column(6,
          sliderInput("sim3.X1m", "Mean",
                      min=-25, max=25,value=2, step=1, ticks=F)),
          column(6,
          sliderInput("sim3.X2m", "Mean",
                      min=-25, max=25,value=2, step=1, ticks=F))),

          fluidRow(column(6,
          sliderInput("sim3.X1sd", "Standard Deviation",
                      min=1, max=30,value=2, step=1, ticks=F)),
          column(6,
          sliderInput("sim3.X2sd", "Standard Deviation",
                      min=1, max=30,value=2, step=1, ticks=F))),

          fluidRow(column(6,
          sliderInput("sim3.X1t", "Effect on treatment",
                      min=-25, max=25,value=2, step=1, ticks=F)),
          column(6,
          sliderInput("sim3.X2t", "Effect on treatment",
                      min=-25, max=25,value=2, step=1, ticks=F))),

          fluidRow(column(6,
          sliderInput("sim3.X1y", "Effect on outcome",
                      min=-25, max=25,value=2, step=1, ticks=F)),
          column(6,
          sliderInput("sim3.X2y", "Effect on outcome",
                      min=-25, max=25,value=2, step=1, ticks=F))),
          actionButton("applySim3", "Apply")),
         mainPanel(plotOutput("sim3plot"))

         ))
    ),
    actionButton("usedata", "Please generate some data", width = "100%",
                 style="font-weight: bold")

))

}



#' SimData Server Functions
#'
#' @noRd
mod_SimData_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_SimData_ui("SimData_1")

## To be copied in the server
# mod_SimData_server("SimData_1")
