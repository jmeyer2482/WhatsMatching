#' SimData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'

X1.t <- code("X1")
X2.t <- code("X2")
t.t <- code("t")
y.t <- code("y")

te.txt <- HTML(paste0(strong("Treatment Effect"), br(),
                "The treatment effect controls the effect that the ",
                "treatment will have on the outcome (", y.t, ")."))

mean.txt <- HTML(paste0(strong("Mean (average)"), br(),
                "The mean is used to generate data drawn from a ",
                "normal distribution for variables ",
                X1.t, " and ", X2.t, "."))

sd.txt <- HTML(paste0(strong("Standard Deviation"), br(),
               "The standard deviation is used to generate data ",
               "drawn from a normal distribution for variables ",
               X1.t, " and ", X2.t, "."))

rho.txt <- HTML(paste0(strong("Covariate Correlation"), br(),
                "The covariate correlation is used to determine the strength ",
                "of the correlation between ", X1.t, " and ", X2.t,
                ". Select 0 for no correlation or a positive or negative value ",
                "for a positive or negative ", "correlation between ", X1.t,
                " and ", X2.t, "."))

xt.txt <- HTML(paste0(strong("Effect on treatment ("), t.t, strong(")"), br(),
                "The effect on treatment controls the degree to which ",
                X1.t, " and ", X2.t, " change the chance of being treated. ",
                "When set to a value other than 0, it can be used to replicate ",
                "a confounding variable."))

xy.txt <- HTML(paste0(strong("Effect on outcome (", y.t, ")"), br(),
                "The effect on outcome controls the degree to which ",
                X1.t, " and ", X2.t, " change the outcome. When set to 0 ",
                "there is no effect on the outcome."))

y.txt <- HTML(paste0(strong("Value of "), y.t, strong(" (outcome)"), br(),
                "This is the value of the outcome before it has been ",
                "subjected to the effects of ", X1.t, ", ", X2.t, ", or ",
                t.t, "."))

range.txt <- HTML(paste0(strong("Variable Range"), br(),
                "This will set the range of a uniform distribution ",
                "for both ", X1.t, " and ", X2.t, "."))

overlap.txt <- HTML(paste0(strong("Overlap"), br(),
                "Use these sliders to adjust the amount of overlap ",
                "between the treated and control groups. Selecting 0 ",
                "represents no overlapSelect a value to set the mean value for the",
                "normally distributed variables ", X1.t, " and ", X2.t, "."))


mod_SimData_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(theme = shinytheme("spacelab"),
        tabsetPanel(#type = "pills",

###Simulation 1
      tabPanel("Simulation 1", value="Sim1",
      sidebarLayout(
        sidebarPanel(style="height: 700px;",

                             # create.sim1.data <- function(te=2)
          fluidRow(
                   strong("Treatment Effect"),
                   icon("question-sign", lib="glyphicon", id=ns("te1")),
                   bsPopover(id=ns("te1"),title=NULL,content=te.txt ,
                             "right",options = list(container = "body"))
                   ),
          fluidRow(
            column(12,
                  sliderInput("sim1.TE", NULL,
                              min=-25, max=25,value=2, step=1, ticks=F))
                  ),
          fluidRow(column(12, align = "right",
                          actionButton("applySim1", "Apply"))
                   )),

          mainPanel(plotlyOutput("sim1plot", height = "700px")
                    )
         )),
###Simulation 2
      tabPanel("Simulation 2", value="Sim2",
      sidebarLayout(
        sidebarPanel(style="height: 700px;",

          fluidRow(
            strong("Treatment Effect"),
            icon("question-sign", lib="glyphicon", id=ns("te2")),
            bsPopover(id=ns("te2"),title=NULL,content=te.txt ,
                      "right",options = list(container = "body"))),
          fluidRow(column(12,
            sliderInput("sim2.TE", NULL,
                      min=-25, max=25,value=2, step=.1, ticks=F))
            ),

          fluidRow(
            strong("Range"),
            icon("question-sign", lib="glyphicon", id=ns("range")),
            bsPopover(id=ns("range"),title=NULL, content=range.txt,
                      "right",options = list(container = "body"))),
          fluidRow(column(12,
            sliderInput("sim2.X1.val", NULL,
                        min=-25, max=25,value=c(0,5), step=1, ticks=F))),

          fluidRow(strong("Group Overlap"),
                   icon("question-sign", lib="glyphicon", id=ns("overlap")),
                   bsPopover(id=ns("overlap"),title=NULL, content=overlap.txt,
                             "right",options = list(container = "body"))),
          fluidRow(
            column(6,
                   sliderInput("sim2.x.overlap", "X1",
                   min=0, max=25,value=4, step=1, ticks=F)),
            column(6,
                   sliderInput("sim2.y.overlap", "X2",
                      min=0, max=25,value=4, step=1, ticks=F))),
          fluidRow(column(12, align = "right",
                          actionButton("applySim2", "Apply")))
          ),

          mainPanel(plotlyOutput("sim2plot", height = "700px"))
          )),
###Simulation 3
          tabPanel("Simulation 3", value="Sim3",
           sidebarLayout(
             sidebarPanel(style="height: 700px;",#width=6,
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
          fluidRow(#align="center",
            strong("Treatment Effect"),
            icon("question-sign", lib="glyphicon", id=ns("te3"))),
          fluidRow(column(12,
            sliderInput("sim3.TE",label=NULL, min=-25, max=25,value=2, step=1, ticks=F),
            bsPopover(id=ns("te3"),title=NULL,content=te.txt ,
                      "right",options = list(container = "body")))
            ),

          fluidRow(
            strong("Covariate Correlation"),
            icon("question-sign", lib="glyphicon", id=ns("rho")),
            bsPopover(id=ns("rho"),title=NULL,content=rho.txt ,
                      "right",options = list(container = "body"))),

          fluidRow(column(12,
            sliderInput("sim3.rho", label=NULL,
                      min=-1, max=1,value=0, step=0.1, ticks=F))
            ),

          fluidRow(#align="center",
            strong("Base value of y (outcome)"),
            icon("question-sign", lib="glyphicon", id=ns("y")),
            bsPopover(id=ns("y"),title=NULL,content=y.txt ,
                      "right",options = list(container = "body"))),

          fluidRow(column(12,
            sliderInput("sim3.y", label=NULL,
                      min=-25, max=25,value=2, step=1, ticks=F))
            ),
          # fluidRow(column(6, strong("X1 values")),
          #          column(6, strong("X2 Values"))),

          fluidRow(#align="center",
            strong("Mean (average)"),
            icon("question-sign", lib="glyphicon", id=ns("mean")),
            bsPopover(id=ns("mean"),title=NULL,content=mean.txt ,
                      "right",options = list(container = "body"))),
          fluidRow(
            column(6, numericInput("sim3.X1m", "X1",
                            min=-25, max=25,value=0, step=1)
                   ),
            column(6, numericInput("sim3.X2m", "X2",
                            min=-25, max=25,value=0, step=1))
            ),

          fluidRow(
            strong("Standard Deviation"),
            icon("question-sign", lib="glyphicon", id=ns("sd")),
            bsPopover(id=ns("sd"),title=NULL,content=sd.txt ,
                      "right",options = list(container = "body"))),
          fluidRow(
            column(6,
                   numericInput("sim3.X1sd", "X1",
                   min=1, max=30,value=2, step=1)
                   ),
            column(6,
                   numericInput("sim3.X2sd", "X2",
                   min=1, max=30,value=2, step=1))),

          fluidRow(
            strong("Effect on treatment"),
            icon("question-sign", lib="glyphicon", id=ns("xt")),
            bsPopover(id=ns("xt"),title=NULL,content=xt.txt ,
                      "right",options = list(container = "body"))),
          fluidRow(
            column(6,
                  numericInput("sim3.X1t", "X1",
                  min=-25, max=25,value=0, step=1)),
            column(6,
                  numericInput("sim3.X2t", "X2",
                  min=-25, max=25,value=0, step=1))),

          fluidRow(#align="center",
            strong("Effect on outcome"),
          icon("question-sign", lib="glyphicon", id=ns("xy")),
          bsPopover(id=ns("xy"),title=NULL,content=xy.txt ,"right",options = list(container = "body"))),
          fluidRow(column(6,
                numericInput("sim3.X1y", "X1",
                min=-25, max=25,value=2, step=1)),
          column(6,
                numericInput("sim3.X2y", "X2",
                min=-25, max=25,value=2, step=1))),

          fluidRow(column(12, align = "right",
                          actionButton("applySim3", "Apply")))),
         mainPanel(plotlyOutput("sim3plot", height = "700px"))

         ))
    ),
    actionButton("usedata", "Please generate some data.", width = "100%",
                 style="font-weight: bold")

    )#tabset panel
  )#fluidpage

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
