#' SimData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyBS bsPopover
#'
#'

X1.t <- code("X1")
X2.t <- code("X2")
t.t <- code("t")
y.t <- code("y")

te.txt <- HTML(
  paste0(
    "The treatment effect controls the effect that the ",
    "treatment will have on the outcome (",
    y.t,
    ")."
  )
)

rho.txt <- HTML(
  paste0(
    "The covariate correlation is used to determine the strength ",
    "of the correlation between ",
    X1.t,
    " and ",
    X2.t,
    ". Select 0 for no correlation or a positive or negative value ",
    "for a positive or negative ",
    "correlation between ",
    X1.t,
    " and ",
    X2.t,
    "."
  )
)

xt.txt <- HTML(
    paste0(
      "The effect on treatment controls the degree to which ",
      X1.t,
      " and ",
      X2.t,
      " change the chance of being treated. ",
      "When set to a value other than 0, it can be used to replicate ",
      "a confounding variable."
    )
  )

xy.txt <- HTML(
  paste0(
    "The effect on outcome controls the degree to which ",
    X1.t,
    " and ",
    X2.t,
    " change the outcome. When set to 0 ",
    "there is no effect on the outcome."
  )
)

y.txt <-
  HTML(
    paste0(
     "This is the value of the outcome before it has been ",
      "subjected to the effects of ",
      X1.t,
      ", ",
      X2.t,
      ", or ",
      t.t,
      "."
    )
  )

range.txt <- HTML(
  paste0(
    "This will set the range of a uniform distribution ",
    "for both ",
    X1.t,
    " and ",
    X2.t,
    "."
  )
)

overlap.txt <- HTML(
  paste0(
    "Use these sliders to adjust the amount of overlap ",
    "between the treated and control groups. Selecting 0 ",
    "represents no overlapSelect a value to set the mean value for the",
    "normally distributed variables ",
    X1.t,
    " and ",
    X2.t,
    "."
  )
)

rel.txt <- HTML(
  paste0(
    "These two variable control how ", X1.t, " and ", X2.t,
    " are related to ", t.t, " and ", y.t, ". The direction ",
    "of these relationsips is outlined below.", br(), br(),
    "Confounder: ", t.t, " &lArr; variable &rArr; ", y.t, br(),
    "Mediator: ", t.t, " &rArr; variable &rArr; ", y.t, br(),
    "Collider: ", t.t, " &rArr; variable &lArr; ", y.t, br(),
    "Ancenstor of t: variable &rArr; ", t.t, br(),
    "Ancestor of y: variable &rArr; ", y.t
  )
)


mod_SimData_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(

    ##Popovers
    bsPopover(
      id = ns("te1"),
      title = "<strong>Treatment Effect</strong>",
      content = paste0(
        "The treatment effect controls the effect that the ",
        "treatment will have on the outcome (",
        y.t,
        ")."
      ) ,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("te2"),
      title = "<strong>Treatment Effect</strong>",
      content = te.txt,
      "right",
      options = list(container = "body")
    ),

    bsPopover(id = ns("range"),
              title = "<strong>Range</strong>",
              content = range.txt,
              "right", options = list(container = "body")
    ),
    bsPopover(
      id = ns("overlap"),
      title = "<strong>Group Overlap</strong>",
      content = overlap.txt,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("te3"),
      title = "<strong>Treatment Effect</strong>",
      content = te.txt ,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("rel"),
      title = "<strong>Causal Relationship</strong>",
      content = rel.txt ,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("te4"),
      title = "<strong>Treatment Effect</strong>",
      content = te.txt ,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("rho"),
      title = "<strong>Covariate Correlation</strong>",
      content = rho.txt ,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("xt"),
      title = "<strong>Covariate Effect on Treatment</strong>",
      content = xt.txt,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("xy"),
      title = "<strong>Covariate Effect on Outcome</strong>",
      content = xy.txt,
      "right",
      options = list(container = "body")
    ),
    bsPopover(
      id = ns("fev.desc"),
      title = "<strong>FEV</strong>",
      content = paste0("This is a dataset from the <code>mplot</code> package. ",
                    "It has 5 observations and 614 individuals. The observations ",
                    "include <code>smoke</code>, <code>sex</code>, <code>age</code>",
                    ", <code>height</code>, and <code>fev</code>."),
      "right",
      options = list(container = "body")
    ),


    tabsetPanel(
      #type = "pills",

      ###Simulation 1
      tabPanel(
        "Simulation 1",
        value = "Sim1",
        sidebarLayout(
          sidebarPanel(
            style = "height: 700px;",

            # create.sim1.data <- function(te=2)
            fluidRow(
              strong("Treatment Effect"),
              icon("question-sign", lib = "glyphicon", id = ns("te1"))
            ),
            fluidRow(column(
              12,
              sliderInput(
                "sim1.TE",
                NULL,
                min = -25,
                max = 25,
                value = 2,
                step = 1,
                ticks = F
              )
            )),
            fluidRow(column(
              12, align = "right",
              actionButton("applySim1", "Preview Data")
            ))
          ),

          mainPanel(plotlyOutput("sim1plot", height = "700px"))
        )
      ),
      ###Simulation 2
      tabPanel(
        "Simulation 2",
        value = "Sim2",
        sidebarLayout(
          sidebarPanel(
            style = "height: 700px;",

            fluidRow(
              strong("Treatment Effect"),
              icon("question-sign", lib = "glyphicon", id = ns("te2"))
            ),
            fluidRow(column(
              12,
              sliderInput(
                "sim2.TE",
                NULL,
                min = -25,
                max = 25,
                value = 2,
                step = .1,
                ticks = F
              )
            )),

            fluidRow(
              strong("Range"),
              icon("question-sign", lib = "glyphicon", id = ns("range"))
            ),

            fluidRow(column(
              12,
              sliderInput(
                "sim2.X1.val",
                NULL,
                min = -25,
                max = 25,
                value = c(0, 5),
                step = 1,
                ticks = F
              )
            )),

            fluidRow(
              strong("Group Overlap"),
              icon("question-sign", lib = "glyphicon", id = ns("overlap"))
            ),

            fluidRow(column(
              6,
              sliderInput(
                "sim2.x.overlap",
                "X1",
                min = 0,
                max = 25,
                value = 4,
                step = 1,
                ticks = F
              )
            ),
            column(
              6,
              sliderInput(
                "sim2.y.overlap",
                "X2",
                min = 0,
                max = 25,
                value = 4,
                step = 1,
                ticks = F
              )
            )),

            fluidRow(column(
              12, align = "right",
              actionButton("applySim2", "Preview Data")
            ))
          ),

          mainPanel(plotlyOutput("sim2plot", height = "700px"))
        )
      ),
      ###Simulation 3
      tabPanel(
        "Simulation 3",
        value = "Sim3",
        sidebarLayout(
          sidebarPanel(
            style = "height: 700px;",

            fluidRow(
              strong("Treatment Effect"),
              icon("question-sign", lib = "glyphicon", id = ns("te3"))
            ),
            fluidRow(column(
              12,
              sliderInput(
                "sim3.TE",
                label = NULL,
                min = -5,
                max = 5,
                value = 2,
                step = .1,
                ticks = F
              )
            )),

            fluidRow(
              strong("Relationships"),
              icon("question-sign", lib = "glyphicon", id =
                     ns("rel"))
            ),
            fluidRow(column(12,
                            selectInput(
                              "sim3.X1rel", "X1",
                              c("Mediator", "Confounder",
                                "Collider", "Ancestor of t", "Ancestor of y")
                            ))),
            fluidRow(column(12,
                            selectInput(
                              "sim3.X2rel", "X2",
                              c("Mediator", "Confounder",
                                "Collider", "Ancestor of t", "Ancestor of y")
                            ))),

            fluidRow(column(
              12, align = "right",
              actionButton("applySim3", "Preview Data")
            ))
          ),
          mainPanel(plotlyOutput("sim3plot", height = "700px"))

        )
      ),

      ###Simulation 4
      tabPanel(
        "Simulation 4",
        value = "Sim4",
        sidebarLayout(
          sidebarPanel(
            style = "height: 700px;",
            #width=6,
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

            fluidRow(
              strong("Treatment Effect"),
              icon("question-sign", lib = "glyphicon", id = ns("te4"))
            ),
            fluidRow(column(
              12,
              sliderInput(
                "sim4.TE",
                label = NULL,
                min = -2,
                max = 2,
                value = 2,
                step = .1,
                ticks = F
              )
            )),


        #covariate correlation
            fluidRow(
              strong("Covariate Correlation"),
              icon("question-sign", lib = "glyphicon", id = ns("rho"))
            ),
            fluidRow(column(
              12,
              sliderInput(
                "sim4.rho",
                label = NULL,
                min = -0.5,
                max = 0.5,
                value = 0.5,
                step = 0.1,
                ticks = F
              )
            )),

        #effect on treatment
            fluidRow(
              strong("Effect on treatment"),
              icon("question-sign", lib = "glyphicon", id = ns("xt"))
            ),
            fluidRow(column(
              6,
              sliderInput(
                "sim4.X1t",
                "X1",
                ticks = F,
                min = -1,
                max = 1,
                value = 0.5,
                step = .1
              )
            ),
            column(
              6,
              sliderInput(
                "sim4.X2t",
                "X2",
                ticks = F,
                min = -1,
                max = 1,
                value = 1,
                step = .1
              )
            )),


      #Effect on y
          fluidRow(
              strong("Effect on outcome"),
              icon("question-sign", lib = "glyphicon", id = ns("xy"))
            ),
            fluidRow(column(
              6,
              sliderInput(
                "sim4.X1y",
                "X1",
                ticks = F,
                min = -1,
                max = 1,
                value = 1,
                step = .1
              )
            ),
            column(
              6,
              sliderInput(
                "sim4.X2y",
                "X2",
                ticks = F,
                min = -1,
                max = 1,
                value = 0,
                step = .1
              )
            )),

      #apply settings
          fluidRow(column(
              12, align = "right",
              actionButton("applySim4", "Preview Data")
            ))
          ),
          mainPanel(plotlyOutput("sim4plot", height = "700px"))

        )
      ),
  ###FEV
      tabPanel("FEV", value = "FEV",
               sidebarLayout(
                 sidebarPanel(
                   style = "height: 700px;",
                   fluidRow(
                     strong("Forced Expiratory Volume"),
                     icon("question-sign", lib = "glyphicon",
                          id = ns("fev.desc"))
                   ),
                   fluidRow(column(
                     12, align = "right",
                     actionButton("applyFEV", "Preview Data")
                   ))
                 ),

                 mainPanel(plotlyOutput("FEVplot", height = "700px"))
               )),

    ),
    actionButton(
      "usedata",
      "Please generate some data.",
      width = "100%",
      style = "font-weight: bold"
    )

  )#tabset panel
)#fluidpage

}



#' SimData Server Functions
#'
#' @noRd
mod_SimData_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_SimData_ui("SimData_1")

## To be copied in the server
# mod_SimData_server("SimData_1")
