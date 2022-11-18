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

te.txt <- HTML("The treatment effect controls the effect that the treatment will have on the outcome (<code>y</code>).")

rho.txt <- HTML(
  paste0("The covariate correlation is used to determine the strength of the correlation between ", X1.t, " and ", X2.t, ". Select 0 for no correlation or a positive or negative value for a positive or negative correlation between ", X1.t, " and ", X2.t, "."))

xt.txt <- HTML(
    paste0("The effect on treatment controls the degree to which ", X1.t, " and ", X2.t, " change the chance of being treated. When set to a value other than 0, it can be used to replicate a confounding variable."))

xy.txt <- HTML(
  paste0("The effect on outcome controls the degree to which ", X1.t, " and ", X2.t, " change the outcome. When set to 0 there is no effect on the outcome."))

y.txt <-
  HTML(
    paste0("This is the value of the outcome before it has been subjected to the effects of ", X1.t, ", ", X2.t, ", or ", t.t, "."))

range.txt <- HTML(
  paste0("This will set the range of a uniform distribution for both ", X1.t, " and ", X2.t, "."))

overlap.txt <- HTML(
  paste0("Use these sliders to adjust the effect that treatment has on ", X1.t, " and ", X2.t, ". The seperation between the treated and untreated groups will increases the further this is from zero (in either direction)."))

rel.txt <- HTML(
  paste0("These two variable control how ", X1.t, " and ", X2.t, " are related to ", t.t, " and ", y.t, ". The direction of these relationsips is outlined below.",
         br(), br(),
         "Confounder: ", t.t, " &#129120; variable &#129122; ", y.t, br(),
         "Mediator: ", t.t, " &#129122; variable &#129120; ", y.t, br(),
         "Collider: ", t.t, " &#129122; variable &#129120; ", y.t, br(),
         "Ancenstor of t: variable &#129122; ", t.t, br(),
         "Ancestor of y: variable &#129122; ", y.t))


mod_SimData_ui <- function(id) {



  ns <- NS(id)
  tagList(fluidPage(
    h3("Data Generation", style="margin-top: 0.2em;"),
    HTML("There are 2 options for data generation - simulated data, or the forced expiratory volume (FEV) data set. All of the data simulations contain the same variables - <code>t</code>: a binary treatment, <code>X1</code> and <code>X2</code>: covariates, and <code>y</code>: a continuous outcome. Each simulation has it's own particular settings to change and can be better at demonstrating different matching effects."), br(), br(),

    tabsetPanel(
      #type = "pills",

      ###Simulation 1
      tabPanel(
        "Simulation 1",
        value = "Sim1",
        sidebarLayout(
          sidebarPanel(
            style = "height: 700px;",
            HTML("This simulation is based on one done by King and Neilsen in their 2019 paper on the propensity score paradox. It contains three groups; one representing a randomised experiment, one representing a random experiment, and one representing a control group. This simulation highlights the difference in matches between the mahalanobis distance and the propensity score."), br(), br(),

            fluidRow(
              strong("Treatment Effect"),
              shinyBS::popify(icon(name="question-sign", lib = "glyphicon", inputId = ns("S.te1")),
                title = "<b>Treatment Effect on Outcome</b>",
                content = te.txt,"right",options = list(container = "body"))
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
            HTML("Like Simulation 1, this simulation is also based on one done by King and Neilsen. It generates two groups, treated and control, that overlap on the uniformly distributed covarties <code>X1</code> and <code>X2</code>. You can adjust the amount of overlap and range of the data. This simulations demonstrates the effect of replacement well."), br(), br(),

          fluidRow(
            strong("Range of Uniform Distribution"),

            shinyBS::popify(icon(name="question-sign", lib = "glyphicon", inputId = ns("S.range")),
                            title = "<b>Range of Uniform Distribution</b>",
                            content = range.txt,
                            "right", options = list(container = "body"))
          ),

            fluidRow(column(
              12,
              sliderInput(
                "sim2.X1.val",
                NULL,
                min = -10,
                max = 10,
                value = c(0, 5),
                step = 1,
                ticks = F
              )
            )),

          fluidRow(
            shinyBS::bsPopover(
              id = ns("S.overlap"),
              title = "<b>Treatment Effect on Covariates</b>",
              content = overlap.txt,
              "right",
              options = list(container = "body")
            ),
              strong("Effect on Covariates"),
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.overlap")),
                              title = "<b>Treatment Effect on Covariates</b>",
                              content = overlap.txt,
                              "right",
                              options = list(container = "body")
                              )
            ),

            fluidRow(column(
              6,
              sliderInput(
                "sim2.x.overlap",
                "<b>X1</b>",
                min = -5,
                max = 5,
                value = 1,
                step = 1,
                ticks = F
              )
            ),
            column(
              6,
              sliderInput(
                "sim2.y.overlap",
                "<b>X2</b>",
                min = -5,
                max = 5,
                value = 1,
                step = 1,
                ticks = F
              )
            )),

            fluidRow(
              strong("Treatment Effect on Outcome"),
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.te2")),
                              title = "<b>Treatment Effect on Outcome</b>",
                              content = te.txt,
                              "right",
                              options = list(container = "body")
                              )
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
            HTML("This idea behind this simulation is to generate data based on causal relationships. In this simulation both <code>X1</code> and <code>X2</code> are drawn from a bivariate normal distribution. This simulation is good for investigating the effect of model selection for both the outcome and the matching covariates."),
            br(), br(),

            fluidRow(
              strong("Relationships"),
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.rel")),
                              title = "<b>Causal Relationship</b>",
                              content = rel.txt ,
                              "right",
                              options = list(container = "body"))
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
            fluidRow(
              strong("Treatment Effect"),
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.te3")),
                              title = "<b>Treatment Effect on Outcome</b>",
                              content = te.txt ,
                              "right",
                              options = list(container = "body")
                              )
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
            HTML("This simulation allow you to control the level of confounding on <code>X1</code> and/or <code>X2</code>. Like Simulation 3, both <code>X1</code> and <code>X2</code> are drawn from a bivariate normal distribution. This simulation is also good for investigating the effect of model selection for both the outcome and the matching covariates."), br(), br(),

        #covariate correlation
            fluidRow(
              strong("Covariate Correlation"),
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.rho")),
                              title = "<b>Covariate Correlation</b>",
                              content = rho.txt ,
                              "right",
                              options = list(container = "body")
                              )
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
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.xt")),
                              title = "<b>Covariate Effect on Treatment</b>",
                              content = xt.txt,
                              "right",
                              options = list(container = "body")
                              )
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
              shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.xy")),
                              title = "<b>Covariate Effect on Outcome</b>",
                              content = xy.txt,
                              "right",
                              options = list(container = "body")
                              )
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

      #Treatment effect on y
      fluidRow(
        strong("Treatment Effect"),
        shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("S.te4")),
                        title = "<b>Treatment Effect</b>",
                        content = te.txt ,
                        "right",
                        options = list(container = "body")
                        )
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
                   HTML("This data set consists of 654 observations on youths aged 3 to 19 from East Boston recorded duing the middle to late 1970's. Forced expiratory volume (<code>fev</code>), a measure of lung capacity, is the variable of interest. <code>age</code> and <code>height</code> are two continuous predictors. <code>sex</code> and <code>smoke</code> are two categorical predictors. For the purpose of this exercise, <code>smoke</code> is being considered the treatment variable."),
                   br(), br(),

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
