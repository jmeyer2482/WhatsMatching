#' MatchSettings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MatchSettings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(markdown("Please select the covariates that you would like to match on
                        and the covariates you would like to use calculate the estimate of
                        the treatment effect. Other matching settings can also be adjusted
                        for two methods to compare below."), br(),

              fluidRow(column(6, strong("Select the covariates to match on"), br(),
                              selectizeInput("treat.f", span("Formula: ",htmlOutput("t.formula", inline = T)),
                                  c("X1", "X2"), c("X1","X2"), multiple = T)),
                       column(6, strong("Select covariates to calculate the estimates"), br(),
                              selectizeInput("outcome.f", span("Formula: ",htmlOutput("y.formula", inline = T)),
                                                c("X1", "X2"), multiple=T))),
              hr(),

              fluidRow(column(4, h4("Matching Method 1"), offset = 4), column(4, h4("Matching Method 2"))),

              fluidRow(
                       column(3, strong("Matching Distance"),
                       icon("question-sign", lib = "glyphicon", id = ns("distance")),
                       bsPopover(
                         id = ns("distance"),
                         title = "<strong>Matching Distance</strong>",
                         content = "Select the distance type you would like to match with. The options are 'Propensity Score' or 'Mahalanobis'.",
                         placement = "right",
                         options = list(container = "body")
                       )),

                       column(4, selectInput("Dist1", NULL,#"First Matching Distance",
                                             c("Mahalanobis", "Propensity Score"),
                                             "Mahalanobis")),
                       column(4, selectInput("Dist2", NULL,#"Second Matching Distance",
                                             c("Mahalanobis", "Propensity Score"),
                                             "Propensity Score"))),

              fluidRow(column(3, strong("Matching Order"),
                              icon("question-sign", lib = "glyphicon", id = ns("ord")),
                              bsPopover(
                                id = ns("ord"),
                                title = "<strong>Matching Distance</strong>",
                                content = "Select the order you would like to have the matches conducted in. Options are 'data', 'largest', 'smallest', 'random'.",
                                placement = "right",
                                options = list(container = "body")
                              )),
                       column(4, selectInput("Ord1", NULL,#"First Matching Order",
                                             c("data", "largest", "smallest", "random"),
                                             "data")),
                       column(4, selectInput("Ord2", NULL,#"Second Matching Order",
                                             c("data", "largest", "smallest", "random"),
                                             "data"))),

              fluidRow(column(3, strong("Use Replacement"),
                              icon("question-sign", lib = "glyphicon", id = ns("rep")),
                              bsPopover(
                                id = ns("rep"),
                                title = "<strong>Use Replacement</strong>",
                                content = "Select whether or not to use replacement with the matches.",
                                placement = "right",
                                options = list(container = "body")
                              )),
                       column(4,  checkboxInput("Rep1", NULL)),
                       column(4, checkboxInput("Rep2", NULL))),

              # fluidRow(
              #   selectizeInput("outcome.f", htmlOutput("y.formula"),
              #             c("X1", "X2"), #y ~ t", "y ~ t + X1 + X2", "y ~ t + X1", "y ~ t + X2"),
              #             multiple=T)),
              br(),

              actionButton("usematching", "Use these matching settings",
                           width = "100%", style="font-weight: bold")
    )
  )
}

#' MatchSettings Server Functions
#'
#' @noRd
mod_MatchSettings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_MatchSettings_ui("MatchSettings_1")

## To be copied in the server
# mod_MatchSettings_server("MatchSettings_1")
