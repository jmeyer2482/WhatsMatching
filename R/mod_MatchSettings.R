#' MatchSettings UI Function
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

mod_MatchSettings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(shiny::markdown("Please select the covariates that you would like to match on
                        and the covariates you would like to use calculate the estimate of
                        the treatment effect. Other matching settings can also be adjusted
                        for two methods to compare below."),
      hr(),

##Popovers
      # bsPopover(
      #   id = ns("M.tf"),
      #   title = "<strong>Matching Covariates</strong>",
      #   content = "Select the covariates you would like to match on. At least one must be selected.",
      #   placement = "right",
      #   options = list(container = "body")
      # ),
        # bsPopover(
        #   id = ns("M.distance"),
        #   title = "<strong>Matching Distance</strong>",
        #   content = "Select the distance type you would like to match with. The options are 'Propensity Score' or 'Mahalanobis'.",
        #   placement = "right",
        #   options = list(container = "body")
        # ),
      # bsPopover(
      #   id = ns("M.ord"),
      #   title = "<strong>Matching Distance</strong>",
      #   content = "Select the order you would like to have the matches conducted in. Options are 'data', 'largest', 'smallest', 'random'.",
      #   placement = "right",
      #   options = list(container = "body")
      # ),
      # bsPopover(
      #   id = ns("M.rep"),
      #   title = "<strong>Use Replacement</strong>",
      #   content = "Select whether or not to use replacement with the matches.",
      #   placement = "right",
      #   options = list(container = "body")
      # ),
      # bsPopover(
      #   id = ns("M.outcome"),
      #   title = "<strong>Outcome Formula</strong>",
      #   content = "This is the forumula that is used to estimate the outcome using linear regression.",
      #   placement = "right",
      #   options = list(container = "body")
      # ),

      fluidRow(column(4, h4("Matching Method 1"), align="left", offset = 3),
               column(4, h4("Matching Method 2"), align="left")),

      fluidRow(
        column(3, strong("Matching Covariates"),
               shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("M.tf")),
                               title = "<b>Matching Covariates</b>",
                               content = "Select the covariates you would like to match on. At least one must be selected.",
                               placement = "right",
                               options = list(container = "body"))
               ),

        column(4, selectizeInput("treat.f1", NULL, c("X1", "X2"), c("X1","X2"), multiple = T)),
        column(4, selectizeInput("treat.f2", NULL, c("X1", "X2"), c("X1","X2"), multiple = T))),


      fluidRow(
               column(3, strong("Matching Distance"),
                      shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("M.distance")),
                                      title = "<b>Matching Distance</b>",
                                      content = 'Select the distance type you would like to match with. The options are "Propensity Score" or "Mahalanobis".',
                                      placement = "right",
                                      options = list(container = "body"))
               ),

               column(4, selectInput("Dist1", NULL,#"First Matching Distance",
                                     c("Mahalanobis", "Propensity Score"),
                                     "Mahalanobis")),
               column(4, selectInput("Dist2", NULL,#"Second Matching Distance",
                                     c("Mahalanobis", "Propensity Score"),
                                     "Propensity Score"))),

      fluidRow(column(3, strong("Matching Order"),
                      shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("M.order")),
                                      title = "<b>Matching Distance</b>",
                                      content = 'Select the order you would like to have the matches conducted in. Options are "data", "largest", "smallest", "random".',
                                      placement = "right",
                                      options = list(container = "body"))
                      ),
               column(4, selectInput("Ord1", NULL,#"First Matching Order",
                                     c("data", "largest", "smallest", "random"),
                                     "data")),
               column(4, selectInput("Ord2", NULL,#"Second Matching Order",
                                     c("data", "largest", "smallest", "random"),
                                     "data"))),

      fluidRow(column(3, strong("Use Replacement"),
                      shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("M.replace")),
                                      title = "<b>Use Replacement</b>",
                                      content = "Select whether or not to use replacement with the matches.",
                                      placement = "right",
                                      options = list(container = "body"))
                      ),
               column(4,  checkboxInput("Rep1", NULL)),
               column(4, checkboxInput("Rep2", NULL))),

      fluidRow(
        column(3, strong("Outcome Formula"),
               shinyBS::popify(icon("question-sign", lib = "glyphicon", inputId = ns("M.outcome")),
                               title = "<b>Outcome Formula</b>",
                               content = "This is the forumula that is used to estimate the outcome using linear regression.",
                               placement = "right",
                               options = list(container = "body"))
               ),
        column(4, selectizeInput("outcome.f", NULL,
                                 c("X1", "X2"), multiple=T)),
        column(4, strong("Formula: "), htmlOutput("y.formula", inline = T))
        ),

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
