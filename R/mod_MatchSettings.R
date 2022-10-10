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
    fluidPage(markdown("For the purposes of this simulation, all data is simulated
                        using the variables `t`, `X1`, `X2`, and `y`. The formula
                        the matching is conducted with is `t ~ X1 + X2`."), br(),
              selectizeInput("treat.f", "Formula for matching treated and control units",
                          c("t ~ X1 + X2", "t ~ X1", "t ~ X2"),
                          "t ~ X1 + X2"), br(),
              fluidRow(column(6, "Matching Method 1"), column(6, "Matching Method 2"),

                       column(6, selectInput("Dist1", "First Matching Distance",
                                             c("Mahalanobis", "Propensity Score"),
                                             "Mahalanobis")),
                       column(6, selectInput("Dist2", "Second Matching Distance",
                                             c("Mahalanobis", "Propensity Score"),
                                             "Propensity Score")),

                       column(6, selectInput("Ord1", "First Matching Order",
                                             c("data", "largest", "smallest", "random"),
                                             "data")),
                       column(6, selectInput("Ord2", "Second Matching Order",
                                             c("data", "largest", "smallest", "random"),
                                             "data")),

                       column(6, checkboxInput("Rep1", "Use Replacement")),
                       column(6, checkboxInput("Rep2", "Use Replacement"))
              ),
              selectizeInput("outcome.f", "Formula for calculating estimates",
                          c("y ~ t", "y ~ t + X1 + X2", "y ~ t + X1", "y ~ t + X2"),
                          "y ~  t"),
              actionButton("usematching", "Use these matching settings",
                           width = "100%", style="font-weight: bold"),
              textOutput("test")
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
