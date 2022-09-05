#' Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Info_ui <- function(id){
  ns <- NS(id)
  tagList(

  fluidPage(
    "Not sure if having buttons or tab panels"
  )

  )
}

#' Info Server Functions
#'
#' @noRd
mod_Info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Info_ui("Info_1")

## To be copied in the server
# mod_Info_server("Info_1")
