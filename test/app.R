#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)

ui <- shinyUI(pageWithSidebar(
  headerPanel("eventReactive and observeEvent"),
  sidebarPanel(
    actionButton("evReactiveButton", "eventReactive"),
    br(),
    actionButton("obsEventButton", "observeEvent"),
    br(),
    actionButton("evReactiveButton2", "eventReactive2")
  ),
  mainPanel(
    verbatimTextOutput("eText"),
    verbatimTextOutput("oText")
  )
))

server <- shinyServer(function(input, output) {
  etext <- eventReactive(input$evReactiveButton, {
    runif(1)
  })
  observeEvent(input$obsEventButton,{
    output$oText <- renderText({ runif(1) })
  })
  eventReactive(input$evReactiveButton2,{
    print("Will not print")
    output$oText <- renderText({ runif(1) })
  })
  output$eText <- renderText({
    etext()
  })
})

shinyApp(ui=ui,server=server)
