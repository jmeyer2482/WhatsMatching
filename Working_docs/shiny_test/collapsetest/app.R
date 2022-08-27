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
library(shinipsum)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # sidebarLayout(
        # sidebarPanel(HTML("This button will open Panel 1 using updateCollapse."),
        #              actionButton("p1Button", "Push Me!"),
        #              selectInput("styleSelect", "Select style for Panel 1",
        #                          c("default", "primary", "danger", "warning", "info", "success"))
        # ),
        # mainPanel(
    h1("The Propensity Score Matching Paradox"),br(),
    selectInput("pick.data",
                "What data would you like to use?",
                c("Real data"="real",
                  "King and Neilsen Simulations"="KN",
                  "Custom Simulation"="sim", "None"="hide"),
                ""), br(),
    # conditionalPanel(condition = "subdata.show",
    selectInput("subpick.data",
                "Select a data set to use?",
                ""#)
    ),
    bsCollapse(id = "collapse1",
        bsCollapsePanel("Check out the raw data",
                        "This is a panel with just text ",
                        "and has the default style. You can change the style in ",
                        "the sidebar.",
                        plotOutput(shinipsum::random_ggplot("point")),
                        style = "info")),
    bsCollapse(id = "collapse2",
               bsCollapsePanel("Check out how the data is matched",
                   sidebarLayout(
                    sidebarPanel(
                        HTML("This button will open Panel 1 using updateCollapse."),
                        actionButton("p1Button", "Push Me!"),
                        selectInput("styleSelect", "Select style for Panel 1",
                                    c("default", "primary", "danger",
                                      "warning", "info", "success"))
                   ),
                   mainPanel(plotOutput("matchdataPlot"))),
                               style = "success")
    ))
# )
    # )
# )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$rawdataPlot <- renderPlot(shinipsum::random_ggplot("point"))

    output$matchdataPlot <- renderPlot(shinipsum::random_ggplot("point"))

    observeEvent(input$pick.data, ({
        x <- switch(input$pick.data,
               real = c("Lalonde"="lalonde","Forced Expiratory Volume"="fev"),
               KN = c("Simulation 1"="sim1", "Simulation 2"="sim2"),
               hide = 1
               )

        if (length(x) > 1) {
        updateSelectInput(session, "subpick.data",
                          choices = x)
            } else {

                if (x==1) output$subdata.show() <- 1 else output$subdata.show() <- 2
            }
    }))

    # output$show <- reactive({
    #     input$num == 2 # Add whatever condition you want here. Must return TRUE or FALSE
    # })

    # outputOptions(output, 'show', suspendWhenHidden = FALSE)


    observeEvent(input$p1Button, ({
        updateCollapse(session, "collapseExample", open = "Panel 1")
    }))

    observeEvent(input$styleSelect, ({
        updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
    }))
}


# Run the application
shinyApp(ui = ui, server = server)
