#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyBS bsPopover bsModal
#' @importFrom htmltools HTML
#' @importFrom shinythemes shinytheme
#'
#' @noRd


te.txt <- HTML(paste0(strong("Mean (average)"), br(),
                        "Select a value to set the mean value for the",
                        "normally distributed variables X1 and X2."))


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic

#set up fluidpage
    fluidPage(
      theme = shinytheme("spacelab"), br(),

      #Settings
      bsPopover(id="butGetData",title=HTML(paste(strong("Data and Match Settings"))),
                content="Change the data and match settings that have informed the plot",
                "right",options = list(container = "body")),
      bsModal(id="modGetData", title="Data and Match Settings", trigger="butGetData",
              size = "large", mod_SimData_ui("SimData_1"),
              tags$head(tags$style("#modGetData .modal-footer{ display:none}"))),
      bsModal(id="modMatchSettings", title="Update Matching Settings",
              trigger="butMatchSettings", size = "large",
              mod_MatchSettings_ui("MatchSettings_1"),
              tags$head(tags$style("#modMatchSettings .modal-footer{ display:none}"))
      ),

      #View data
      bsPopover(id="butViewData",title=HTML(paste(strong("View Data"))),
                content="View the data and match settings that have informed the plot",
                "right",options = list(container = "body")),
      bsModal(id="modViewData", title="View Data", trigger = "butViewData",
              size = "large", mod_ViewData_ui("ViewData_1"),
              tags$head(tags$style("#modViewData .modal-footer{ display:none}"))
      ),

      #Random matches
      bsPopover(id="butRandom",title=HTML(paste(strong("Random Matching"))),
                content="Generate a random simulated data set for matching. Warning: will overwrite whatever is currently loaded",
                "right",options = list(container = "body")
      ),

      #Info
      bsPopover(id="butInfo",title=HTML(paste(strong("App Information"))),
                content="See all the information about the app and the research that underpins it.",
                "right",options = list(container = "body")),
      bsModal(id="modInfo", title="App Information", trigger = "butInfo",
              size = "large", mod_Info_ui("Info_1")),

  shiny::sidebarLayout(position="right",

    shiny::sidebarPanel(width = 3,
        h4("Treatment Effect Settings"),#br(),
        "Formula: ", code(textOutput("txt.M.o.f", inline = T), .noWS="outside"), br(),
        "Treatment Effect: ", textOutput("txt.M.TE", inline = T), br(),
        h4("Match settings"),
        tableOutput("m.info"),
        includeMarkdown("inst/app/www/main_side.html")
    ),
  #matching plot panel
      mainPanel(width = 9, align = "center",

  #title
    fluidRow(
      align = "center",
      h1("What's Matching? An Exploratory Shiny App")
    ),

  #main page
    fluidRow(
       column(4, offset = 4,
          align = "center",
          #Settings
          actionButton("butGetData",label=NULL,icon=icon("cog", lib="glyphicon"), width = "20%"),

          #View data
          actionButton("butViewData",label=NULL, icon=icon("table"), width = "20%"),

          #Random matches
          actionButton("butRandom",label=NULL, icon=icon("shuffle"), width = "20%"),

          #Info
          actionButton("butInfo",label=NULL, icon=icon("info"), width = "20%"))
    ),



        plotlyOutput("distPlot", width = "100%", height = "850px")
      )

    )#close sidebarpanel



    )#close fluidpage

  ) #close taglist
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "WhatsMatching"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
