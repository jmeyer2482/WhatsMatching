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
      span(
        h4("Overview"),#br(),
        "The purpose of these plots is to provide insights into how ",
        "matching methods work in practice. Generally, matching is used to assist ",
        "in controlling confounding and bias in observational data where the treatment ",
        "is a binary variable prior to estimating the treatment effects.", br(),
        "Simulated data containing a treatment variable (", code("t", .noWS="outside"),
        "), covariates (", code("X1", .noWS="outside"),"and", code("X2", .noWS="outside"), ") and an outcome varible (",
        code("y", .noWS="outside"), ") have been used to generate and compare matching methods. ",
        "The data in all of the simulatons are matched on the same formula - ",
        code(textOutput("txt.M.t.f", inline = T), .noWS="outside"), br(),
        br(),
        h4("Plot 1 and Plot 2"),#br(),
        "Plot 1 and 2 show the underlying data the matches were performed on. ",
        "A frame by frame progression of how the matches occurred for two ",
        "different methods are shown. ",
        "Blue trianges represent the treated group, red triangles the control group. ",
        "When matching occurs, there is a control unit paired to every treated unit.", br(),
        # br(),
        # strong("Plot 2 - Method 2"),br(),
        # "Plot 2 is the same as Plot 1 but for Method 2. ",br(),
        br(),
        h4("Plot 3 - Standardised Mean Difference (SMD)"),#br(),
        "Plot 3 shows the SMD between each of the covariates for the raw values ",
        "and each of the matching methods. Each frame shows the changes in the SMD ",
        "for the matched data in Plot 1 and Plot 2. The raw SMD does not change as ",
        "the underlying data does not change.",br(),
        br(),
        h4("Plot 4 - Estimates of the Treatment Effect"),#br(),
        "Plot 4 shows the estimates of the treatment effect for various propensity score ",
        "methods, including stratification and weighting. The cumulative effect of the estimates ",
        "as units are added to the data is also visible in relation to the matches seen in Plot 1 ",
        "and 2. The formula and the treatment ",
        "effect used to simulate the outcome is the same for all methods.", br(),
        br(),
        h4("Estimate Settings"),#br(),
        "Outcome Formula: ", code(textOutput("txt.M.o.f", inline = T), .noWS="outside"), br(),
        "Treatment Effect: ", textOutput("txt.M.TE", inline = T), br(),
        br(),
        h4("Match settings"),#br(),
        # "Method 1 Distance: ", textOutput("txt.M1.dist",inline=T),br(),
        # "Method 1 Order: ", textOutput("txt.M1.ord",inline=T),br(),
        # "Method 1 Use Replacement: ", textOutput("txt.M1.rep",inline=T),br(),
        # "Method 2 Distance: ", textOutput("txt.M2.dist",inline=T),br(),
        # "Method 2 Order: ", textOutput("txt.M2.ord",inline=T),br(),
        # "Method 2 Use Replacement: ", textOutput("txt.M2.rep",inline=T)
        tableOutput("m.info")
        )


    ),
  #matching plot panel
      mainPanel(width = 9, align = "center",

  #title
    fluidRow(
      align = "center",
      h1("The Propensity Score Paradox")
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
      app_title = "PSM.golex"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
