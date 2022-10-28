#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import markdown
#' @rawNamespace import(plotly, except = last_plot)
#' @import shinyBS
#' @importFrom stats as.formula
#' @importFrom htmltools HTML
#' @importFrom DT JS renderDT
#' @importFrom utils data
#' @importFrom stringi stri_locate_first_fixed
#' @import mplot
#'
#' @noRd
#'
#'
#'
#'

library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(shinyBS)
library(htmltools)
library(shinythemes)
library(DT)
library(markdown)
# library(mplot)


#function for getting fev dataset
load.fev <- function() {
  requireNamespace("mplot")

  #create environment for loading, otherwise loads into global
  e <- new.env()

  #get data
  data(list = "fev",
       package = "mplot",
       envir = e)

  #store data
  d <- e$fev

  #return data
  return(d)
}

#function for general data plots
plot.data <- function(d) {
  a.set <-
    list(title = list(
      standoff = 2,
      autorange = TRUE,
      tickmode = "auto"
    ))

  control.col <-'#3772ff'
  treat.col <- '#df2935'

  p1 <-
    ggplot(d, aes(.data$X1, .data$X2, colour = .data$Allocation)) +
    geom_point(alpha = 0.5) + theme(legend.position = "none") +
    # guides(colour = "Group") +
    scale_color_manual(values=c(control.col, treat.col), name="Group")
  p1 <- ggplotly(p1) %>% layout(xaxis = a.set, yaxis = a.set)

  p2 <-
    ggplot(d, aes(.data$Allocation, .data$y, fill = .data$Allocation)) +
    geom_boxplot(alpha=0.5) + xlab("") + theme(legend.position = "none") +
    # guides(colour = "Group") +
    scale_fill_manual(values=c(control.col, treat.col), name="Group")
  p2 <- ggplotly(p2) %>% layout(xaxis = a.set, yaxis = a.set)

  p3 <- ggplot(d, aes(.data$X1, .data$y, colour = .data$Allocation)) +
    geom_point(alpha = 0.5) + theme(legend.position = "none") +
    # guides(colour = "Group") +
    scale_color_manual(values=c(control.col, treat.col), name="Group")
  p3 <- ggplotly(p3) %>% layout(xaxis = a.set, yaxis = a.set)

  p4 <- ggplot(d, aes(.data$X2, .data$y, colour = .data$Allocation)) +
    geom_point(alpha = 0.5) + theme(legend.position = "none") +
    # guides(colour = "Group") +
    scale_color_manual(values=c(control.col, treat.col), name="Group")
  p4 <- ggplotly(p4) %>% layout(xaxis = a.set, yaxis = a.set)

  return(
    plotly::subplot(
      p1,
      p2,
      p3,
      p4,
      nrows = 2,
      titleX = T,
      titleY = T,
      margin = 0.05
    ) %>% config(displayModeBar = F)
  )
}


random.data <- function(TE = round(runif(1,-10, 10), 1)) {
  rand.data <- sample(1:3, 1)

  d <- create.sim.data(sim = rand.data, te = TE)

  return(d)

}


match.detail <- function(M, M.num, outcome.f, treat.f) {

  list(
    paste("Method", M.num),
    M$distance,
    M$order,
    M$replace,
    deparse1(outcome.f),
    deparse1(treat.f)
  )


}


random.msg <- function() {
  n <- round(runif(1, 0.51, 10.49), 0)

  switch(
    n,
    "1" = paste(
      "In 2019, King and Neilsen identified a paradox when using",
      "the propensity score for matching and demonstrated that",
      "extensive matching on the propensity score can increase",
      "bias despite improving covariate balance."
    ),
    "2" = paste(
      "The propensity score can be used in multiple way to achieve",
      "this, notably matching, weighting and stratification (aka",
      "subclassification)"
    ),
    "3" = paste(
      "Matching requires that a type of distance between units",
      "is calculated. This distance is then used to pair or match",
      "units that are close together while discarding or pruning",
      "those that are not."
    ),
    "4" = paste(
      "Weighting with the propensity score differs from matching",
      "as no units are pruned from the dataset. This is an advantage",
      "over matching in the sense that all data can remain in the",
      "analysis, giving the opportunity to calculate the average",
      "treatment effect on all units, as opposed to only the treated ones."
    ),
    "5" = paste(
      "Stratification relies on the use of the propensity score to group",
      "units into quantiles. Once the number of groups has been",
      "established, usually between 5 and 10, the treatment",
      "effect is estimated across the groups and then averaged to",
      "give a mean difference average treatment effect."
    ),
    "6" = paste(
      "Rosenbaum and Rubin (1983) defined the propensity score as",
      "the 'conditional probability of assignment to a particular",
      "treatment given a vector of observed covariates'. It is one",
      "of the most widely used statistical methods when analysing",
      "observational data."
    ),
    "7" = paste(
      "In most cases, the propensity score is estimated from",
      "available data using a simple logistic regression on a binary",
      "treatment assignment indicator."
    ),
    "8" = paste(
      "The propensity score is the probability of receiving treatment which",
      "reduces a multidimensional space to a single dimension for easy",
      "comparison between treated and untreated units."
    ),
    "9" = paste("Test 9"),
    "10" = paste("Test 10")
  )
}


##SERVER

app_server <- function(input, output, session) {
  # Your application server logic

  #reactive value for storing variables
  values <- rv()

  #variable settings for intialisation
  values$TE <- 2
  values$outcome.f <- y ~ t
  values$M1.dist <- "Mahalanobis"
  values$M1.ord <- sample(x = c("data", "smallest", "largest", "random"),
                          size = 1)
  values$M1.rep <- sample(x = c(F, T), 1)
  values$M2.dist <- "Propensity Score"
  values$M2.ord <- sample(
                     x = c("data", "smallest", "largest", "random"),
                     size = 1)
  values$M2.rep <- sample(x = c(F, T), 1)
  values$selected.d <- NULL#random.data(2)
  values$outcome.f <-
    sample(c(y ~ t, y ~ t + X1, y ~ t + X2, y ~ t + X1 + X2), 1)[[1]]
  values$treat.f <- t ~ X1 + X2
  values$d <- NULL
  values$d.set <- "sim"
  values$selected.p <- NULL
  values$jitter <- 1


  #open the settings dialogue
  observe({
    #if there's no stored data then update the button description
    if (is.null(values$selected.d)) {
      updateActionButton(session, "usedata",
                         label = "Please generate some data")
    } else {
      updateActionButton(session, "usedata",
                         label = "Continue without changing data")
    }

  }) %>%
    bindEvent(input$butGetData)

  #generate random data and matches
  observe({

    #randomly select distance and order for options available
    dist <- sample(
      x = c("Mahalanobis", "Propensity Score"),
      size = 2,
      replace = T
    )
    ord <- sample(
      x = c("data", "smallest", "largest", "random"),
      size = 2,
      replace = T
    )

    #if the distance and the order are the same then
    #make sure one is with and one without replacement
    #ensures methods will always be different
    if (dist[1] == dist[2] & ord[1] == ord[2]) {
      rep <- sample(x = c(F, T), 2, replace = T)
    } else {
      rep <- sample(x = c(F, T), 2, replace = F)
    }

    #assign matching variables to session for use in another function
    values$outcome.f <<-
      sample(c(y ~ t, y ~ t + X1, y ~ t + X2, y ~ t + X1 + X2), 1)[[1]]
    values$treat.f <<- t ~ X1 + X2

    values$M1.dist <<- dist[[1]]
    values$M1.ord <<- ord[[1]]
    values$M1.rep <<- rep[[1]]

    values$M2.dist <<- dist[[2]]
    values$M2.ord <<- ord[[2]]
    values$M2.rep <<- rep[[2]]

    #random treatment effect
    values$TE <<- round(runif(1,-2, 2), 1)

    #random data based on treatment effect
    d <- random.data(values$TE)
    values$selected.d <<- d
    values$selected.p <<- plot.data(d)


  }) %>%
    bindEvent(input$butRandom)

  #load matching plots
  #triggered by a change to the stored data
  observe({

    #requires the stored data (eg cannot be NULL)
    req(values$selected.d)

    #display loading message while calculations occurring.
    #is turned off at the end of end of the process
    #ensures nothing else happens while the data is loading.
    showModal(modalDialog(
      title = "Loading",
      easyClose = F,
      random.msg(),
      footer = tagList()
    ))

    #assign from reactivevalues to ensure values are static
    o.f <- stats::as.formula(values$outcome.f)
    d <- values$selected.d
    TE <- values$TE
    t.f <- stats::as.formula(values$treat.f)
    D1 <- values$M1.dist
    O1 <- values$M1.ord
    R1 <- values$M1.rep
    D2 <- values$M2.dist
    O2 <- values$M2.ord
    R2 <- values$M2.rep

    if(is.null(values$selected.p)) values$selected.p <<- plot.data(d)

    #get matched data for 2 methods
    M1 <- matched.data(t.f, d, D1, O1, R1)
    M2 <- matched.data(t.f, d, D2, O2, R2)

    #store the matched data
    values$M1 <<- M1
    values$M2 <<- M2

    #check the treatment variable from the formula
    #assign plots accordingly
    if (all.vars(t.f)[1] == "smoke") {
      cp <- combined.plot("age", "height", M1, M2, te = TE, o.f)
    } else {
      cp <- combined.plot("X1", "X2", M1, M2, te = TE, o.f)
    }

    #outputs for sidebar on main page
    output$txt.M.TE <- renderText(TE)
    output$txt.M.o.f <- renderText(deparse1(o.f))
    output$txt.M.t.f <- renderText(deparse1(t.f))

    #update options so settings reflect current outputs
    updateSelectizeInput(session, "treat.f", selected = deparse(t.f))
    updateSelectInput(session, "Dist1", selected = D1)
    updateSelectInput(session, "Dist2", selected = D2)
    updateSelectInput(session, "Ord1", selected = O1)
    updateSelectInput(session, "Ord2", selected = O2)
    updateCheckboxInput(session, "Rep1", value = R1)
    updateCheckboxInput(session, "Rep2", value = R2)
    updateSelectizeInput(session, "outcome.f", selected = deparse(o.f))

    #create a table of information about the plot outputs
    m.info <- list(
      Distance = c(D1, D2),
      Order = c(O1, O2),
      Replace = c(ifelse(R1, "Yes", "No"), ifelse(R2, "Yes", "No"))
    ) %>%
      as.data.frame()

    #update the columns for output
    rownames(m.info) <- c("Plot 1", "Plot 2")

    #output table with matching info
    output$m.info <- shiny::renderTable(
      t(m.info),
      rownames = T,
      bordered = TRUE,
      hover = TRUE
    )

    #output the combined plots
    output$distPlot <- plotly::renderPlotly(cp)

    #close the modal now that everything is finished
    #sleep for 5 extra seconds so plotly object has time to load
    removeModal()

  }) %>%
    bindEvent(values$selected.d,
              ignoreInit = F,
              ignoreNULL = T)


  #create plot for simulation 1
  #also store data in values
  #and update the label for the usedata button
  plot1.data <- reactive({
    TE <- input$sim1.TE
    d <- create.sim.data(sim = 1, te = TE,
                         jitter=values$jitter)

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 1")

    # updateSelectInput(session,
    #                   "outcome.f",
    #                   choices = c("y ~ t", "y ~ t + X1 + X2", "y ~ t + X1", "y ~ t + X2"))
    #
    # values$d.set <<- "sim"

    p <- plot.data(d)

    values$data.plots <<- p

    p

  }) %>%
    bindEvent(input$applySim1)

  output$sim1plot <- plotly::renderPlotly(plot1.data())




  observe({
    Val <- max(input$sim2.X1.val) - min(input$sim2.X1.val)

    updateSliderInput(session, "sim2.x.overlap", min = 0, max = Val)
    updateSliderInput(session, "sim2.y.overlap", min = 0, max = Val)

  }) %>%
    bindEvent(input$sim2.X1.val)

  #create plot for simulation 2
  #also store data in values
  #and update the label for the usedata button
  plot2.data <- reactive({
    TE <- input$sim2.TE
    val.min <- min(input$sim2.X1.val)
    val.max <- max(input$sim2.X1.val)
    diff <- val.max - val.min
    overlap.X1 <- diff - input$sim2.x.overlap
    overlap.X2 <- diff - input$sim2.y.overlap

    d <- create.sim.data(
      sim = 2,
      te = TE,
      g1_min = val.min,
      g1_max = val.max,
      g2_shift_X1 = overlap.X1,
      g2_shift_X2 = overlap.X2,
      jitter=values$jitter
    )

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 2")

    # updateSelectInput(session,
    #                   "outcome.f",
    #                   choices = c("y ~ t", "y ~ t + X1 + X2", "y ~ t + X1", "y ~ t + X2"))

    # values$d.set <<- "sim"

    p <- plot.data(d)

    values$data.plots <<- p

    p

  }) %>%
    bindEvent(input$applySim2)

  output$sim2plot <- plotly::renderPlotly(plot2.data())


  plot3.data <- reactive({
    TE <- input$sim3.TE
    X1r <- input$sim3.X1rel
    X2r <- input$sim3.X2rel

    d <- create.sim.data(sim = 3, te = TE, relX1=X1r, relX2=X2r,
                         jitter=values$jitter)

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 3")

    p <- plot.data(d)

    values$data.plots <<- p

    p

  }) %>%
    bindEvent(input$applySim3)

  output$sim3plot <- plotly::renderPlotly(plot3.data())


  plot4.data <- reactive({
    TE <- input$sim4.TE
    rho <- input$sim4.rho
    y <- 2

    # X1m <- 0
    # X2m <- 0
    X1sd <- input$sim4.X1sd
    X2sd <- input$sim4.X2sd
    X1t <- input$sim4.X1t
    X2t <- input$sim4.X2t
    X1y <- input$sim4.X1y
    X2y <- input$sim4.X2y

    d <- create.sim.data(
      sim = 4,
      te = TE,
      rho = rho,
      weight_y0 = y,
      # mean1 = X1m,
      # mean2 = X2m,
      sd1 = X1sd,
      sd2 = X2sd,
      weight_t1 = X1t,
      weight_t2 = X2t,
      weight_y1 = X1y,
      weight_y2 = X2y,
      jitter=values$jitter
    )

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 4")

    p <- plot.data(d)

    values$data.plots <<- p

    p

  }) %>%
    bindEvent(input$applySim4)

  output$sim4plot <- plotly::renderPlotly(plot4.data())


  plotFEV.data <- reactive({
    d <- load.fev()

    values$d <<- d
    values$TE <<- NULL

    a.set <-
      list(title = list(
        standoff = 2,
        autorange = TRUE,
        tickmode = "auto"
      ))

    control.col <-'#3772ff'
    treat.col <- '#df2935'

    d$Allocation <- ifelse(d$smoke==0, "Non smoker", "Smoker")

    p1 <-
      ggplot(d, aes(.data$age, .data$height, colour = .data$Allocation)) +
      geom_point(alpha = 0.5) + theme(legend.position = "none") +
      # guides(colour = "Group") +
      scale_color_manual(values=c(control.col, treat.col), name="Group")
    p1 <- ggplotly(p1) %>% layout(xaxis = a.set, yaxis = a.set)

    p2 <-
      ggplot(d, aes(.data$Allocation, .data$fev, fill = .data$Allocation)) +
      geom_boxplot(alpha=0.5) + xlab("") + theme(legend.position = "none") +
      # guides(colour = "Group") +
      scale_fill_manual(values=c(control.col, treat.col), name="Group")
    p2 <- ggplotly(p2) %>% layout(xaxis = a.set, yaxis = a.set)

    p3 <-
      ggplot(d, aes(.data$age, .data$fev, colour = .data$Allocation)) +
      geom_point(alpha = 0.5) + theme(legend.position = "none") +
      # guides(colour = "Group") +
      scale_color_manual(values=c(control.col, treat.col), name="Group")
    p3 <- ggplotly(p3) %>% layout(xaxis = a.set, yaxis = a.set)

    p4 <-
      ggplot(d, aes(.data$height, .data$age, colour = .data$Allocation)) +
      geom_point(alpha = 0.5) + theme(legend.position = "none") +
      # guides(colour = "Group") +
      scale_color_manual(values=c(control.col, treat.col), name="Group")
    p4 <- ggplotly(p4) %>% layout(xaxis = a.set, yaxis = a.set)


    updateActionButton(session, "usedata",
                       label = "Use the Forced Expiratory Volume data")

    values$d.set <<- "fev"

    p <- plotly::subplot(
      p1,
      p2,
      p3,
      p4,
      nrows = 2,
      titleX = T,
      titleY = T,
      margin = 0.05
    ) %>% config(displayModeBar = F)

    values$data.plots <<- p

    p

  }) %>%
    bindEvent(input$applyFEV)

  output$FEVplot <- plotly::renderPlotly(plotFEV.data())

  observe({
    shinyBS::toggleModal(session, "modGetData", toggle = "close")
    shinyBS::toggleModal(session, "modMatchSettings", toggle = "open")

    values$selected.p <<- values$data.plots

  }) %>%
    bindEvent(input$usedata)

  observe({

    d <- values$d

    cols <- colnames(d)

    opts <- cols[!cols %in% c("smoke", "fev", "t", "y", "Allocation")]


    updateSelectizeInput(
      session,
      "outcome.f",
      choices = opts,
        # switch(
        # values$d.set,
        # "sim" = c("y ~ t",
        #           "y ~ t + X1 + X2",
        #           "y ~ t + X1",
        #           "y ~ t + X2"),
        # "fev" = c(
        #   "fev ~ smoke",
        #   "fev ~ smoke + age",
        #   "fev ~ smoke + height",
        #   "fev ~ smoke + sex",
        #   "fev ~ smoke + age + height",
        #   "fev ~ smoke + sex + height",
        #   "fev ~ smoke + age + sex",
        #   "fev ~ smoke + age + height + sex"
        # )
      # ),
      options = list(
        render = I(
          "
            {
              item:   function(item, escape) { return '<div><code>' + item.label + '</code></div>'; },
              option: function(item, escape) { return '<div><code>' + item.label + '</code></div>'; }
            }
          "
        )
      )
    )

    updateSelectizeInput(
      session,
      "treat.f",
      # selected = switch(values$d.set,
      #                   "sim" = "t ~ X1 + X2",
      #                   "fev" = "smoke ~ age + height"),
      choices = opts,
      selected = opts,
      # switch(
      #   values$d.set,
      #   "sim" = c("t ~ X1 + X2"),
      #   "fev" = c(
      #     "smoke ~ age + sex",
      #     "smoke ~ height + sex",
      #     "smoke ~ age + height",
      #     "smoke ~ age + height + sex"
      #   )
      # ),
      options = list(
        render = I(
          "
            {
              item:   function(item, escape) { return '<div><code>' + item.label + '</code></div>'; },
              option: function(item, escape) { return '<div><code>' + item.label + '</code></div>'; }
            }
          "
        )
      )
    )

  }) %>%
    bindEvent(values$d)

  #observer to update reactive values if they are changed
  observe({

    values$M1.dist <<- input$Dist1
    values$M1.ord <<- input$Ord1
    values$M1.rep <<- input$Rep1
    values$M2.dist <<- input$Dist2
    values$M2.ord <<- input$Ord2
    values$M2.rep <<- input$Rep2

    d <- values$d

    t.y <- c("smoke", "fev", "t", "y")[c("fev", "smoke", "t", "y") %in% colnames(d)]

    values$outcome.f <<- paste(t.y[2], "~", t.y[1], paste(NULL, input$outcome.f, sep="+ ", collapse = " "))

    values$treat.f <<- paste(t.y[1], "~",paste(input$treat.f, collapse = " + "))
  })

  output$y.formula <- renderUI(code(noquote(values$outcome.f)))

  output$t.formula <- renderUI(code(noquote(values$treat.f)))

  #start matching process
  observe({

    shinyBS::toggleModal(session, "modMatchSettings", toggle = "close")

    #values$d is null at initialisation. covers random use case.
    if (is.null(values$d)) {
      d <- values$selected.d
    } else {
      d <- values$d
    }

    a <- NULL

    t.f <- values$treat.f

    #Check for more characters on right side of formula
    test1 <- nchar(t.f) - stringi::stri_locate_first_fixed(t.f,"~")[1] <= 2

    #If there isn't, generate an error message
    if(test1) a <- "The formula for calculating the matches is incomplete"


    if(is.null(a)) {

      values$selected.d <<- NULL
      values$selected.d <<- d

    } else {

      # shinyjs::disable()

      showModal(
        modalDialog(
          title = "Error",
          easyClose = F, a)
        )
    }

  }) %>%
    bindEvent(input$usematching)

    #render data plots
    output$dataPlot <- plotly::renderPlotly({values$selected.p})

    #render data table of
    output$dataTable <- DT::renderDT(
      values$selected.d,
      options = list(
        pageLength = 25,
        initComplete = DT::JS("function(){$(this).addClass('compact');}")
      )
    )

    #render information table
    output$matchtable <- renderTable({
      Meth1 <- c(values$M1.dist, values$M1.ord, values$M1.rep)
      Meth2 <- c(values$M2.dist, values$M2.ord, values$M2.rep)

      M.list <- list(M1 = Meth1, M2 = Meth2)  %>% as.data.frame()

      c.labs <- c("Method 1", "Method 2")
      r.labs <- c("Distance", "Order", "Replacement")

      colnames(M.list) <- c.labs
      rownames(M.list) <- r.labs

      M.list
      }, striped = T, hover = T, bordered = T, rownames = T, colnames = T)

  observe({
    a <- input$sim4.X1t

    updateSliderInput(session, "sim3.X2t",
                      max=abs(a)-0.1, step=.1)

  }) %>% bindEvent(input$sim4.X1t)


  observe({
    a <- input$sim4.X2t

    updateSliderInput(session, "sim3.X1t",
                      max=abs(a)-0.1, step=.1)

  }) %>% bindEvent(input$sim4.X2t)

  #render outcome formula
  output$f.outcome <-
    renderText(paste(code(deparse1(values$outcome.f))))

  #render treatment formula
  output$f.treat <-
    renderText(paste(code(deparse1(values$treat.f))))

  #render treatment effect
  output$TE <- renderText(values$TE)

  ## To be copied in the server
  mod_SimData_server("SimData_1")
  mod_ViewData_server("ViewData_1")
  mod_MatchSettings_server("MatchSettings_1")
  mod_Info_server("Info_1")


}
