#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
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
#' @importFrom stringr str_split
#' @importFrom shinyjs enable disable useShinyjs
#' @importFrom rlang is_empty
#' @import mplot
#'
#' @noRd
#'
#'
#'
#'

#load quietly to reduce messages on shiny server
library(ggplot2, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(plotly, quietly = T, warn.conflicts = F)
library(shiny, quietly = T, warn.conflicts = F)
library(shinyBS, quietly = T, warn.conflicts = F)
library(htmltools, quietly = T, warn.conflicts = F)
library(DT, quietly = T, warn.conflicts = F)
library(markdown, quietly = T, warn.conflicts = F)


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

  p1 <- ggplot(d, aes(.data$X1, .data$X2, colour = .data$Allocation)) +
    geom_point(alpha = 0.5) + theme(legend.position = "none") +
    scale_color_manual(values=c(control.col, treat.col), name="Group") +
    theme_bw()
  p1 <- ggplotly(p1) %>% layout(xaxis = a.set, yaxis = a.set)

  p2 <- ggplot(d, aes(.data$Allocation, .data$y, fill = .data$Allocation)) +
    geom_boxplot(alpha=0.5) + xlab("") + theme(legend.position = "none") +
    scale_fill_manual(values=c(control.col, treat.col), name="Group") +
    theme_bw()
  p2 <- ggplotly(p2) %>% layout(xaxis = a.set, yaxis = a.set)

  p3 <- ggplot(d, aes(.data$X1, .data$y, colour = .data$Allocation)) +
    geom_point(alpha = 0.5) + theme(legend.position = "none") +
    scale_color_manual(values=c(control.col, treat.col), name="Group") +
    theme_bw()
  p3 <- ggplotly(p3) %>% layout(xaxis = a.set, yaxis = a.set)

  p4 <- ggplot(d, aes(.data$X2, .data$y, colour = .data$Allocation)) +
    geom_point(alpha = 0.5) + theme(legend.position = "none") +
    scale_color_manual(values=c(control.col, treat.col), name="Group") +
    theme_bw()
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
    ) %>% layout(showlegend=FALSE) %>% config(displayModeBar = F)
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

  sample(c(
    paste(
      "In 2019, King and Neilsen identified a paradox when using",
      "the propensity score for matching and demonstrated that",
      "extensive matching on the propensity score can increase",
      "bias despite improving covariate balance."
    ),
    paste(
      "The propensity score can be used in multiple way to achieve",
      "this, notably matching, weighting and stratification (aka",
      "subclassification)"
    ),
    paste(
      "Matching requires that a type of distance between units",
      "is calculated. This distance is then used to pair or match",
      "units that are close together while discarding or pruning",
      "those that are not."
    ),
    paste(
      "Weighting with the propensity score differs from matching",
      "as no units are pruned from the dataset. This is an advantage",
      "over matching in the sense that all data can remain in the",
      "analysis, giving the opportunity to calculate the average",
      "treatment effect on all units, as opposed to only the treated ones."
    ),
    paste(
      "Stratification relies on the use of the propensity score to group",
      "units into quantiles. Once the number of groups has been",
      "established, usually between 5 and 10, the treatment",
      "effect is estimated across the groups and then averaged to",
      "give a mean difference average treatment effect."
    ),
    paste(
      "Rosenbaum and Rubin (1983) defined the propensity score as",
      "the 'conditional probability of assignment to a particular",
      "treatment given a vector of observed covariates'. It is one",
      "of the most widely used statistical methods when analysing",
      "observational data."
    ),
    paste(
      "In most cases, the propensity score is estimated from",
      "available data using a simple logistic regression on a binary",
      "treatment assignment indicator."
    ),
    paste(
      "The propensity score is the probability of receiving treatment which",
      "reduces a multidimensional space to a single dimension for easy",
      "comparison between treated and untreated units."
    )),
  1)
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
  values$M1.ord <- "data"
  values$M1.rep <- F
  values$M2.dist <- "Propensity Score"
  values$M2.ord <- "data"
  values$M2.rep <- F
  values$selected.d <- create.sim.data(1)
  values$outcome.f <- "y ~ t"
  values$treat.cov1 <- c("X1","X2")
  values$treat.cov2 <- c("X1","X2")
  values$d <- NULL
  values$d.set <- "sim"
  values$selected.p <- NULL
  values$jitter <- 1
  values$cols <- NULL
  values$trueTE <- NULL


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
      rep <- sample(x = c(F, T), 2, replace = F)
    } else {
      rep <- sample(x = c(F, T), 2, replace = T)
    }

    #assign matching variables to session for use in another function
    values$outcome.f <<-
      sample(c("y ~ t", "y ~ t + X1", "y ~ t + X2", "y ~ t + X1 + X2"), 1)[[1]]
    # values$treat.f1 <<- "t ~ X1 + X2"
    # values$treat.f2 <<- "t ~ X1 + X2"
    values$treat.cov1 <- c("X1","X2")
    values$treat.cov2 <- c("X1","X2")

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

    values$d <<- d
    values$selected.p <<- plot.data(d)
    values$selected.d <<- d


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

    d <- values$selected.d

    #get the treatment and outcome variables in order
    t.y <- c("smoke", "fev", "t", "y")[c("smoke", "fev", "t", "y") %in% colnames(d)]

    #get the selections
    sel.t1 <- values$treat.cov1
    sel.t2 <- values$treat.cov2

    #generate the formula for matching and estimating
    t.f1 <- as.formula(paste(t.y[1], "~",paste(sel.t1, collapse = " + ")))
    t.f2 <- as.formula(paste(t.y[1], "~",paste(sel.t2, collapse = " + ")))

    #assign from reactivevalues to ensure values are static
    o.f <- stats::as.formula(values$outcome.f)
    TE <- values$TE
    D1 <- values$M1.dist
    O1 <- values$M1.ord
    R1 <- values$M1.rep
    D2 <- values$M2.dist
    O2 <- values$M2.ord
    R2 <- values$M2.rep

    if(is.null(values$selected.p)) values$selected.p <<- plot.data(d)

    #get matched data for 2 methods
    M1 <- matched.data(t.f1, d, D1, O1, R1)
    M2 <- matched.data(t.f2, d, D2, O2, R2)

    #store the matched data
    values$M1 <<- M1
    values$M2 <<- M2


    # suppress unnecessary warnings when generating plot
    storeWarn<- getOption("warn")
    options(warn = -1)

    #get the treatment effect if it's in the simulated data
    if(is.null(d$error)){

      newTE <- "None"

    } else {

      err <-  mean(d$error[d$t==1], na.rm = T)-mean(d$error[d$t==0], na.rm = T)
      newTE <- round(TE + err,2)
      values$trueTE <<- newTE

    }

    #outputs for sidebar on main page
    output$txt.M.TE <- renderText(newTE)
    output$txt.M.o.f <- renderText(deparse(o.f))


    #check the treatment variable from the formula
    #assign plots accordingly
    if (all.vars(o.f)[1] == "fev") {
      cp <- combined.plot("age", "height", M1, M2, te = NULL, o.f)
    } else {
      cp <- combined.plot("X1", "X2", M1, M2, te = newTE, o.f)
    }

    fonts <- list(list(color="#495057",
                       family='system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", "Noto Sans", "Liberation Sans", Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji";'))

    cp <- cp %>% layout(
      paper_bgcolor="#fff",
      font=fonts, legend=list(font=fonts)
    )

    #covers random cases
    updateSelectInput(session, "Dist1", selected = D1)
    updateSelectInput(session, "Dist2", selected = D2)
    updateSelectInput(session, "Ord1", selected = O1)
    updateSelectInput(session, "Ord2", selected = O2)
    updateCheckboxInput(session, "Rep1", value = R1)
    updateCheckboxInput(session, "Rep2", value = R2)

    #mark up as code
    t.cov1 <- HTML(paste(paste("<code>",sel.t1,"</code>", sep=""), collapse=", "))
    t.cov2 <- HTML(paste(paste("<code>",sel.t2,"</code>", sep=""), collapse=", "))


    #table of selections
    output$m.info <- renderUI({

      tags$table(
        style = "width:100%",
        tags$tr(
          tags$th(""),
          tags$th("Method 1"),
          tags$th("Method 2")
        ),
        tags$tr(
          tags$th("Covariates"),
          tags$td(t.cov1),
          tags$td(t.cov2)
        ),
        tags$tr(
          tags$th("Distance"),
          tags$td(D1),
          tags$td(D2)
        ),
        tags$tr(
          tags$th("Order"),
          tags$td(O1),
          tags$td(O2)
        ),
        tags$tr(
          tags$th("Replace"),
          tags$td(ifelse(R1, "Yes", "No")),
          tags$td(ifelse(R2, "Yes", "No"))
        )
      )

    })

    #output the combined plots
    output$distPlot <- plotly::renderPlotly(cp)

    #close the modal now that everything is finished
    removeModal()

    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({
      options(warn = storeWarn)
    }) ,ms = 15000) #25 seconds


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

    p <- plot.data(d)

    values$data.plots <<- p

    p

  }) %>%
    bindEvent(input$applySim2)

  output$sim2plot <- plotly::renderPlotly(plot2.data())

##Simulation 3
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

##Simulation 4
  plot4.data <- reactive({
    TE <- input$sim4.TE
    rho <- input$sim4.rho
    y <- 2

    X1t <- input$sim4.X1t
    X2t <- input$sim4.X2t
    X1y <- input$sim4.X1y
    X2y <- input$sim4.X2y

    d <- create.sim.data(
      sim = 4,
      te = TE,
      rho = rho,
      weight_y0 = y,
      mean1 = 0,
      mean2 = 0,
      sd1 = 1,
      sd2 = 1,
      weight_t1 = X1t,
      weight_t2 = X2t,
      weight_y1 = X1y,
      weight_y2 = X2y,
      jitter=1
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
      scale_color_manual(values=c(control.col, treat.col), name="Group")
    p1 <- ggplotly(p1) %>% layout(xaxis = a.set, yaxis = a.set)

    p2 <-
      ggplot(d, aes(.data$Allocation, .data$fev, fill = .data$Allocation)) +
      geom_boxplot(alpha=0.5) + xlab("") + theme(legend.position = "none") +
      scale_fill_manual(values=c(control.col, treat.col), name="Group")
    p2 <- ggplotly(p2) %>% layout(xaxis = a.set, yaxis = a.set)

    p3 <-
      ggplot(d, aes(.data$age, .data$fev, colour = .data$Allocation)) +
      geom_point(alpha = 0.5) + theme(legend.position = "none") +
      scale_color_manual(values=c(control.col, treat.col), name="Group")
    p3 <- ggplotly(p3) %>% layout(xaxis = a.set, yaxis = a.set)

    p4 <-
      ggplot(d, aes(.data$height, .data$age, colour = .data$Allocation)) +
      geom_point(alpha = 0.5) + theme(legend.position = "none") +
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

  #plot FEV data
  output$FEVplot <- plotly::renderPlotly(plotFEV.data())


  observe({


    values$M1.dist <<- input$Dist1
    values$M1.ord <<- input$Ord1
    values$M1.rep <<- input$Rep1
    values$M2.dist <<- input$Dist2
    values$M2.ord <<- input$Ord2
    values$M2.rep <<- input$Rep2

  })


  observe({

    d <- values$d

    values$selected.p <<- values$data.plots

    sel.t1 <- values$treat.cov1
    sel.t2 <- values$treat.cov2

    cols <- colnames(d)

    opts <- cols[!cols %in% c("smoke", "fev", "t", "y", "Allocation", "te")]

    #get the non-treatment covariates out of the formula
    if(is.null(values$outcome.f)){
      sel.y <- NULL
    } else {
      sel.y <- stringr::str_split(values$outcome.f," ")[[1]][-c(1:4)]
      sel.y <- sel.y[sel.y!="+"]
      if(rlang::is_empty(sel.y)) sel.y <- NULL
    }

    updateSelectizeInput(
      session,
      "outcome.f",
      selected = sel.y,
      choices = opts,
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
      "treat.f1",
      selected = sel.t1,
      choices = opts,
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
      "treat.f2",
      selected = sel.t2,
      choices = opts,
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

    shinyBS::toggleModal(session, "modGetData", toggle = "close")
    shinyBS::toggleModal(session, "modMatchSettings", toggle = "open")

  }) %>%
    bindEvent(input$usedata)

  #set the displayed formula based on the selected dataset
  observe({
    #get the column names
    d.cols <- colnames(values$d)

    if (is.null(d.cols)) d.cols <- colnames(values$selected.d)

    #get the treatment and outcome variables in order
    t.y <- c("smoke", "fev", "t", "y")[c("smoke", "fev", "t", "y") %in% d.cols]

    #get the selections
    sel.o <- input$outcome.f

    #generate the formula for matching and estimating
    y.f <- paste(t.y[2], "~", t.y[1], paste(NULL, sel.o, sep="+ ", collapse = " "))

    #display the formulas
    output$y.formula <- renderUI(code(noquote(y.f)))

    #store the formulas
    values$outcome.f <<- y.f

  })

  #start matching process
  observe({

    shinyBS::toggleModal(session, "modMatchSettings", toggle = "close")

    #values$d is null at initialisation. covers random use case.
    if (is.null(values$d)) {
      d <- values$selected.d
    } else {
      d <- values$d
    }

    values$selected.d <<- NULL
    values$selected.d <<- d

  }) %>%
    bindEvent(input$usematching)


  #toggle use matching button based on formula being complete
  observe({

    f1 <- input$treat.f1
    f2 <- input$treat.f2

    if(is.null(f1) | is.null(f2)){
      shinyjs::disable("usematching")
      shiny::updateActionButton(session, "usematching",
                                label = "The formula for calculating the matches is incomplete")
    } else {
      shinyjs::enable("usematching")
      shiny::updateActionButton(session, "usematching",
                                label = "Use these matching settings")
      values$treat.cov1 <<- f1
      values$treat.cov2 <<- f2
    }
  })


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
    output$matchtable <- renderUI({

      t.cov1 <- HTML(paste(paste("<code>",values$treat.cov1,"</code>", sep=""), collapse=", "))
      t.cov2 <- HTML(paste(paste("<code>",values$treat.cov2,"</code>", sep=""), collapse=", "))


      tags$table(
        style = "width:100%",
        tags$tr(
          tags$th(""),
          tags$th("Method 1"),
          tags$th("Method 2")
        ),
        tags$tr(
          tags$th("Covariates"),
          tags$td(t.cov1),
          tags$td(t.cov2)
        ),
        tags$tr(
          tags$th("Distance"),
          tags$td(values$M1.dist),
          tags$td(values$M2.dist)
        ),
        tags$tr(
          tags$th("Order"),
          tags$td(values$M1.ord),
          tags$td(values$M2.ord)
        ),
        tags$tr(
          tags$th("Replace"),
          tags$td(ifelse(values$M1.rep, "Yes", "No")),
          tags$td(ifelse(values$M2.rep, "Yes", "No"))
        )
      )

    })

  #render outcome formula
  output$f.outcome <-
    renderText(paste(code(noquote(values$outcome.f))))

  #render treatment formula
  output$f1.treat <-
    renderText(paste(code(noquote(values$treat.f1))))

  #render treatment formula
  output$f2.treat <-
    renderText(paste(code(noquote(values$treat.f2))))

  #render treatment effect
  output$TE <- renderText(values$trueTE)

  ## To be copied in the server
  mod_SimData_server("SimData_1")
  mod_ViewData_server("ViewData_1")
  mod_MatchSettings_server("MatchSettings_1")

}
