#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
#'
#'
#'

#
# library(MatchIt)
library(optmatch)
library(ggplot2)
library(dplyr)
# library(catdata)
# library(mplot)
library(plotly)
library(ggforce)
library(shiny)
library(shinyBS)
# library(ggpubr)
library(htmltools)
library(shinythemes)
library(DT)


plot.data <- function(d){

  a.set <- list(title=list(standoff=2,autorange = TRUE, tickmode="auto"))

  # ggpubr::ggarrange(
  p1 <- ggplot(d, aes(X1, X2, colour=Allocation)) +
    geom_point(alpha=0.5) + theme(legend.position = "none") +
    guides(colour="Group")
  p1 <- ggplotly(p1) %>% layout(xaxis=a.set, yaxis=a.set)

  p2 <- ggplot(d, aes(Allocation, y, fill=Allocation)) + geom_boxplot() +
    xlab("") + theme(legend.position = "none") +
    guides(colour="Group")
  p2 <- ggplotly(p2) %>% layout(xaxis=a.set, yaxis=a.set)

  p3 <- ggplot(d, aes(X1, y, colour=Allocation)) + geom_point(alpha=0.5)  +
    theme(legend.position = "none") +
    guides(colour="Group")
  p3 <- ggplotly(p3) %>% layout(xaxis=a.set, yaxis=a.set)

  p4 <- ggplot(d, aes(X2, y, colour=Allocation)) + geom_point(alpha=0.5)  +
    theme(legend.position = "none") +
    guides(colour="Group")
  p4 <- ggplotly(p4) %>% layout(xaxis=a.set, yaxis=a.set)

  return(
    plotly::subplot(p1,p2,p3,p4, nrows=2, titleX=T, titleY = T,
                    #widths = c(0.45,0.45), heights = c(0.45,0.45),
                    margin = 0.05) %>% config(displayModeBar = F)
         )
}


random.data <- function(TE=round(runif(1, -10, 10),1)){

  rand.data <- sample(1:3,1)

  if (rand.data==1){
    d <- create.sim1.data(te=TE)
  } else if (rand.data==2){
    d <- create.sim2.data(te=TE)
  } else if (rand.data==3) {
    d <- create.sim3.data(te=TE)
  }

  return(d)

}


match.detail <- function(M, M.num){

  ord.s <- paste0("Matches were calculated using the ",
                 switch(M$order,
                        "data"="order the data was already in.",
                        "largest"="the largest propensity score first.",
                        "smallest"="the smallest propensity score first.",
                        "random"="a random order."))

  # paste0(
      paste0("*Method ", M.num, ": ",
      switch(M$distance,
             "Mahalanobis"="Mahalanobis Distance",
             "Propensity Score"="Propensity Score"),
      " Matching was performed ",
      ifelse(M$replace, "with", "without"), " replacement.",
      ifelse(M$replace, "", ord.s)
      )

}


##SERVER

app_server <- function(input, output, session) {
  # Your application server logic

  values <- reactiveValues(

    TE=2,
    outcome.f=y~t,
    M1.dist="Mahalanobis",
    M1.ord="data",
    M1.rep=T,
    M2.dist="Propensity Score",
    M2.ord="data",
    M2.rep=T,
    selected.d=NULL,#random.data(2),
    outcome.f=y~t,
    treat.f=t~X1+X2,
    d=NULL

  )

  observe({

    if (is.null(values$selected.d)) {

      updateActionButton(session, "usedata",
                         label = "Please generate some data.")
    } else {
      updateActionButton(session, "usedata",
                         label = "Continue without changing data.")
    }

    }) %>%
    bindEvent(input$butGetData)


  observe({

    values$TE <<- round(runif(1, -10, 10),1)

    values$selected.d <<- random.data(values$TE)

    values$outcome.f <<- y ~ t

    dist <- sample(x=c("Mahalanobis", "Propensity Score"), 2)
    ord <- sample(x=c("data", "smallest", "largest", "random"), 2)

    if(dist[1]==dist[2] & ord[1]==ord[2]){
      rep <- sample(x=c(F,T), 2, replace = T)
    } else {
      rep <- sample(x=c(F,T), 2, replace = F)
    }

    values$outcome.f <<- sample(c(y~t, y~t+X1, y~t+X2, t~t+X1+X2),1)[[1]]
    values$treat.f <<- t~X1+X2

    values$M1.dist <<- dist[[1]]
    values$M1.ord <<- ord[[1]]
    values$M1.rep <<- rep[[1]]

    values$M2.dist <<- dist[[2]]
    values$M2.ord <<- ord[[2]]
    values$M2.rep <<- rep[[2]]

    }) %>%
    bindEvent(input$butRandom)

  #, ignoreNULL = TRUE)

  # listener <- reactive({
  #   list(values$selected.d, values$run)
  # })


  observe({

    req(values$TE,values$selected.d)
    # ,
    #     values$M1.dist,values$M1.ord,values$M1.rep,
    #     values$M2.dist,values$M2.ord,values$M2.rep)

    showModal(modalDialog(
      title = "Loading",
      easyClose = F,
      "Please wait while the object loads.",
      footer = tagList()
    ))


    TE <- values$TE
    d <- values$selected.d
    o.f <- values$outcome.f
    t.f <- values$treat.f

    D1 <- values$M1.dist #input$Dist1
    O1 <- values$M1.ord #input$Ord1
    R1 <- values$M1.rep #input$Rep1
    D2 <- values$M2.dist #input$Dist2
    O2 <- values$M2.ord #input$Ord2
    R2 <- values$M2.rep #input$Rep2

    # print(list(TE, d, f, D1, O1, R1, D2, O2, R2))

    # print(d)

    M1 <- matched.data(t.f, d, D1, O1, R1)
    M2 <- matched.data(t.f, d, D2, O2, R2)

    values$M1 <<- M1
    values$M2 <<- M2

    M1.desc <- match.detail(M1,1)

    M2.desc <- match.detail(M2,2)

    cp <- combined.plot("X1", "X2", M1, M2, te=TE, o.f)

    output$M1.text <- renderText(M1.desc)

    output$M2.text <- renderText(M2.desc)

    output$distPlot <- renderPlotly(cp)

    removeModal()

    }) %>%
    bindEvent(values$selected.d, ignoreInit = F, ignoreNULL = F)


  #create plot for simulation 1
  #also store data in values
  #and update the label for the usedata button
  plot1.data <- reactive({

    TE <- input$sim1.TE

    d <- create.sim1.data(TE)

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 1")

    # shinyjs::enable("usedata")

    plot.data(d)

    }) %>%
    bindEvent(input$applySim1)

  output$sim1plot <- renderPlotly(plot1.data())


  observe({

    Val <- max(input$sim2.X1.val)-min(input$sim2.X1.val)

    updateSliderInput(session,"sim2.x.overlap", min=0, max=Val)
    updateSliderInput(session,"sim2.y.overlap", min=0, max=Val)

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

    d <- create.sim2.data(TE, val.min, val.max, overlap.X1, overlap.X2)

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 2")

    plot.data(d)

    }) %>%
    bindEvent(input$applySim2)

  output$sim2plot <- renderPlotly(plot2.data())


  plot3.data <- reactive({

    TE <- input$sim3.TE
    rho <- input$sim3.rho
    y <- input$sim3.y

    X1m <- input$sim3.X1m
    X2m <- input$sim3.X2m
    X1sd <- input$sim3.X1sd
    X2sd <- input$sim3.X2sd
    X1t <- input$sim3.X1t
    X2t <- input$sim3.X2t
    X1y <- input$sim3.X1y
    X2y <- input$sim3.X2y

    txt <- ""

    if(!between(X1m,-25,25)){
      txt <- paste0(txt,br(),"X1 Mean")
    }

    if(!between(X2m,-25,25)){
      txt <- paste0(txt,br(),"X2 Mean")
    }

    if(!between(X1sd,0,30)){
      txt <- paste0(txt,br(),"X1 Standard Deviation")
    }

    if(!between(X2sd,0,30)){
      txt <- paste0(txt,br(),"X2 Standard Deviation")
    }

    if(!between(X1t,-25,25)){
      txt <- paste0(txt,br(),"X1 Effect on Treatment")
    }

    if(!between(X2t,-25,25)){
      txt <- paste0(txt,br(),"X2 Effect on Treatment")
    }

    if(!between(X1y,-25,25)){
      txt <- paste0(txt,br(),"X1 Effect on Outcome")
    }

    if(!between(X2y,-25,25)){
      txt <- paste0(txt,br(),"X2 Effect on Outcome")
    }

    if (txt=="") { OOR <- NULL } else {
      OOR <- paste0("The following variables are outside the set ranges:",txt) %>%
                HTML()
    }


    if (!is.null(OOR)) {
      showModal(modalDialog(
        title = "Out of Range",
        easyClose = T,
        OOR
      ))

      } else {

      d <- create.sim3.data(
              te=TE, rho=rho, weight_y0=y,
              mean1=X1m, mean2=X2m, sd1=X1sd, sd2=X2sd,
              weight_t1=X1t, weight_t2=X2t,
              weight_y1=X1y, weight_y2=X2y)

      values$TE <<- TE
      values$d <<- d

      updateActionButton(session, "usedata",
                         label = "Use the data from Simulation 3")

      plot.data(d)

      }

    }) %>%
    bindEvent(input$applySim3)

  output$sim3plot <- renderPlotly(plot3.data())


  observe({

    toggleModal(session, "modGetData", toggle="close")
    toggleModal(session, "modMatchSettings", toggle="open")
    }) %>%
    bindEvent(input$usedata)


  observe({
      values$M1.dist <<- input$Dist1
      values$M1.ord <<- input$Ord1
      values$M1.rep <<- input$Rep1
      values$M2.dist <<- input$Dist2
      values$M2.ord <<- input$Ord2
      values$M2.rep <<- input$Rep2
      values$outcome.f <<- as.formula(input$outcome.f)
    })


  observe({
    addTooltip(session, id="sim3.X1sd",
               title="Enter the standard deviation of X1 here.",
               placement = "bottom",trigger = "hover",
               options = list(container = "body"))
  }) %>%
      bindEvent(input$Sim3)


  observe({

    toggleModal(session, "modMatchSettings", toggle="close")

    if (is.null(values$d)) {d <- values$selected.d} else {d <- values$d}

    values$selected.d <<- NULL

    values$selected.d <<- d

    }) %>%
    bindEvent(input$usematching)

  # observe({
  #
  #   b
  #
  # }) %>%
  #   bindEvent(input$butInfo)


  output$dataPlot <- renderPlotly(plot.data(values$selected.d))

  output$dataTable <- renderDT(values$selected.d,
                               options = list(
                                 initComplete = JS(
                                   "function(){$(this).addClass('compact');}")))

  output$f.outcome <- renderText(paste(code(deparse1(values$outcome.f))))

  output$f.treat <- renderText(paste(code(deparse1(values$treat.f))))

  output$TE <- renderText(values$TE)

  output$matchtable <- renderTable({


    Meth1 <- c(values$M1.dist, values$M1.ord, values$M1.rep)
    Meth2 <- c(values$M2.dist, values$M2.ord, values$M2.rep)

    M.list <- list(M1=Meth1, M2=Meth2)  %>% as.data.frame()

    c.labs <- c("Method 1", "Method 2")
    r.labs <- c("Distance", "Order", "Replacement")

    colnames(M.list) <- c.labs
    rownames(M.list) <- r.labs

    M.list
  }, striped=T, hover=T, bordered=T, rownames = T, colnames = T)


  ## To be copied in the server
  mod_SimData_server("SimData_1")
  mod_ViewData_server("ViewData_1")
  mod_MatchSettings_server("MatchSettings_1")
  mod_Info_server("Info_1")


}

