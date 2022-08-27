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
library(MatchIt)
library(optmatch)
library(ggplot2)
library(dplyr)
library(catdata)
library(mplot)
library(plotly)
library(ggforce)
library(shiny)
library(shinyBS)
library(ggpubr)


plot.data <- function(d){
  ggpubr::ggarrange(
    ggplot(d, aes(X1, X2, colour=t.char)) + geom_point(alpha=0.5),
    ggplot(d, aes(t.char, y, fill=t.char)) + geom_boxplot() +
      xlab("Groups"),
    ggplot(d, aes(X1, y, colour=t.char)) + geom_point(alpha=0.5),
    ggplot(d, aes(X2, y, colour=t.char)) + geom_point(alpha=0.5),
    ncol=2, nrow=2, common.legend = T, legend = "none")
}


random.data <- function(TE=round(runif(1, -10, 10),1)){

  rand.data <- round(runif(1,0.51,3.49))

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
  paste0("Method ", M.num,": ", M$distance, " ",
    ifelse(M$replace, "With", "without"),
    " replacement, matching ordered by ", M$order)
}


##SERVER

app_server <- function(input, output, session) {
  # Your application server logic

  values <- reactiveValues(

    TE=round(runif(1, -10, 10),1),
    M1.dist="Mahalanobis",
    M1.ord="data",
    M1.rep=T,
    M2.dist="Propensity Score",
    M2.ord="data",
    M2.rep=T,
    selected.d=random.data(round(runif(1, -10, 10),1))

  )


  observeEvent(input$butMatchData, {


    values$TE <<- round(runif(1, -10, 10),1)

    values$selected.d <<- random.data(values$TE)

  }, ignoreNULL = TRUE)


  observeEvent(values$selected.d, {

    req(values$TE,values$selected.d)

    showModal(modalDialog(
      title = "Loading",
      easyClose = F,
      "Please wait while the object loads.",
      footer = tagList()
    ))


    TE <- values$TE
    d <- values$selected.d

    D1 <- input$Dist1
    O1 <- input$Ord1
    R1 <- input$Rep1
    D2 <- input$Dist2
    O2 <- input$Ord2
    R2 <- input$Rep2

    M1 <- matched.data(t~X1+X2, d, D1, O1, R1)
    M2 <- matched.data(t~X1+X2, d, D2, O2, R2)

    values$M1 <<- M1
    values$M2 <<- M2

    M1.desc <- match.detail(M1,1)

    M2.desc <- match.detail(M2,2)

    cp <- combined.plot("X1", "X2", "y", M1, M2, te=TE)

    output$M1.text <- renderText(M1.desc)

    output$M2.text <- renderText(M2.desc)

    output$distPlot <- renderPlotly(cp)

    removeModal()

  })


  #create plot for simulation 1
  #also store data in values
  #and update the label for the usedata button
  plot1.data <- eventReactive(input$applySim1,{

    TE <- input$sim1.TE

    d <- create.sim1.data(TE)

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 1")

    values$TE <<- TE
    values$d <<- d

    plot.data(d)

  })

  output$sim1plot <- renderPlot(plot1.data())


  observeEvent(input$sim2.X1.val, {

    Val <- max(input$sim2.X1.val)-min(input$sim2.X1.val)

    updateSliderInput(session,"sim2.x.overlap", min=0, max=Val)
    updateSliderInput(session,"sim2.y.overlap", min=0, max=Val)

  })

  #create plot for simulation 2
  #also store data in values
  #and update the label for the usedata button
  plot2.data <- eventReactive(input$applySim2,{

    TE <- input$sim2.TE
    val.min <- min(input$sim2.X1.val)
    val.max <- max(input$sim2.X1.val)
    diff <- val.max - val.min
    overlap.X1 <- diff - input$sim2.x.overlap
    overlap.X2 <- diff - input$sim2.y.overlap

    d <- values$d <- create.sim2.data(TE, val.min, val.max, overlap.X1, overlap.X2)

    values$TE <<- TE
    values$d <<- d

    updateActionButton(session, "usedata",
                       label = "Use the data from Simulation 2")

    plot.data(d)

    })

  output$sim2plot <- renderPlot(plot2.data())


  plot3.data <- eventReactive(input$applySim3,{

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

    })

  output$sim3plot <- renderPlot(plot3.data())


  observeEvent(input$usedata, {

    toggleModal(session, "modGetData", toggle="close")
    toggleModal(session, "modMatchSettings", toggle="open")
  })


  observe({
      values$M1.dist <<- input$Dist1
      values$M1.ord <<- input$Ord1
      values$M1.rep <<- input$Rep1
      values$M2.dist <<- input$Dist2
      values$M2.ord <<- input$Ord2
      values$M2.rep <<- input$Rep2
  })


  observeEvent(input$usematching, {

    values$selected.d <<- values$d

    toggleModal(session, "modMatchSettings", toggle="close")

    })


  output$dataPlot <- renderPlot(plot.data(values$selected.d))

  output$dataTable <- renderTable(values$selected.d, striped=T,
                                  hover=T, bordered=T)

  output$matchtable <- renderTable({

    Meth1 <- c("t ~ X1 + X2", values$M1.dist, values$M1.ord, values$M1.rep, values$TE)
    Meth2 <- c("t ~ X1 + X2", values$M2.dist, values$M2.ord, values$M2.rep, values$TE)

    M.list <- list(`Method 1`=Meth1, `Method 2`=Meth2)  %>% as.data.frame()

    labs <- c("Function","Distance", "Order", "Replacement", "Treatment Effect")

    rownames(M.list) <- labs

    M.list
  }, striped=T, hover=T, bordered=T)

  ## To be copied in the server
  mod_SimData_server("SimData_1")
  mod_ViewData_server("ViewData_1")
  mod_MatchSettings_server("MatchSettings_1")


}

