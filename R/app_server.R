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
#' @importFrom stats as.formula sd
#' @importFrom htmltools HTML
#' @importFrom DT JS renderDT
#' @importFrom utils data
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
    values$outcome.f <<- sample(c("y ~ t",
                                  "y ~ t + X1",
                                  "y ~ t + X2",
                                  "y ~ t + X1 + X2"), 1)[[1]]
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
    d <- create.sim.data(sim=sample(1:4, 1),te=values$TE)

    values$d <<- d
    values$selected.p <<- plot.data(d)
    values$selected.d <<- d


  }) %>%
    bindEvent(input$butRandom)

  dist.plots <- reactive({

    # get the matched data
    m1 <- values$M1
    m2 <- values$M2

    # put the data in the same order in a
    ords <- cbind(m1$paired.data[c("d.order","dist", "distance")], Method="1") %>%
      rbind(cbind(m2$paired.data[c("d.order","dist", "distance")], Method="2")) %>%
      group_by(.data$Method) %>%
      mutate(st=.data$dist/stats::sd(.data$dist),
             ord.t=paste0("<b>Ordered by Matching Distance</b><br>",
                          "Method ",.data$Method,": ",.data$distance,"<br>",
                         "Pair Number: ", .data$d.order,"<br>",
                         "Distance: ", round(.data$dist,3)),
             st.t=paste0("<b>Ordered by Matching Distance</b><br>",
                         "Method ",.data$Method,": ",.data$distance,"<br>",
                          "Pair Number: ", .data$d.order,"<br>",
                          "Standard Deviations from zero: ", round(.data$st,3))) %>%
      ungroup()


    ord.plot.raw <- ggplot(ords, aes(.data$d.order, .data$dist, color=.data$Method,
                                     text=.data$ord.t, size=.data$Method,
                                     shape=.data$Method)) +
      geom_point() +
      labs(x = "Pairs Ordered by Distance", y = "Distance") +
      scale_color_manual(values=c("1"='#FED148', "2"='#B241B4'), name="Method") +
      scale_shape_manual(values=c("1"=21, "2"=20), name="Method") +
      scale_size_manual(values=c(3,1.5)) +
      theme_bw()

    ord.plot.raw <- ord.plot.raw %>% ggplotly(tooltip = "text")

    ord.plot.std <- ggplot(ords, aes(.data$d.order, .data$st, color=.data$Method,
                                     text=.data$st.t, size=.data$Method,
                                     shape=.data$Method)) +
      geom_point() +
      labs(x = "Pairs Ordered by Distance", y = "Stardard Deviations") +
      scale_color_manual(values=c("1"='#FED148', "2"='#B241B4'), name="Method") +
      scale_shape_manual(values=c("1"=21, "2"=20), name="Method") +
      scale_size_manual(values=c(3,1.5)) +
      theme_bw()

    ord.plot.std <- ord.plot.std %>% ggplotly(tooltip = "text")

    seqs <- cbind(m1$paired.data[c("seq.order","dist", "distance")], Method="1") %>%
      rbind(cbind(m2$paired.data[c("seq.order","dist", "distance")], Method="2")) %>%
      group_by(.data$Method) %>%
      mutate(st =.data$dist/stats::sd(.data$dist),
             ord.t = paste0("<b>Ordered by Matching Sequence</b><br>",
                            "Method ",.data$Method,": ",.data$distance,"<br>",
                         "Pair Number: ", .data$seq.order,"<br>",
                         "Distancet: ", round(.data$st,3)),
             st.t=paste0("<b>Ordered by Matching Sequence</b><br>",
                         "Method ",.data$Method,": ",.data$distance,"<br>",
                         "Pair Number: ", .data$seq.order,"<br>",
                         "Standard Deviations from zero: ", round(.data$st,3))) %>%
      ungroup()


    seq.plot.raw <- ggplot(seqs, aes(.data$seq.order, .data$dist, color=.data$Method,
                                     text=.data$ord.t, size=.data$Method,
                                     shape=.data$Method)) +
      geom_point() +
      labs(x = "Pairs Ordered by Sequence", y = "Distance") +
      scale_color_manual(values=c("1"='#FED148', "2"='#B241B4'), name="Method") +
      scale_shape_manual(values=c("1"=21, "2"=20), name="Method") +
      scale_size_manual(values=c(3,1.5)) +
      theme_bw()

    seq.plot.raw <- seq.plot.raw %>% ggplotly(tooltip = "text")

    seq.plot.std <- ggplot(seqs, aes(.data$seq.order, .data$st, color=.data$Method,
                                     text=.data$st.t, size=.data$Method,
                                     shape=.data$Method)) +
      geom_point() +
      labs(x = "Pairs Ordered by Sequence", y = "Stardard Deviations") +
      scale_color_manual(values=c("1"='#FED148', "2"='#B241B4'), name="Method") +
      scale_shape_manual(values=c("1"=21, "2"=20), name="Method") +
      scale_size_manual(values=c(3,1.5)) +
      theme_bw()

    seq.plot.std <- seq.plot.std %>% ggplotly(tooltip = "text")

    plotly::subplot(seq.plot.raw, ord.plot.raw, seq.plot.std, ord.plot.std,
                    nrows = 2, shareX = T, shareY = T) %>%
      layout(showlegend=FALSE)

  })

  output$distsPlot <- plotly::renderPlotly({dist.plots()})

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

    #assign from reactive values
    txt.o.f <- values$outcome.f
    o.f <- stats::as.formula(txt.o.f)
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
      values$trueTE <- NULL

    } else {

      err <-  mean(d$error[d$t==1], na.rm = T)-mean(d$error[d$t==0], na.rm = T)
      newTE <- round(TE + err,2)
      values$trueTE <<- newTE

    }

    #outputs for sidebar on main page
    output$txt.M.TE <- renderText(newTE)
    output$txt.M.o.f <- renderUI(code(txt.o.f))


    #check the treatment variable from the formula
    #assign plots accordingly
    if (all.vars(o.f)[1] == "fev") {
      cp <- combined.plot("age", "height", M1, M2, te = NULL, o.f)
    } else {
      cp <- combined.plot("X1", "X2", M1, M2, te = newTE, o.f)
    }

    #update formatting for plot
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



  #model comparisons
  models <- reactive({
    m1 <- values$M1
    m2 <- values$M2
    t.var <- m1$treatment
    #assign data to variable
    dt <- m1$data

    #needed for weights to work within function


    fs <- switch(t.var,
                 "smoke"=c(fev~smoke, fev~smoke+age, fev~smoke+height,
                           fev~smoke+sex, fev~smoke+age+height,
                           fev~smoke+age+sex, fev~smoke+height+sex,
                           fev~smoke+height+age+sex),
                 "t"=c(y~t, y~t+X1, y~t+X2, y~t+X1+X2))

    environment(fs) <- environment()

    #matched data 1 estimates
    m1.ests <- sapply(fs,
                   function(x) {
                     mod.s <- lm(x, m1$matched.data)
                     cci <- cbind(coef(mod.s)[[t.var]],
                                  t(confint(mod.s)[t.var,]),
                                  "Method 1", deparse(x))
                     return(cci) }
    ) %>% t() %>% as.data.frame() %>% mutate(across(c(1:3), as.numeric))


    #matched data 2 estimates
    m2.ests <- sapply(fs,
                      function(x) {
                        mod.s <- lm(x, m2$matched.data)
                        cci <- cbind(coef(mod.s)[[t.var]],
                                     t(confint(mod.s)[t.var,]),
                                     "Method 2", deparse(x))
                        return(cci) }
    ) %>% t() %>% as.data.frame() %>% mutate(across(c(1:3), as.numeric))


    #vanilla model
    raw.ests <- sapply(fs,
              function(x) {
                mod.s <- lm(x, dt)
                cci <- cbind(coef(mod.s)[[t.var]],
                             t(confint(mod.s)[t.var,]),
                             "Unadjusted", deparse(x))
                return(cci) }
    ) %>% t() %>% as.data.frame() %>% mutate(across(c(1:3), as.numeric))




    #weighted model
    wt.ests <- sapply(fs,
                       function(x) {
                         mod.s <- lm(x, dt, weights=m1$wt.ATT)
                         cci <- cbind(coef(mod.s)[[t.var]],
                                      t(confint(mod.s)[t.var,]),
                                      "Weighted", deparse(x))
                         return(cci) }
    ) %>% t() %>% as.data.frame() %>% mutate(across(c(1:3), as.numeric))
    # mod.w <- lm(f1, dt, weights=m$wt.ATE)

    #Stratified estimates
    max.strat <- max(as.numeric(m1$stratification))

    strat.ests <- sapply(fs,
          function(y)
            sapply(
              1:max.strat,
              function(x) {
                mod.s <- lm(y, dt[m1$stratification==x,])
                return(cbind(coef(mod.s),confint(mod.s))[t.var,])
              }) %>% as.data.frame() %>% apply(1, mean, na.rm=T)

          ) %>% t() %>% as.data.frame() %>%
      mutate(across(c(1:3), as.numeric), Method="Stratified", Formula=fs,
             )

    # #this has different col names to the other outputs.
    # #updated for rbind.
    colnames(strat.ests) <- paste0("V",c(1:5))

    tot.ests <- rbind(m1.ests, m2.ests, raw.ests, wt.ests, strat.ests)

    colnames(tot.ests) <- c("Estimate", "CI_L", "CI_H", "Model", "Formula")

    #m.f added for sorting and plotting.
    tot.ests <- tot.ests %>%
      arrange(desc(Model), desc(nchar(Formula)), desc(Formula)) %>%
      mutate(m.f=factor(paste0(Model,"\n",Formula),
                        levels=paste0(Model,"\n",Formula)))

    filt <- input$methods

    return(tot.ests[tot.ests$Model %in% filt,])

  })

  #test to see if I could generate the data
  # output$tblests <- renderTable({models()})


  #plot the estimates
  comp.plot <- reactive({

    mod <- models()

    true.est <- values$trueTE

    p <-  ggplot(mod, aes(y=m.f, x=Estimate, colour=Model, lty=Model)) +
        geom_point(size=3) +
        geom_errorbarh(aes(xmin=CI_L, xmax=CI_H), height=0.25, lwd=1.5) +
        geom_vline(xintercept=true.est, color="red", lty=2) +
        labs(x="Estimate of the Treatment Effect",
             y="Model and Formula Used") +
        scale_color_manual(values=c("Method 1"='#FED148',
                                    "Method 2"='#B241B4',
                                    "Unadjusted"="#272727",
                                    "Weighted"="#009FB7",
                                    "Stratified"="#696773")) +
        scale_linetype_manual(values=c("Method 1"='solid',
                                       "Method 2"='dashed',
                                       "Unadjusted"="dotted",
                                       "Weighted"="dotdash",
                                       "Stratified"="twodash")) +
        theme_bw() +
        theme(text = element_text(size = 18), legend.position = "none")

    return(p)

  }) %>%
    bindEvent(input$butMethods)

  output$ests.plot <- renderPlot({comp.plot()})


  #Reactive table with matching data to display
  #wasn't able to get the data I wanted in the format I want easily
  table.data <- reactive({

    #get the matched data that's in use
    m1 <- values$M1
    m2 <- values$M2

    #get the covariates used for matching
    cov1 <- all.vars(m1$formula)[-1]
    cov2 <- all.vars(m2$formula)[-1]

    #recode the selected variables and turn into a single string
    #essentially "<code>var1</code>, <code>var2</code>"
    #for as many variables as specified
    t.cov1 <- HTML(paste(
                      paste("<code>",cov1,"</code>", sep="")
                      , collapse=", "))
    t.cov2 <- HTML(paste(
                      paste("<code>",cov2,"</code>", sep="")
                      , collapse=", "))

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
        tags$td(m1$distance),
        tags$td(m2$distance)
      ),
      tags$tr(
        tags$th("Order"),
        tags$td(m1$order),
        tags$td(m2$order)
      ),
      tags$tr(
        tags$th("Replace"),
        tags$td(ifelse(m1$replacement, "Yes", "No")),
        tags$td(ifelse(m2$replacement, "Yes", "No"))
      )
    )

  })

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
    wt.X1 <- diff - input$sim2.x.overlap
    wt.X2 <- diff - input$sim2.y.overlap

    d <- create.sim.data(
      sim = 2,
      te = TE,
      cov_min = val.min,
      cov_max = val.max,
      wt_X1 = wt.X1,
      wt_X2 = wt.X2,
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

    opts <- cols[!cols %in% c("smoke", "fev", "t", "y", "Allocation", "error")]

    #get the non-treatment covariates out of the formula
    if(is.null(values$outcome.f)){
      sel.y <- NULL
    } else {
      sel.y <- strsplit(values$outcome.f," ")[[1]][-c(1:4)]
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

  #render data table without the error variable
  output$dataTable <- DT::renderDT({
    d <- values$selected.d

    if("error" %in% colnames(d)){ d[colnames(d)!="error"]
    } else { d }

    },
    options = list(
      pageLength = 25,
      initComplete = DT::JS("function(){$(this).addClass('compact');}")
    )
  )


  #render information tables
  output$matchtable <- renderUI(table.data())
  output$m.info <- renderUI(table.data())

  #render outcome formula
  output$f.outcome <- renderText(paste(code(noquote(values$outcome.f))))

  #render treatment formula
  output$f1.treat <- renderText(paste(code(noquote(values$treat.f1))))

  #render treatment formula
  output$f2.treat <- renderText(paste(code(noquote(values$treat.f2))))

  #render treatment effect
  output$TE <- renderText(values$trueTE)

  ## To be copied in the server
  mod_SimData_server("SimData_1")
  mod_ViewData_server("ViewData_1")
  mod_MatchSettings_server("MatchSettings_1")

}
