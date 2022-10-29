#' multimatchplot
#'
#' @description ReA fct function
#'
#' @param xvar a scalar character of the name of the variable you want on the x-axis
#' @param yvar a scalar character of the name of the variable you want on the y-axis
#' @param M1 the output from the `matched.data()` function
#' @param M2 the output from the `matched.data()` function
#' @param te the treatment effect you would like displayed, can be NULL (default)
#' @param outcome.f a two-sided formula to estimate the the effect of treatment on the outcome, must at least contain the treatment variable on the right side (e.g. outcome ~ treatment)
#'
#' @return Returns a Plotly object with 2 matching comparisons as specified in M1 and M2 as well as a  plot showing the standardised mean differences between covariates and matching methods and a plot showing the estimated treatment effect for the two matching methods with the raw, and weighted and stratified propensity score estimates.
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom stats coef
#'
#'
#'

combined.plot <- function(xvar, yvar, M1, M2, te=NULL,
                          outcome.f=y~t){
  treatment <- M1$treatment

  outcome <- all.vars(outcome.f)[1]


  gg1 <- matching.plot(M1, xvar, yvar, multi=T, plot.n=1)

  gg2 <- matching.plot(M2, xvar, yvar, multi=T, plot.n=2)

  gg3 <- std.means.plot(M1, M2, xvar, yvar, treatment)

  gg4 <- estimates.plot(M1, M2, outcome.f, te)

  subplot(gg1, gg2, gg3, gg4,
          widths = c(0.45,0.45), heights = c(0.45,0.45),
          margin = 0.05, shareX = F, shareY = F, nrows = 2,
          titleX = T, titleY = T) %>%
    layout(legend = list(
      title=list(text='<b>Legend</b><br>Items can be toggled<br>',
                 font=list(color="#2d2d2d"))),
      plot_bgcolor="#FCFAF9") %>%
    animation_opts(transition = 0, redraw=T, frame=400) %>%
    animation_slider(currentvalue = list(prefix = "Number of pairs matched: ",
                                         xanchor = "left",
                                         font = list(color="#2d2d2d"))) %>%
    # style(font=list(color='#2d2d2d')) %>%
    suppressWarnings()
}


std.means.plot <- function(M1, M2, xvar, yvar, treatment){

  s.means.M1 <- std.means(M1, xvar, yvar, treatment)

  s.means.M1.cum <- accumulate_by(s.means.M1, ~n) %>%
    mutate(method="Method 1",
           txty= paste0(yvar, " SMD using ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.y,3)),
           txtx= paste0(xvar, " SMD using ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.x,3)))

  s.means.M2 <- std.means(M2, xvar, yvar, treatment)

  s.means.M2.cum <- accumulate_by(s.means.M2, ~n) %>%
    mutate(method="Method 2",
           txty= paste0(yvar, " SMD using ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.y,3)),
           txtx= paste0(xvar, " SMD using ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.x,3)))

  s.means.cum <- rbind(s.means.M1.cum, s.means.M2.cum)

  ys <- s.means.cum[c('full.smd.x', 'matched.smd.x',
                      'full.smd.y', 'matched.smd.y')]

  # target <- rep(0,nrow(s.means.cum))

  y.range <- axis.ranges(ys,0.1)

  y.max <- axis.ranges(ys,0) %>% max()
  a <- subplot.title(paste0("Plot 3: Standarised Mean Differences of ", xvar, " and ", yvar), 1, y.max)

  gg <- ggplot(s.means.cum, aes(x=n, frame=.data$frame, colour=.data$method)) +
    geom_line(aes(x=n, y=.data$matched.smd.x,
                  linetype=.data$method, group=.data$method, text=.data$txtx),
              size=1) +
    geom_line(aes(x=n, y=.data$matched.smd.y,
                  linetype=.data$method, group=.data$method, text=.data$txty),
              size=1) +
    scale_color_manual(values = c("Method 1"='#FED148', "Method 2"='#B241B4')) +
    theme_bw()

  gg <- ggplotly(gg, tooltip="text") %>%
    animation_opts(transition = 0, redraw=T, frame=400) %>% suppressWarnings()

  gg <- gg %>%
    add_ribbons(data=s.means.cum, x=~n, ymin=~target-0.15, ymax=~target+0.15,
                text="Ideal SMD Range", opacity=0.2, color=I("springgreen3"), inherit=F,
                hovertemplate = "%{text}<extra></extra>", name="Ideal SMD Range",
                legendgroup="target"
    ) %>%
    add_lines(data=s.means.cum, x=~n, legendgroup="raw",
              line=list(dash='dot', width=3), opacity = 0.6,
              y=~full.smd.x, inherit = F, name=paste(xvar, "Raw SMD"), text=xvar, color=I("#272727"),
              hovertemplate = "%{text} Raw SMD: %{y:>-0,.2f}<extra></extra>"
    ) %>%
    add_lines(data=s.means.cum, x=~n, legendgroup="raw",
              line=list(dash='dot', width=3), opacity = 0.6,
              y=~full.smd.y, inherit = F, name=paste(yvar, "Raw SMD"), text=yvar, color=I("#272727"),
              hovertemplate = "%{text} Raw SMD: %{y:>-0,.2f}<extra></extra>"
    ) %>%
    layout(showlegend = FALSE, annotations=a,
           xaxis = axis.settings("n observations"),
           yaxis = axis.settings("Standarised Mean Difference",
                                 y.range, F, 0.1)
    )

  return(gg)

}



estimates.plot <- function(M1, M2, outcome.f, te){

  M1.ests <- matching.ests(M1, outcome.f) %>%
    mutate(method="Method 1",
           lab=paste(.data$method,"-",.data$distance,.data$txt))

  M2.ests <- matching.ests(M2, outcome.f) %>%
    mutate(method="Method 2",
           lab=paste(.data$method,"-",.data$distance,.data$txt))

  comb.match.ests <- rbind(M1.ests, M2.ests)

  comb.match.ests.cum <- comb.match.ests %>%
    accumulate_by(~.data$idx)

  if(is.null(te)){
    agg.ests <- cbind(get.estimates(M1, outcome.f))
    ys <- c(comb.match.ests[['estimate']],agg.ests[1,2:4] %>% unlist())
    } else {
    agg.ests <- cbind(get.estimates(M1, outcome.f), te=te)
    ys <- c(comb.match.ests[['estimate']],agg.ests[1,2:5] %>% unlist())
    }

  # ys <- c(comb.match.ests[['estimate']],agg.ests[1,2:5] %>% unlist())

  y.range <- axis.ranges(ys,0.1)

  #annotation for multiplot
  y.max <- axis.ranges(ys,0) %>% max()
  a <- subplot.title("Plot 4: Estimates of the Treatment Effect", 1, y.max)

  gg <- ggplot(comb.match.ests.cum,
              aes(.data$idx, .data$estimate, frame=.data$frame, group=.data$method, text=.data$lab,
                  color=.data$method, linetype=.data$method
              )) + geom_line(size=1) +
          scale_color_manual(values = c("Method 1"='#FED148', "Method 2"='#B241B4')) +
          theme_bw()

  gg <- ggplotly(gg, tooltip = "text") %>%
    animation_opts(transition = 0, redraw=T, frame=400)

  if (!is.null(te)){
    gg <- gg %>%
      add_ribbons(data=agg.ests, x=~n, ymin=~te-0.2, ymax=~te+0.2, opacity=0.2,
                  color=I("springgreen3"), inherit=F, name="Estimate Target Range",
                  hovertemplate="Estimate Target Range (+/- 0.2)<extra></extra>",
                  legendgroup="target") %>%
      add_lines(data=agg.ests, x=~n, y=~te, inherit = F,
                name = "True Estimate", color=I("springgreen3"),
                line=list(dash='dot'), visible='legendonly',
                hovertemplate="True Estimate: %{y:>-0,.2f}<extra></extra>")
    }

  gg <- gg %>%
    add_lines(data=agg.ests, x=~n, y=~raw, inherit = F, line=list(dash='dot', width=3),
              name = "Raw Estimate", color=I("#272727"), legendgroup="raw", opacity = 0.6,
              hovertemplate="Raw Estimate: %{y:>-0,.2f}<extra></extra>") %>%
    add_lines(data=agg.ests, x=~n, y=~weighted, inherit = F, color=I("#009FB7"),
              name = "Weighted Estimate", line=list(dash='longdashdot', width=3),
              hovertemplate="Weighted Estimate: %{y:>-0,.2f}<extra></extra>",
              visible='legendonly') %>%
    add_lines(data=agg.ests, x=~n, y=~stratified, inherit = F, color=I("#696773"),
              name = "Stratified Estimate", line=list(dash='dashdot', width=3),
              hovertemplate="Stratified Estimate: %{y:>-0,.2f}<extra></extra>",
              visible='legendonly') %>%
    layout(showlegend = TRUE, annotations=a,
           xaxis = axis.settings("n observations"),
           yaxis = axis.settings("Estimate of Treatment Effect", y.range, F, 0.1))

  return(gg)
}


wt_lm <- function(f, data, wts) {
  ff <- as.formula(f)
  environment(ff) <- environment()
  lm(formula = ff, data = data,
     weights = wts)
}


get.estimates <- function(match.data, f) {

  treatment <- match.data$treatment

  d <- match.data$data

  #raw
  mod.v <- lm(f, d)
  raw <- coef(mod.v)[[treatment]]

  #weighted
  wtd <- cbind(d,wts=match.data$wt.ATE)
  mod.w <- wt_lm(f, wtd, wts = wtd$wts)
  weighted <- coef(mod.w)[[treatment]]

  #stratified
  max.strat <- max(as.numeric(match.data$stratification))

  stratified <- lapply(1:max.strat, function(x) {
    m <- lm(f, d[match.data$stratification==x,])

    return(coef(m)[[treatment]])
  }  ) %>% unlist() %>% mean(na.rm=T)

  a <- rbind(raw, weighted, stratified) %>% as.data.frame()

  colnames(a) <- c("estimate")

  a$method <- rownames(a)

  b <- a[,1] %>% t() %>% as.data.frame()

  colnames(b) <- rownames(a)

  b <- cbind(n=1:nrow(match.data$pairs),b)

  return(b)

}


matching.ests <- function(match.data, f){


  d <- accumulate_by(match.data$matched.data, ~ord)
  treatment <- match.data$treatment
  max.match <- max(d$frame)

  a <- lapply(1:max.match, function(x) {
    m <- lm(f, d[d$frame==x,])

    return(c(distance=match.data$distance,
             idx=x,
             estimate=coef(m)[[treatment]]))
  }  ) %>% bind_rows()

  a <- a %>% mutate(across(.data$idx:.data$estimate, as.numeric)) %>%
    mutate(txt=paste0("Estimate: ", round(.data$estimate,3)))

  return(a)

}
