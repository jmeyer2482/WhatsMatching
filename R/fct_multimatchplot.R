#' multimatchplot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom stats coef
#'
#'
#' @noRd

combined.plot <- function(xvar, yvar, M1, M2, te=NULL,
                          outcome.f=y~t){

  # m.matches <- matched.data(f=f, data=d, dist="mahalanobis",
  #                           order=ord, replace=rep)
  # p.matches <- matched.data(f=f, data=d, dist="propensity score",
  #                           order=ord, replace=rep)
  treatment <- M1$treatment

  outcome <- all.vars(outcome.f)[1]


  gg1 <- matching.plot(M1, xvar, yvar, multi=T)

  gg2 <- matching.plot(M2, xvar, yvar, multi=T)



  s.means.M1 <- std.means(M1, xvar, yvar, treatment)

  s.means.M1.cum <- accumulate_by(s.means.M1, ~n) %>%
    select(-.data$rframe) %>%
    mutate(rframe=.data$frame,
           method="1",
           txty= paste0(yvar, " SMD using Method ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.y,3)),
           txtx= paste0(xvar, " SMD using Method ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.x,3)))

  s.means.M2 <- std.means(M2, xvar, yvar, treatment)

  s.means.M2.cum <- accumulate_by(s.means.M2, ~n) %>%
    select(-.data$rframe) %>%
    mutate(rframe=.data$frame,
           method="2",
           txty= paste0(yvar, " SMD using Method ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.y,3)),
           txtx= paste0(xvar, " SMD using Method ",
                        .data$method," - ", .data$distance, " : ",
                        round(.data$matched.smd.x,3)))

  s.means.cum <- rbind(s.means.M1.cum, s.means.M2.cum)

  ys <- s.means.cum[c('full.smd.x', 'matched.smd.x',
                      'full.smd.y', 'matched.smd.y')]

  y.range <- c(min(ys), max(ys))

  gg3 <- ggplot(s.means.cum, aes(x=n, frame=.data$rframe, colour=.data$method)) +
    geom_line(aes(x=n, y=.data$matched.smd.x,
                  linetype=.data$method, group=.data$method, text=.data$txtx)) +
    geom_line(aes(x=n, y=.data$matched.smd.y,
                  linetype=.data$method, group=.data$method, text=.data$txty))

  gg3 <- ggplotly(gg3, tooltip="text") %>%
    animation_opts(transition = 0, redraw=T, frame=400)

  gg3 <- gg3 %>% add_lines(data=s.means.cum, x=~n,
                           y=~full.smd.x, inherit = F, name=xvar, text=xvar,
                           hovertemplate = "%{text} Raw SMD: %{y:>-0,.2f}<extra></extra>"
  ) %>%
    add_lines(data=s.means.cum, x=~n,
              y=~full.smd.y, inherit = F, name=yvar, text=yvar,
              hovertemplate = "%{text} Raw SMD: %{y:>-0,.2f}<extra></extra>"
    ) %>%
    layout(showlegend = FALSE,
           xaxis = axis.settings("n observations"),
           yaxis = axis.settings("Standarised Mean Difference",
                                 y.range, F)
    )

  M1.ests <- matching.ests(M1, outcome.f) %>%
    mutate(method="1",#paste("Method 1 -", distance),
           lab=paste("Method",.data$method,"-",.data$distance,.data$txt))

  M2.ests <- matching.ests(M2, outcome.f) %>%
    mutate(method="2",#paste("Method 2 -", distance),
           lab=paste("Method",.data$method,"-",.data$distance,.data$txt))

  comb.match.ests <- rbind(M1.ests, M2.ests)
  # matching.ests(M1, outcome) %>%
  #   mutate(method=paste0("Method 1 - ",distance)),
  # matching.ests(M2, outcome) %>%
  #   mutate(method=paste0("Method 2 - ",distance),
  #          txt=paste0(method, " Estimate: ", round(estimate,3)))
  # )

  comb.match.ests.cum <- comb.match.ests %>%
    accumulate_by(~.data$idx-1) %>% select(-.data$rframe) %>%
    mutate(rframe=.data$frame)

  agg.ests <- cbind(get.estimates(M1, outcome.f), te=te)

  ys <- c(comb.match.ests[['estimate']],agg.ests[1,2:4] %>% unlist())

  y.range <- c(min(ys),max(ys))

  gg4 <- ggplot(comb.match.ests.cum,
                aes(.data$idx, .data$estimate, frame=.data$rframe, group=.data$method, text=.data$lab,
                    color=.data$method, linetype=.data$method
                )) + geom_line(size=1)

  gg4 <- ggplotly(gg4, tooltip = "text") %>%
    animation_opts(transition = 0, redraw=T, frame=400)

  gg4 <- gg4 %>%
    add_lines(data=agg.ests, x=~n, y=~raw, inherit = F,
              name = "Raw Estimate",
              hovertemplate="Raw Estimate: %{y:>-0,.2f}<extra></extra>") %>%
    add_lines(data=agg.ests, x=~n, y=~weighted, inherit = F,
              name = "Weighted Estimate",
              hovertemplate="Weighted Estimate: %{y:>-0,.2f}<extra></extra>") %>%
    add_lines(data=agg.ests, x=~n, y=~stratified, inherit = F,
              name = "Stratified Estimate",
              hovertemplate="Stratified Estimate: %{y:>-0,.2f}<extra></extra>") %>%
    add_lines(data=agg.ests, x=~n, y=~te, inherit = F,
              name = "True Estimate",
              hovertemplate="True Estimate: %{y:>-0,.2f}<extra></extra>") %>%
    layout(showlegend = FALSE,
           xaxis = axis.settings("n observations"),
           yaxis = axis.settings("Estimate of Treatment Effect", y.range, F)
    )

  subplot(gg1, gg2, gg3, gg4,
          widths = c(0.45,0.45), heights = c(0.45,0.45),
          margin = 0.05, shareX = F, shareY = F, nrows = 2,#) %>%#,
          titleX = T, titleY = T) %>%
    # layout(annotations=annotations) %>%
    animation_opts(transition = 0, redraw=T, frame=400) %>%
    animation_slider(currentvalue = list(prefix = "Number of pairs removed: ",
                                         xanchor = "left",
                                         font = list(color="black")))
}


wt_lm <- function(f, data, wts) {
  ff <- as.formula(f)
  environment(ff) <- environment()
  lm(formula = ff, data = data,
     weights = wts)
}


get.estimates <- function(match.data, f) {

  treatment <- match.data$treatment

  # f <- paste0(outcome, " ~ ", match.data$treatment) %>% as.formula()
  d <- match.data$data

  #raw
  mod.v <- lm(f, d)
  raw <- coef(mod.v)[[treatment]]
  # cbind(estimate=coef(mod.v),confint(mod.v))[treatment,]

  #weighted
  wtd <- cbind(d,wts=match.data$wt.ATT)
  # print(wts)
  # print(f)
  # print(d)
  mod.w <- wt_lm(f, wtd, wts = wtd$wts)
  weighted <- coef(mod.w)[[treatment]]
  # cbind(estimate=coef(mod.w),
  #                   confint(mod.w))[treatment,]

  #stratified
  max.strat <- max(as.numeric(match.data$stratification))

  stratified <- lapply(1:max.strat, function(x) {
    m <- lm(f, d[match.data$stratification==x,])

    return(coef(m)[[treatment]])
    # confint(m))[treatment,])
  }  ) %>% unlist() %>% mean(na.rm=T)

  a <- rbind(raw, weighted, stratified) %>% as.data.frame()

  colnames(a) <- c("estimate")#, "CI_2.5", "CI_97.5")

  a$method <- rownames(a)

  b <- a[,1] %>% t() %>% as.data.frame()

  colnames(b) <- rownames(a)

  b <- cbind(n=1:(nrow(match.data$pairs)-1),b)

  return(b)

}


matching.ests <- function(match.data, f){

  # f <- paste0(outcome, " ~ ", match.data$treatment) %>% as.formula()


  d <- accumulate_by(match.data$matched.data, ~ord) %>%
    mutate(rframe=max(.data$frame)-.data$frame)
  treatment <- match.data$treatment
  max.match <- max(d$rframe)

  a <- lapply(1:max.match, function(x) {
    m <- lm(f, d[d$rframe==x,])

    return(c(distance=match.data$distance,
             idx=x,
             estimate=coef(m)[[treatment]]))
    # ,
    #       confint(m))[treatment,])
  }  ) %>% bind_rows()

  # colnames(a) <- c("distance", "idx", "estimate", "CI_2.5", "CI_97.5")

  a <- a %>% mutate(across(.data$idx:.data$estimate, as.numeric)) %>%
    mutate(txt=paste0("Estimate: ", round(.data$estimate,3)))

  # for (x in 1:nrow(a)) {
  #   if (is.na(a[x,4]))
  #   {a[x,4] <- a[x,3]}
  #
  #   if (is.na(a[x,5]))
  #   {a[x,5] <- a[x,3]}
  # }

  return(a)

}
