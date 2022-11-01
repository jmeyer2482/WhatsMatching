#' matchplot
#'
#' @description A fct function
#'
#' @param match.data An object returned from the matched.data function
#' @param xvar A string naming the variable to go on the X axis
#' @param yvar A string naming the variable to go on the Y axis
#' @param multi Boolean value of whether the output will be part of a plotly subplot
#' @param plot.n a number indicating the number plot it is for the title
#'
#' @return A plotly object of the sequence of matches performed on the data
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom smd smd
#' @importFrom lazyeval f_eval
#'
#' @examples
#' #simulate some data
#' d <- create.sim.data(1,2)
#' #matching formula
#' f <- t~X1+X2
#' #create matched data object
#' M <- matched.data(f,d,"Propensity Score")
#'
#' #create an animated plot of the matched data
#' matching.plot(M, "X1", "X2", FALSE)
#'
#' @export

matching.plot <- function(match.data, xvar, yvar, multi=F, plot.n=1){

  mdist <- match.data$distance
  tvar <- match.data$treatment

  all.data <- data.frame(x=match.data$data[[xvar]],
                         y=match.data$data[[yvar]],
                         t=factor(match.data$data[[tvar]])) %>%
    mutate(Allocation=ifelse(t==0,"Control","Treated"))


  m.data <- data.frame(
    x=match.data$matched.data[[xvar]],
    y=match.data$matched.data[[yvar]],
    t=factor(match.data$matched.data[[tvar]]),
    subclass=match.data$matched.data[["subclass"]],
    pair.dist=match.data$matched.data[["pair.dist"]],
    ord=match.data$matched.data[["ord"]]) %>%
    mutate(Allocation=ifelse(t==0,"Control","Treated"),
           txt=paste0(.data$Allocation,
                      "<br>Matched using: ", mdist, " Distance",
                      "<br>Distance between pair: ", .data$pair.dist,
                      "<br>Pair number: ", .data$subclass,
                      "<br>", xvar,": ", .data$x,
                      "<br>", yvar, ": ", .data$y))
  # %>%
  #   arrange(.data$pair.dist, .data$subclass) %>%
  #   mutate(ord=(row_number(.data$pair.dist) + (row_number(.data$pair.dist)%%2))/2)


  p.data <- data.frame(
    t.x=match.data$paired.data[[paste0("t.",xvar)]],
    t.y=match.data$paired.data[[paste0("t.",yvar)]],
    c.x=match.data$paired.data[[paste0("c.",xvar)]],
    c.y=match.data$paired.data[[paste0("c.",yvar)]],
    t.t="Treated",
    c.t="Control",
    subclass=rownames(match.data$paired.data),
    pair.dist=match.data$paired.data[["dist"]],
    ord=match.data$paired.data[["d.order"]]) %>%
    mutate(txt.c=paste0("Control",
                        "<br>Matched using: ", mdist, " Distance",
                        "<br>Distance between pair: ", round(.data$pair.dist,3),
                        "<br>Pair number: ", .data$subclass,
                        "<br>", xvar,": ", .data$c.x,
                        "<br>", yvar, ": ", .data$c.y),
           txt.t=paste0("Treated",
                        "<br>Matched using: ", mdist, " Distance",
                        "<br>Distance between pair: ", round(.data$pair.dist,3),
                        "<br>Pair number: ", .data$subclass,
                        "<br>", xvar,": ", .data$t.x,
                        "<br>", yvar, ": ", .data$t.y))

  d.ann <- m.data %>% mutate(distance = mdist, frame=.data$ord)

  d <- accumulate_by(p.data, ~ord) %>% mutate(distance = mdist)

  control.col <-'#3772ff'
  treat.col <- '#df2935'

  # p <- ggplot(d, aes(x=.data$x, y=.data$y, group=.data$subclass,
  #                     frame=.data$frame, color=.data$Allocation)) +
  #   geom_point(aes(shape=.data$Allocation, text=.data$txt),
  #              size=3, stroke=0.8, alpha=0.8) +
  #   geom_line(color="black") +
  #   scale_shape_manual(values = c("Control"=2, "Treated"=6)) +
  #   scale_color_manual(values = c("Control"=control.col, "Treated"=treat.col)) +

  p <- ggplot(d, aes(x=.data$t.x, y=.data$t.y, frame=.data$frame)) +
    geom_segment(aes(x=.data$t.x, y=.data$t.y, xend=.data$c.x, yend=.data$c.y),
                 color="black") +
    geom_point(aes(x=.data$t.x, y=.data$t.y, text=.data$txt.t,
                   color=t.t, shape=t.t),
               size=3, stroke=0.8, alpha=0.8) +
    geom_point(aes(x=.data$c.x, y=.data$c.y, text=.data$txt.c,
                   color=c.t, shape=c.t),
               size=3, stroke=0.8, alpha=0.8) +
    scale_color_manual(name="Allocation",values=c(control.col,treat.col)) +
    scale_shape_manual(name="Allocation",values=c(2,6)) +
    theme_bw()
  # +
  #   theme(legend.title = element_blank(), legend.position='none')


  pltly <- ggplotly(p, tooltip = 'text')

  pltly <- pltly %>%
    add_markers(data=all.data, x=~x, y=~y,
                color=~t, colors=c(control.col, treat.col),name=~Allocation,
                marker = list(opacity = 0.2, size = 10,
                              line=list(width=1.5)),
                symbol= ~t, showlegend=FALSE,
                symbols = c("triangle-up-open","triangle-down-open"),
                hoverinfo = "none", inherit = F)

  #annotation for multiplot
  x.min <- axis.ranges(all.data$x,0.1) %>% min()
  y.max <- axis.ranges(all.data$y,0.1) %>% max()
  a <- subplot.title(paste0("Plot ", plot.n, ": Method ", plot.n,
                            " - Matching with ", mdist),
                     x.min, y.max)

  pltly <- pltly %>%
    add_markers(data=d.ann, x=~x, y=~y, color=~t, frame=~frame,
                hoverinfo="none", name=~Allocation, showlegend=FALSE,
                opacity=0.3, inherit = F,
                marker = list(size = 20, symbol="circle-open",
                              line = list(width = 8, color=~t,
                                          cmin=control.col, cmax=treat.col))
    ) %>%
    layout(annotations=a,
           xaxis = axis.settings(xvar),
           yaxis = axis.settings(yvar)) %>%
    animation_opts(transition = 0, redraw=T, frame=400)

  if (multi==F) {
    pltly <- pltly %>%
      layout(title=paste0("Matching via ", mdist), annotations=NULL) %>%
      animation_slider(currentvalue =
                         list(prefix = "Number of pairs matched: ",
                              xanchor = "left",
                              font = list(color="black"))
      )
  }

  return(pltly)
}

getLevels <- function (x) {
  if (is.factor(x))
    levels(x)
  else sort(unique(x))
}


accumulate_by <- function(dat, var) {

  var <- lazyeval::f_eval(var, dat)

  lvls <- getLevels(var)

  dats <- lapply(seq_along(-lvls), function(x) {

    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])

  })

  dplyr::bind_rows(dats) %>% mutate(rframe=.data$frame)#max(.data$frame)-.data$frame)

}

subplot.title <- function(title, pos.x, pos.y) { list(
  x = pos.x,
  y = pos.y,
  text = title,
  font=list(family="Arial Black"),
  xref = "x",
  yref = "y",
  showarrow = FALSE,
  xanchor="left",
  yanchor="bottom"
)}

axis.ranges <- function(range, margin=0.1){
  min.r <- min(range)
  max.r <- max(range)

  r <- ((max.r-min.r)*margin) * c(-1,1)

  r <- r + c(min.r,max.r)

  return(r)
}

#standardise titles
axis.settings <- function(axis.text="Insert Title", axis.range=c(0,1),
                          auto.range = T, margin=0.1){
  #add 10% buffer to each side
  axis.ranges(axis.range, margin=margin)
  # axis.range <- ((axis.range[2]-axis.range[1])*.1) * c(-1,1) + axis.range

  #only use the axis range if auto.range is false.
  if(auto.range){
    list(
      title=list(font=list(family="Arial Black",color="#2d2d2d"),
                 text=axis.text,
                 standoff=0),
      autorange = TRUE,
      tickmode="auto")
    } else {
      list(
        title=list(font=list(family="Arial Black",color="#2d2d2d"),
                   text=axis.text,
                   standoff=0),
        range = axis.range,
        tickmode="auto")
    }
}


std.means <- function(match.data, xvar, yvar, tvar) {

  data <- match.data$data

  m.data <- match.data$matched.data

  m.data <- m.data %>% arrange(.data$pair.dist, .data$subclass) %>%
    mutate(ord=(row_number(.data$pair.dist) + (row_number(.data$pair.dist)%%2))/2)

  m.data <- accumulate_by(m.data, ~ord)# %>% mutate(rframe=max(.data$frame)-.data$frame)

  s.means <- cbind(n = 1:max(m.data$frame), target=rep(0,max(m.data$frame)),
                   full.smd.x = rep(smd::smd(data[[xvar]], data[[tvar]])$estimate,
                                    max(m.data$frame)),
                   matched.smd.x = sapply(1:(max(m.data$frame)),
                                  function(x)
                                     smd::smd(subset(m.data, m.data$frame==x)[[xvar]],
                                              subset(m.data, m.data$frame==x)[[tvar]])$estimate),
                   full.smd.y = rep(smd::smd(data[[yvar]],data[[tvar]])$estimate,
                                    max(m.data$rframe)),
                   matched.smd.y = sapply(1:max(m.data$frame), function(x)
                     smd::smd(subset(m.data, m.data$frame==x)[[yvar]],
                              subset(m.data, m.data$frame==x)[[tvar]])$estimate)
  ) %>% as.data.frame()

  s.means <- cbind(distance=match.data$distance, s.means)

  return(s.means)

}
