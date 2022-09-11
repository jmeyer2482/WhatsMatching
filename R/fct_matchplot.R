#' matchplot
#'
#' @description A fct function
#'
#' @param match.data An object returned from the matched.data function
#' @param xvar A string naming the variable to go on the X axis
#' @param yvar A string naming the variable to go on the Y axis
#' @param multi Boolean value of whether the output will be part of a plotly subplot
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

matching.plot <- function(match.data, xvar, yvar, multi=F){

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
    pair.dist=match.data$matched.data[["pair.dist"]]) %>%
    mutate(Allocation=ifelse(t==0,"Control","Treated"),
           txt=paste0(.data$Allocation, "<br>Pair number: ", .data$subclass,
                      "<br>", xvar,": ", round(.data$x,3),
                      "<br>", yvar, ": ", round(.data$y,3))) %>%
    arrange(.data$pair.dist, .data$subclass) %>%
    mutate(Allocation=ifelse(t==0,"Control","Treated"),
           ord=(row_number(.data$pair.dist) + (row_number(.data$pair.dist)%%2))/2)

  d.ann <- m.data %>% mutate(distance = mdist, rframe=max(.data$ord)-.data$ord)

  d <- accumulate_by(m.data, ~ord) %>% mutate(distance = mdist)

  gp <- ggplot(d, aes(x=.data$x, y=.data$y, group=.data$subclass, frame=.data$rframe
                      , text=.data$txt)) +
    geom_point(aes(color=t, shape=t), size=3, stroke=1) +
    geom_line() +
    geom_point(data=d.ann, aes(color=t),
               size = 5, stroke = 2, shape=21, alpha=0.5) +
    scale_color_manual(values=c(0,1),
                       palette=colorRampPalette(c("red", "blue"))) +
    scale_shape_manual(values = c(2,6))

  pltly <- ggplotly(gp, tooltip = 'text')  %>%
    add_markers(data=all.data, x=~x, y=~y, text=~Allocation,
                color=~t, colors=c("pink","lightblue"),
                marker = list(alpha = 0.5, size = 10,line=list(width=1.5)),
                symbol= ~t, symbols = c('105','106'),
                hovertemplate = glue::glue("%{{text}}<br>{xvar}: %{{x:.3f}}<br>{yvar}: %{{y:.3f}}<extra></extra>"),
                inherit = F) %>%
    layout(showlegend = FALSE,
           xaxis = axis.settings(xvar),
           yaxis = axis.settings(yvar)) %>%
    animation_opts(transition = 0, redraw=T, frame=400)

  if (multi==F) {
    pltly <- pltly %>%
      layout(title=paste0("Matching via ", mdist)) %>%
      animation_slider(currentvalue =
                         list(prefix = "Number of pairs removed: ",
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

  dplyr::bind_rows(dats) %>% mutate(rframe=max(.data$frame)-.data$frame)

}


hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}

#standardise titles
axis.settings <- function(axis.text="Insert Title", axis.range=c(0,0),
                          auto.range = T){
  #add 10% buffer to each side
  axis.range <- ((axis.range[2]-axis.range[1])*.1) * c(-1,1) + axis.range

  #only use the axis range if auto.range is false.
  if(auto.range){
    list(
      title=list(font=list(family="Arial Black"),
                 text=axis.text,
                 standoff=0),
      autorange = TRUE,
      tickmode="auto"
    )} else {
      list(
        title=list(font=list(family="Arial Black"),
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

  m.data <- accumulate_by(m.data, ~ord) %>% mutate(rframe=max(.data$frame)-.data$frame)

  s.means <- cbind(n = 0:max(m.data$rframe),
                   full.smd.x = rep(smd::smd(data[[xvar]], data[[tvar]])$estimate,
                                    max(m.data$rframe)+1),
                   matched.smd.x = sapply(0:(max(m.data$rframe)),
                                  function(x)
                                     smd::smd(subset(m.data, m.data$rframe==x)[[xvar]],
                                              subset(m.data, m.data$rframe==x)[[tvar]])$estimate),
                   full.smd.y = rep(smd::smd(data[[yvar]],data[[tvar]])$estimate,
                                    max(m.data$rframe)+1),
                   matched.smd.y = sapply(0:max(m.data$rframe), function(x)
                     smd::smd(subset(m.data, m.data$rframe==x)[[yvar]],
                              subset(m.data, m.data$rframe==x)[[tvar]])$estimate)
  ) %>% as.data.frame()

  s.means <- cbind(distance=match.data$distance, s.means)

  return(s.means)

}
