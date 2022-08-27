###Functions for Matching Shiny app

create.sim1.data <- function(te=2){
  t = c(rep(0, 25),rep(1, 25),rep(0, 25),rep(1, 25),rep(0, 50))
  g = c(rep("Paired", 50), rep("Random", 50), rep("Controls", 50))

  px1c <- runif(25, min = -2, max = 2)
  px2c <- runif(25, min = -2, max = 2)
  px1t <- px1c + runif(25, min = -0.05, max = 0.05)
  px2t <- px2c + runif(25, min = -0.05, max = 0.05)

  rx1 <- runif(50, min = -2, max = 2)
  rx2 <- runif(50, min = -8, max = -4)

  cx1 <- runif(50, min = -6, max = -4)
  cx2 <- runif(50, min = -8, max = 2)

  X1 <- c(px1c, px1t, rx1, cx1)
  X2 <- c(px2c, px2t, rx2, cx2)

  y <- t*te + X1 + X2

  t.char <- ifelse(t==0,"Control", "Treated")

  data <- as.data.frame(list(X1=X1, X2=X2, t=t, t.char=t.char, g=g, y=y))

  return(data)
}

#simulate K and N paper data with options for moving data around
create.sim2.data <- function(te=2, g1_min=0, g1_max=5,
                             g2_shift_X1=1, g2_shift_X2=1) {

  X1 <- c(runif(100, g1_min,g1_max),
          runif(100, g1_min+g2_shift_X1,g1_max+g2_shift_X1))

  X2 <- c(runif(100, g1_min,g1_max),
          runif(100, g1_min+g2_shift_X2,g1_max+g2_shift_X2))

  t <- c(rep(0,100),rep(1,100))
  t.char <- ifelse(t==0,"Control","Treated")
  y <- te*t + X1 + X2

  d <- list(X1=X1,X2=X2,t=t, t.char=t.char, y=y) %>% as.data.frame()

  return(d)
}


# Generating X1 and X2
create.sim3.data <- function(

    # Generating X1 and X2
    n = 200,
    mean1 = 0,
    mean2 = 0,
    sd1 = 1,
    sd2 = 1,
    rho = .3, #must be between -1 and 1 inclusive.

    # Generating t
    weight_t1 = 0.5, #effect of X1 on t, -10 to 10
    weight_t2 = 0.5,  #effect of X2 on t, -10 to 10

    # Generating y
    weight_y0 = 2, #base value of y
    weight_y1 = 1, #effect of X1 on y, -10 to 10
    weight_y2 = -1, #effect of X2 on y, -10 to 10
    te = 2 # True treatment effect, -10 to 10
){

  means = c(mean1,mean2)

  varcovarMat <- matrix(c(sd1^2, sd1*sd2*rho, sd1*sd2*rho, sd2^2),2)

  for (i in 1:1000) {
    #create the data
    df <- data.frame(MASS::mvrnorm(n, mu = means, Sigma = varcovarMat)) %>%
      mutate(scaledX1 = scale(X1), scaledX2 = scale(X2),
             t = rbinom(n, 1,
                        prob = arm::invlogit(weight_t1*scaledX1 + weight_t2*scaledX2)),
               y = weight_y0 + te*t + weight_y1*X1 + weight_y2*X2,
               t.char = ifelse(t==0,"Control","Treated")) %>%
      select(-c(scaledX1,scaledX2))

    #only return if there are no more than half of the units "treated"
    if(table(df$t)[[2]]<=(n/2)) return(df)

  }
}

##MATCHING FUNCTIONS

#get a distance matrix based on the distance method of propensity or mahalanobis
distance.matrix <- function(f, data, dist = "Propensity Score"){
  #f
  #a two sided formula containing the treatment variable
  #on the left side and the matching variables on the right

  #data
  #the data that is to be used for matching

  #dist
  #a matching distance. Either "propensity" or "mahalanobis"

  #calculate the propensity score using logistic regression
  p.scores <- glm(formula = f, data = data, family = "binomial")

  #create a distance matrix using optmatch based on selected distance
  #the matrix returned is n x p where n is treated and p is control
  #each value is a distance between a treated and control unit
  if (dist=="Propensity Score"){
    dist <- optmatch::match_on(x=p.scores$fitted.values, z=p.scores$y)
  } else if (dist=="Mahalanobis"){
    dist <- optmatch::match_on(x = f, data = data,
                               method = "mahalanobis")
  }

  #return the distance matrix and the propensity scores used to create it
  #if the propensity score method was used.
  return(list(d.m=dist, p.scores=p.scores$fitted.values))
}

##This function returns pairs of row IDs for the matches of a n x p matrix of distances
dist.matches <- function(d.m, order=NULL, replace=FALSE){
  #d.m
  #a distance matrix

  #order
  #a vector of ordering which should be the same length as
  #nrows(d.m)

  #replace
  #boolean value of whether to replace matched units or not

  d.mat <- d.m@.Data

  t.names <- attributes(d.mat)$dimnames$treatment
  c.names <- attributes(d.mat)$dimnames$control

  #order by the data if no order specified
  if (is.null(order)) order <- seq_along(t.names)


  #initialise a dataframe for storing matched pairs
  ids <- list(treatment=character(),
              control=character(),
              dist=numeric()
  ) %>% as.data.frame()

  #loop over the distance matrix rows using the order specified
  for (i in t.names[order]) {
    #select the row
    #if replace is TRUE then only allow columns that have not been used
    if (replace) {
      d <- d.mat[i,]
    } else {
      d <- d.mat[i,!c.names %in% ids$control]
    }

    c.val <- names(which.min(d))
    if (length(d)==1) c.val <- c.names[!c.names %in% ids$control]

    #NOT REQUIRED
    # if (inc.equ) {
    #   nms <- names(which(d==min(d)))
    #   a <- list(treatment = rep(as.character(i), length(nms)),
    #             control = nms,
    #             dist = rep(min(d), length(nms)))
    # } else {
    a <- list(treatment = i,
              control = c.val,
              dist = min(d))
    # }

    #add the matched pair to the dataframe
    ids <- rbind(ids,a)

    row.names(ids) <- 1:nrow(ids)

  }

  #return all the matched pairs with the score they were matched on
  #dataframe with nrow(d.m) rows and 3 cols.
  return(ids)
}

#a function to arbitrarily assign stratification without error
#one issue is that if the cuts occur on the same point in more than
#one group then you can't use that cut point. This function works
# backward from the largest selected number of groups until the largest
#number of groups is achieveable with distinct breaks.
get.cuts <- function(n, p.s) {
  #n
  #number of preferred breaks

  #p.s
  #a vector of the propensity score

  for (i in n:1) {
    #create a vector of cuts
    Q <- quantile(p.s, prob=seq(from=0,to=1,by=1/i),na.rm=TRUE)

    #check the breaks are unique
    chk.lvls <- max(table(Q))

    #if the breaks are unique then make the cuts on the propensity score
    if(chk.lvls == 1) {
      #create a stratification variable.
      cuts <- cut(p.s, breaks = Q, labels = 1:i, include.lowest = TRUE)

      #return the cuts
      return(cuts)
    }
  }
}


#a function to calculate the propensity score with weighting and stratification variables
dists <- function(f, prop.s, data, cuts=10) {
  #f
  #a two sided formula containing the treatment variable
  #on the left side and the matching variables on the right

  #data
  #the data that is to be used for matching

  #get the response variable from the formula
  t = data[[all.vars(f)[1]]]

  #not currently required
  # #check the response is binary for logistic regression
  # if(all(as.numeric(data[t] == data[1,t]) %in% c(0,1))) {
  #     data[t] <- as.numeric(data[t] != data[1,t])
  # }

  #make an id column and add propensity scores to the data
  dt <- cbind(idx = row.names(data), data, prop.s)

  #add ATE weight to the data
  dt$ATE.wt <- t/prop.s + (1-t)/(1-prop.s)

  #add ATT weight to the data
  dt$ATT.wt <- t + (1-t)*prop.s/(1-prop.s)

  #create a stratification variable.
  dt$prop.st <- get.cuts(cuts, prop.s)

  #return the additional data
  return(dt)
}


matched.data <- function(f, data, dist, order="data", replace=FALSE){
  #f
  #a two sided formula containing the treatment variable
  #on the left side and the matching variables on the right

  #data
  #the data that is to be used for matching

  #dist
  #a matching distance. Either "propensity" or "mahalanobis"

  #order
  #a named order of "data", "smallest", "largest", "random"
  #used to perform matches and is only relevant if replace = TRUE
  #"data" just uses the order the data is already in
  #"smallest" matches by the smallest propensity score first
  #"largest" matches by the largest propensity score first
  #"random" matches on a random order every time

  #replace
  #boolean value of whether to replace matched units or not

  #get the distance matrix
  d.m <- distance.matrix(f=f,data=data,dist=dist)
  p.scores <- d.m$p.scores
  d.m <- d.m$d.m
  order.name <- order

  #create a distance vector to supply to dist.matches function
  order <- switch(order,
                  "largest" = order(p.scores[rownames(d.m)],
                                    decreasing = T),
                  "smallest" = order(p.scores[rownames(d.m)]),
                  "random" = sample(seq_along(d.m[,1]), nrow(d.m),
                                    replace = FALSE),
                  "data" = seq_along(rownames(d.m)))

  #get the pairs of matches using the dist.matches function
  pairs <- dist.matches(d.m=d.m, order=order,replace=replace)

  #create a dataframe that has all treated units with the matched control
  #units in the same row and the distance between them.
  paired.data <- cbind(distance = dist,
                       t=data[pairs$treatment,],
                       c=data[pairs$control,],
                       pairs)

  paired.data <- paired.data %>%
    mutate(d.order=row_number(dist),
           d.rorder=row_number(-d.order))

  matched <- cbind(id = pairs$treatment,
                   data[pairs$treatment,],
                   subclass=pairs$treatment,
                   pair.dist = pairs$dist) %>%
    rbind(cbind(id = pairs$control,
                data[pairs$control,],
                subclass=pairs$treatment,
                pair.dist = pairs$dist))

  matched <- matched %>% arrange(pair.dist, subclass) %>%
    mutate(ord=(row_number(pair.dist) + (row_number(pair.dist)%%2))/2)

  #create a dataframe with the id and the number of time the id was used
  #for weighting the matches appropriately
  m.wts <- table(c(pairs$treatment, pairs$control)) %>% as.data.frame()

  #get the propensity score, weighting values and stratification
  d.d <- dists(f = f, prop.s = p.scores, data = data)

  #add the matched weights to the data
  d.d <- left_join(d.d, m.wts, by = c('idx'='Var1'))


  return(list(paired.data=paired.data,
              matched.data=matched,
              data=data,
              formula=f,
              distance=dist,
              propensity=d.d$prop.s,
              stratification=d.d$prop.st,
              wt.ATT=d.d$ATT.wt,
              wt.ATE=d.d$ATE.wt,
              wt.Matched=d.d$Freq,
              d.m=d.m,
              pairs=pairs,
              treatment=all.vars(f)[1],
              order=order.name,
              replacement=replace)
  )
}


accumulate_by <- function(dat, var) {

  var <- lazyeval::f_eval(var, dat)

  lvls <- plotly:::getLevels(var)

  dats <- lapply(seq_along(-lvls), function(x) {

    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])

  })

  dplyr::bind_rows(dats) %>% mutate(rframe=max(frame)-frame)

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
                 standoff=5),
      autorange = TRUE,
      tickmode="auto",
      margin=list()
    )} else {
      list(
        title=list(font=list(family="Arial Black"),
                   text=axis.text,
                   standoff=5),
        range = axis.range,
        tickmode="auto",
        margin=list())
    }
}


scatterlines <- function(data, xvar, yvar, facet, full.data, treat){

  ggplot(full.data, aes(x=!!sym(xvar), y=!!sym(yvar))) +
    geom_point(aes(color=factor(!!sym(treat))), shape="X") +
    geom_line(data,aes(group=subclass)) +
    geom_point(data,aes(age, height, color=factor(smoke)))


  gp <- ggplotly(p)

  gp <- gp %>%
    animation_opts(transition = 0, redraw=T, frame=120) %>%
    animation_slider(currentvalue =
                       list(prefix = "Number of obs removed: "))

  return(gp)
}


matching.plot <- function(match.data, xvar, yvar, multi=F){

  mdist <- match.data$distance
  tvar <- match.data$treatment

  all.data <- data.frame(x=match.data$data[[xvar]],
                         y=match.data$data[[yvar]],
                         t=factor(match.data$data[[tvar]])) %>%
    mutate(t.char=ifelse(t==0,"Control","Treated"))


  m.data <- data.frame(
    x=match.data$matched.data[[xvar]],
    y=match.data$matched.data[[yvar]],
    t=factor(match.data$matched.data[[tvar]]),
    subclass=match.data$matched.data[["subclass"]],
    pair.dist=match.data$matched.data[["pair.dist"]]) %>%
    mutate(t.char=ifelse(t==0,"Control","Treated"),
           txt=paste0(t.char, "<br>Pair number: ", subclass,
                       "<br>", xvar,": ", round(x,3),
                       "<br>", yvar, ": ", round(y,3))) %>%
    arrange(pair.dist, subclass) %>%
    mutate(t.char=ifelse(t==0,"Control","Treated"),
           ord=(row_number(pair.dist) + (row_number(pair.dist)%%2))/2)

  d.ann <- m.data %>% mutate(distance = mdist, rframe=max(ord)-ord)

  d <- accumulate_by(m.data, ~ord) %>% mutate(distance = mdist)

  gp <- ggplot(d, aes(x=x, y=y, group=subclass, frame=rframe
                      , text=txt)) +
    geom_point(aes(color=t, shape=t), size=3, stroke=1) +
    geom_line() +
    geom_point(data=d.ann, aes(color=t),
               size = 5, stroke = 2, shape=21, alpha=0.5) +
    scale_color_manual(values=c(0,1),
                       palette=colorRampPalette(c("red", "blue"))) +
    scale_shape_manual(values = c(2,6))

  pltly <- ggplotly(gp, tooltip = 'text')  %>%
    add_markers(data=all.data, x=~x, y=~y, text=~t.char,
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


std.means <- function(match.data, xvar, yvar, tvar) {

  data <- match.data$data

  m.data <- match.data$matched.data

  m.data <- m.data %>% arrange(pair.dist, subclass) %>%
    mutate(ord=(row_number(pair.dist) + (row_number(pair.dist)%%2))/2)

  m.data <- accumulate_by(m.data, ~ord) %>% mutate(rframe=max(frame)-frame)

  s.means <- cbind(n = 0:max(m.data$rframe),
                   full.smd.x = rep(smd::smd(data[[xvar]], data[[tvar]])$estimate,
                                    max(m.data$rframe)+1),
                   matched.smd.x = sapply(0:(max(m.data$rframe)), function(x)
                     smd::smd(subset(m.data, rframe==x)[[xvar]],
                              subset(m.data, rframe==x)[[tvar]])$estimate),
                   full.smd.y = rep(smd::smd(data[[yvar]],data[[tvar]])$estimate,
                                    max(m.data$rframe)+1),
                   matched.smd.y = sapply(0:max(m.data$rframe), function(x)
                     smd::smd(subset(m.data, rframe==x)[[yvar]],
                              subset(m.data, rframe==x)[[tvar]])$estimate)
  ) %>% as.data.frame()

  s.means <- cbind(distance=match.data$distance, s.means)

  return(s.means)

}




# covar.plot <- function(match.data, xvar, yvar, treatment){
#
#
#
#   s.means.p <- std.means(prop.matches, xvar, yvar, tvar)
#
#   s.means.p.cum <- accumulate_by(s.means.p, ~n) %>% select(-rframe) %>%  mutate(rframe=frame)
#
#   s.means.m <- std.means(maha.matches, xvar, yvar, tvar)
#
#   s.means.m.cum <- s.means.m %>% accumulate_by(~n) %>% select(-rframe) %>%  mutate(rframe=frame)
#
#   s.means.cum <- rbind(s.means.m.cum, s.means.p.cum)
#
#
#
#   p <- ggplot(s.means.cum, aes(x=n, frame=rframe)) +
#     geom_line(aes(x=n, y=matched.smd.x, color=distance)) +
#     geom_line(aes(x=n, y=matched.smd.y, color=distance))
#
#   p <- ggplotly(p) %>%
#     animation_opts(transition = 0, redraw=T, frame=400) %>%
#     layout(showlegend = FALSE,
#            xaxis = list(title=xvar,
#                         autorange = TRUE,
#                         tickmode="auto"),
#            yaxis = list(title=yvar,
#                         autorange = TRUE,
#                         tickmode="auto"))
#   return(p)
# }


get.estimates <- function(match.data, outcome) {

  treatment <- match.data$treatment

  f <- paste0(outcome, " ~ ", match.data$treatment) %>% as.formula()
  d <- match.data$data

  #raw
  mod.v <- lm(f, d)
  raw <- cbind(estimate=coef(mod.v),confint(mod.v))[treatment,]

  #weighted
  mod.w <- lm(f, d, weights = match.data$wt.ATT)
  weighted <- cbind(estimate=coef(mod.w),
                    confint(mod.w))[treatment,]

  #stratified
  max.strat <- max(as.numeric(match.data$stratification))

  stratified <- lapply(1:max.strat, function(x) {
    m <- lm(f, d[match.data$stratification==x,])

    return(cbind(estimate=coef(m),
                 confint(m))[treatment,])
  }  ) %>% bind_rows() %>% colMeans(na.rm=T)

  a <- rbind(raw, weighted, stratified) %>% as.data.frame()

  colnames(a) <- c("estimate", "CI_2.5", "CI_97.5")

  a$method <- rownames(a)

  b <- a[,1] %>% t() %>% as.data.frame()

  colnames(b) <- rownames(a)

  b <- cbind(n=1:(nrow(match.data$pairs)-1),b)

  return(b)

}


matching.ests <- function(match.data, outcome){

  f <- paste0(outcome, " ~ ", match.data$treatment) %>% as.formula()


  d <- accumulate_by(match.data$matched.data, ~ord) %>%
    mutate(rframe=max(frame)-frame)
  treatment <- match.data$treatment
  max.match <- max(d$rframe)

  a <- lapply(1:max.match, function(x) {
    m <- lm(f, d[d$rframe==x,])

    return(cbind(distance=match.data$distance,
                 idx=x,
                 estimate=coef(m),
                 confint(m))[treatment,])
  }  ) %>% bind_rows()

  colnames(a) <- c("distance", "idx", "estimate", "CI_2.5", "CI_97.5")

  a <- a %>% mutate(across(idx:CI_97.5, as.numeric)) %>%
    mutate(txt=paste0("Estimate: ", round(estimate,3)))

  for (x in 1:nrow(a)) {
    if (is.na(a[x,4]))
    {a[x,4] <- a[x,3]}

    if (is.na(a[x,5]))
    {a[x,5] <- a[x,3]}
  }

  return(a)

}


combined.plot <- function(xvar, yvar, outcome, M1, M2, te=NULL){

  # m.matches <- matched.data(f=f, data=d, dist="mahalanobis",
  #                           order=ord, replace=rep)
  # p.matches <- matched.data(f=f, data=d, dist="propensity score",
  #                           order=ord, replace=rep)
  treatment <- M1$treatment


  gg1 <- matching.plot(M1, xvar, yvar, multi=T)

  gg2 <- matching.plot(M2, xvar, yvar, multi=T)



  s.means.M1 <- std.means(M1, xvar, yvar, treatment)

  s.means.M1.cum <- accumulate_by(s.means.M1, ~n) %>%
    select(-rframe) %>%
    mutate(rframe=frame,
           method="1",
           txt= paste0(yvar, " SMD using Method ",
                       method," - ", distance, " : ",
                       round(matched.smd.y,3)))

  s.means.M2 <- std.means(M2, xvar, yvar, treatment)

  s.means.M2.cum <- accumulate_by(s.means.M2, ~n) %>%
    select(-rframe) %>%
    mutate(rframe=frame,
           method="2",
           txt= paste0(yvar, " SMD using Method ",
                        method," - ", distance, " : ",
                       round(matched.smd.y,3)))

  s.means.cum <- rbind(s.means.M1.cum, s.means.M2.cum)

  ys <- s.means.cum[c('full.smd.x', 'matched.smd.x',
                      'full.smd.y', 'matched.smd.y')]

  y.range <- c(min(ys), max(ys))

  gg3 <- ggplot(s.means.cum, aes(x=n, frame=rframe, colour=method,
                                 text=txt)) +
    geom_line(aes(x=n, y=matched.smd.x,
                  linetype=method, group=method)) +
    geom_line(aes(x=n, y=matched.smd.y,
                  linetype=method, group=method))

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

  M1.ests <- matching.ests(M1, outcome) %>%
    mutate(method="1",#paste("Method 1 -", distance),
            lab=paste("Method",method,"-",distance,txt))

  M2.ests <- matching.ests(M2, outcome) %>%
    mutate(method="2",#paste("Method 2 -", distance),
           lab=paste("Method",method,"-",distance,txt))

  comb.match.ests <- rbind(M1.ests, M2.ests)
    # matching.ests(M1, outcome) %>%
    #   mutate(method=paste0("Method 1 - ",distance)),
    # matching.ests(M2, outcome) %>%
    #   mutate(method=paste0("Method 2 - ",distance),
    #          txt=paste0(method, " Estimate: ", round(estimate,3)))
    # )

  comb.match.ests.cum <- comb.match.ests %>%
    accumulate_by(~idx-1) %>% select(-rframe) %>%
    mutate(rframe=frame)

  agg.ests <- cbind(get.estimates(M1, outcome), te=te)

  ys <- c(comb.match.ests[['estimate']],agg.ests[1,2:4] %>% unlist())

  y.range <- c(min(ys),max(ys))

  gg4 <- ggplot(comb.match.ests.cum,
         aes(idx, estimate, frame=rframe, group=method, text=lab,
             color=method, linetype=method
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
          widths = c(0.47,0.47), heights = c(0.47,0.47),
          margin = 0.015, shareX = F, shareY = F, nrows = 2,#) %>%#,
         titleX = T, titleY = T) %>%
    # layout(annotations=annotations) %>%
    animation_opts(transition = 0, redraw=T, frame=400) %>%
    animation_slider(currentvalue = list(prefix = "Number of pairs removed: ",
                                         xanchor = "left",
                                         font = list(color="black")))
}



