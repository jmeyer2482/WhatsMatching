#' matches
#'
#' @md
#'
#' @description A function for simulating data
#'
#' @param f a two sided formula containing the treatment variable on the left side and the matching variables on the right
#' @param data a dataframe of the data that is to be used for matching
#' @param dist a matching distance, either "Propensity Score" or "Mahalanobis"
#' @param order a named order of "data", "smallest", "largest", "random" to perform matches and is only relevant if replace = TRUE
#' - "data" just uses the order the data is already in
#' - "smallest" matches by the smallest propensity score first
#' - "largest" matches by the largest propensity score first
#' - "random" matches on a random order every time
#' @param replace boolean value of whether to replace matched units or not
#'
#' @return A list of items that are generated from the matching process
#'
#' @details
#' The output contains the following items:
#' * $paired.data a dataframe of the matched pairs of data (prefix t. for treated group and c. for control group) with the calculated distance and order of matching
#' * $matched.data a dataframe with the matched data ready for analysis
#' * $data a dataframe of the original data
#' * $formula the formula used for matching
#' * $distance the distance used for matching
#' * $propensity a numeric vector of propensity scores in the same order as the original data
#' * $stratification a factored vector which has stratified the propensity score into groups (between 5 and 10)
#' * $wt.ATE a numeric vector of the average treatment effect based on the propensity score
#' * $wt.ATT a numeric vector of the average treatment effect on the treated based on the propensity score
#' * $wt.Matched a vector of weights based on how the matching occurred
#' * $d.m the 'DenseMatrix' output from the [optmatch::match_on()] function used for matching
#' * $pairs a dataframe with the row labels for each treated unit and it's matched control with the calculated distance between them
#' * $treatment the treatment variable
#' * $order the order the matches were conducted in
#' * $replacement was replacement used to match, TRUE or FALSE
#'
#'
#' @import dplyr
#' @importFrom stats glm lm quantile
#' @importFrom optmatch match_on
#'
#' @export
#'
#' @examples
#' #simulate some data
#' d <- create.sim.data(1,2)
#' #matching formula
#' f <- t~X1+X2
#' #create matched data object
#' M <- matched.data(f,d,"Propensity Score")
#'


##MATCHING FUNCTIONS

matched.data <- function(f, data, dist, order="data", replace=FALSE){

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
           d.rorder=row_number(-.data$d.order))

  matched <- cbind(id = pairs$treatment,
                   data[pairs$treatment,],
                   subclass = pairs$treatment,
                   pair.dist = pairs$dist,
                   m.order = as.numeric(rownames(pairs))
                   ) %>%
    rbind(cbind(id = pairs$control,
                data[pairs$control,],
                subclass=pairs$treatment,
                pair.dist = pairs$dist,
                m.order = as.numeric(rownames(pairs))))

  matched <- matched %>% arrange(.data$pair.dist, .data$m.order) %>%
    mutate(ord=(row_number(.data$pair.dist) + (row_number(.data$pair.dist)%%2))/2)

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
  p.scores <- stats::glm(formula = f, data = data, family = "binomial")

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

    a <- list(treatment = i,
              control = c.val,
              dist = min(d))

    #add the matched pair to the dataframe
    ids <- rbind(ids,a)

  }

  row.names(ids) <- 1:nrow(ids)

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
