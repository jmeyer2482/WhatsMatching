#' simulations
#'
#' @description A function for simulating data
#'
#'
#'
#' @param sim a integer referencing the simulation - 1, 2, 3, or 4
#'
#' @param te a number representing the treatment effect, specify for all simulations.
#'
#' @param n the number of observations to populate for simulation 2, 3, and 4
#' @param jitter controls the SD of the rnorm function that is used to add jitter to the outcome variable
#'
#' @param g1_min Simulation 2 - minimum value of for uniform distribution
#' @param g1_max Simulation 2 - maximum value of for uniform distribution
#' @param g2_shift_X1 Simulation 2 - how far to shift the X1 value for a control group
#' @param g2_shift_X2 Simulation 2 - how far to shift the X2 value for a control group
#'
#' @param relX1 Simulation 3 - the causal relationship of X1 in the data. Can be 'mediator', 'confounder', 'collider', 'ancestor to y', 'ancestor to t'
#' @param relX2 Simulation 3 - the causal relationship of X2 in the data. Can be 'mediator', 'confounder', 'collider', 'ancestor to y', 'ancestor to t'
#'
#' @param mean1 Simulation 3 and 4 - X1 mean value for a normal distribution
#' @param mean2 Simulation 3 and 4 - X2 mean value for a normal distribution
#' @param sd1 Simulation 3 and 4 - X1 standard deviation value for a normal distribution
#' @param sd2 Simulation 3 and 4 - X2 standard deviation value for a normal distribution
#' @param rho Simulation 3 and 4 - correlation coefficient for relationship between X1 and X2
#' @param weight_t1 Simulation 3 and 4 - the weight of X1 on the treatment variable t
#' @param weight_t2 Simulation 3 and 4 - the weight of X2 on the treatment variable t
#' @param weight_y0 Simulation 3 and 4 - the unaffected value of the outcome y
#' @param weight_y1 Simulation 3 and 4 - the weight of X1 on the outcome variable y
#' @param weight_y2 Simulation 3 and 4 - the weight of X2 on the outcome variable y
#'
#'
#' @return A dataframe with a 2 covariates, X1 and X2, and a treatment and outcome variable for the purposes of matching
#'
#' @importFrom dplyr mutate %>% select case_when
#' @importFrom stats runif rbinom rnorm
#' @importFrom MASS mvrnorm
#' @importFrom arm invlogit
#' @importFrom tidyr expand_grid
#'
#'
#' @export
#'
#' @examples
#' # generate a simulated dataframe
#' d <- create.sim.data(1,2)
#' head(d)

create.sim.data <- function(
    sim = 1,

    te=2, jitter = 0,

    g1_min=0, g1_max=5, g2_shift_X1=1, g2_shift_X2=1,

    # Generating X1 and X2
    n = 200, mean1 = 50, mean2 = 40, sd1 = 5, sd2 = 5, rho = 0.2,
    # Generating t
    weight_t1 = .5, weight_t2 = .5,
    # Generating y
    weight_y0 = 20, weight_y1 = 1.2, weight_y2 = 0.8,

    #sim4 vars
    relX1="Mediator", relX2="Confounder"
){

  #simulation 1
  if (sim==1){

    n <- 150

    t <- c(rep(0, 25),rep(1, 25),rep(0, 25),rep(1, 25),rep(0, 50))

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

    d <- list(t=t, Allocation=t.char, X1=X1, X2=X2, y=y) %>% as.data.frame()

    # return(data)
  }

  #Simulation 2
  if (sim==2) {
    n.2 <- round(n/2,0)

    X1 <- c(runif(n.2, g1_min,g1_max),
            runif(n.2, g1_min+g2_shift_X1,g1_max+g2_shift_X1))

    X2 <- c(runif(n.2, g1_min,g1_max),
            runif(n.2, g1_min+g2_shift_X2,g1_max+g2_shift_X2))

    t <- c(rep(0,n.2),rep(1,n.2))
    t.char <- ifelse(t==0,"Control","Treated")
    y <- te*t + X1 + X2

    d <- list(t=t, Allocation=t.char, X1=X1, X2=X2, y=y) %>% as.data.frame()

    # return(d)
  }


  if (sim==3) {


    # Collider, Mediator, Confounder
    txt <- c("Mediator", "Collider", "Confounder", "Ancestor of t", "Ancestor of y")
    rel <- tidyr::expand_grid(txt,txt)
    colnames(rel) <- c("X1.rel","X2.rel")

    #simplify directions for easy assignment
    rel$X1d.t <- case_when(rel$X1.rel=="Collider"~F,
                            rel$X1.rel=="Mediator"~F,
                            rel$X1.rel=="Confounder"~T,
                            rel$X1.rel=="Ancestor of t"~T,
                            rel$X1.rel=="Ancestor of y"~F)
    rel$X2d.t <- case_when(rel$X2.rel=="Collider"~F,
                            rel$X2.rel=="Mediator"~F,
                            rel$X2.rel=="Confounder"~T,
                            rel$X2.rel=="Ancestor of t"~T,
                            rel$X2.rel=="Ancestor of y"~F)
    rel$X1d.y <- case_when(rel$X1.rel=="Collider"~F,
                            rel$X1.rel=="Mediator"~T,
                            rel$X1.rel=="Confounder"~T,
                            rel$X1.rel=="Ancestor of t"~F,
                            rel$X1.rel=="Ancestor of y"~T)
    rel$X2d.y <- case_when(rel$X2.rel=="Collider"~F,
                            rel$X2.rel=="Mediator"~T,
                            rel$X2.rel=="Confounder"~T,
                            rel$X2.rel=="Ancestor of t"~F,
                            rel$X2.rel=="Ancestor of y"~T)

    #assign directions
    rel$direction.t <- case_when(
      (rel$X1d.t==T & rel$X2d.t==T)~"both",
      (rel$X1d.t==T & rel$X2d.t==F)~"X1",
      (rel$X1d.t==F & rel$X2d.t==T)~"X2",
      (rel$X1d.t==F & rel$X2d.t==F)~"none")

    rel$direction.y <- case_when(
      (rel$X1d.y==T & rel$X2d.y==T)~"both",
      (rel$X1d.y==T & rel$X2d.y==F)~"X1",
      (rel$X1d.y==F & rel$X2d.y==T)~"X2",
      (rel$X1d.y==F & rel$X2d.y==F)~"none")

    options <- rel[rel$X1.rel==relX1 & rel$X2.rel==relX2,]
    dir.t <- options$direction.t
    dir.y <- options$direction.y

    #covariance matrix
    varcovarMat <- matrix(c(sd1^2, sd1*sd2*rho, sd1*sd2*rho, sd2^2),2)

    #simulate bivariate normal distribution for X1 and X2
    means <- c(mean1,mean2)
    d <- data.frame(MASS::mvrnorm(n, mu = means, Sigma = varcovarMat))

    #simulate independent treatment
    #set pr to 0.4 to ensure less treated units.
    d$t <- rbinom(n, 1, 0.4)

    #get weighted probabilities
    prX1 <- (max(d$X1)-d$X1)/(max(d$X1)-min(d$X1))*weight_t1
    prX2 <- (max(d$X2)-d$X2)/(max(d$X2)-min(d$X2))*weight_t2

    #update t based on causal relationships
    d$t <- switch(dir.t,
                   "X1"=rbinom(n, 1, prob = prX1),
                   "X2"=rbinom(n, 1, prob = prX2),
                   "both"=rbinom(n, 1, prob = (prX1+prX2)/2),
                   "none"=d$t)

    #simulate X1 and X2 if they are mediators
    #they are effected by t before calculating y
    d$X1 <- switch(relX1,
                   "Mediator"=d$X1+d$t*1.5,
                   d$X1)

    d$X2 <- switch(relX2,
                   "Mediator"=d$X2+d$t*1.5,
                   d$X2)

    #simluate y
    d$y <- switch(dir.y,
                   "X1"=weight_y0 + weight_y1*d$X1 + te*d$t,
                   "X2"=weight_y0 + weight_y2*d$X2 + te*d$t,
                   "both"=weight_y0 + weight_y1*d$X1 + weight_y2*d$X2 + te*d$t,
                   "none"=weight_y0 + te*d$t)

    #simulate X1 and X2 if they are colliders
    #they are effected by both t and y
    d$X1 <- switch(relX1,
                   "Collider"=d$X1+d$t+d$y,
                   d$X1)

    d$X2 <- switch(relX2,
                   "Collider"=d$X2+d$t+d$y,
                   d$X2)


    d$Allocation <- ifelse(d$t==1,"Treated", "Control")

    #reorder columns for consistency
    d <- d[c("t", "Allocation", "X1", "X2", "y")]

  }

  #simulation 4
  if (sim==4) {

    means <- c(mean1,mean2)

    varcovarMat <- matrix(c(sd1^2, sd1*sd2*rho, sd1*sd2*rho, sd2^2),2)

    #create the covariates X1 and X2
    d <- data.frame(MASS::mvrnorm(n, mu = means, Sigma = varcovarMat))# %>%

    #get weighted probabilities
    prX1 <- (max(d$X1)-d$X1)/(max(d$X1)-min(d$X1))*weight_t1
    prX2 <- (max(d$X2)-d$X2)/(max(d$X2)-min(d$X2))*weight_t2

    #create the data
    pr <- (prX1 + prX2)/2
    t <- rbinom(n, 1, prob=pr)
    y <- weight_y0 + te*t + weight_y1*d$X1 + weight_y2*d$X2
    Allocation <- ifelse(t==0,"Control","Treated")

    d <- list(t=t, Allocation=Allocation, X1=d$X1, X2=d$X2, y=y) %>%
        as.data.frame()

  }

  #add jitter
  #if jitter is 0 it adds 0, jitter 0 by default
  d$y <- d$y + rnorm(n,0,jitter)

  #rounded values
  d <- d %>% mutate(across(3:5, ~ round(.x,3)))


  return(d)
}


