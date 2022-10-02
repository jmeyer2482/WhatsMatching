#' simulations
#'
#' @description A function for simulating data
#'
#'
#'
#' @param sim a integer referencing the simulation - 1, 2 or 3
#'
#' @param te a number represnting the treatment effect, specify for all simulations.
#'
#' @param n the number of observations to populate for simulation 2 and 3
#' @param jitter controls the SD of the rnorm function that is used to add jitter to the outcome variable
#'
#' @param g1_min Simulation 2 - minimum value of for uniform distribution
#' @param g1_max Simulation 2 - maximum value of for uniform distribution
#' @param g2_shift_X1 Simulation 2 - how far to shift the X1 value for a control group
#' @param g2_shift_X2 Simulation 2 - how far to shift the X2 value for a control group
#'
#' @param mean1 Simulation 3 - X1 mean value for a normal distribution
#' @param mean2 Simulation 3 - X2 mean value for a normal distribution
#' @param sd1 Simulation 3 - X1 standard deviation value for a normal distribution
#' @param sd2 Simulation 3 - X2 standard deviation value for a normal distribution
#' @param rho Simulation 3 - correlation coefficient for relationship between X1 and X2
#' @param weight_t1 Simulation 3 - the weight of X1 on the treatment variable t
#' @param weight_t2 Simulation 3 - the weight of X2 on the treatment variable t
#' @param weight_y0 Simulation 3 - the unaffected value of the outcome y
#' @param weight_y1 Simulation 3 - the weight of X1 on the outcome variable y
#' @param weight_y2 Simulation 3 - the weight of X2 on the outcome variable y
#'
#' @return A dataframe with a 2 covariates, X1 and X2, and a treatment and outcome variable for the purposes of matching
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom stats runif rbinom rnorm
#' @importFrom MASS mvrnorm
#' @importFrom arm invlogit
#'
#'
#' @export
#'
#' @examples
#' # generate a simulated dataframe
#' create.sim.data(1,2)

create.sim.data <- function(
    sim = 1,

    te=2, jitter = 0.2,

    g1_min=0, g1_max=5, g2_shift_X1=1, g2_shift_X2=1,

    # Generating X1 and X2
    n = 200, mean1 = 0, mean2 = 0, sd1 = 1, sd2 = 1, rho = .3,
    # Generating t
    weight_t1 = 0.5, weight_t2 = 0.5,
    # Generating y
    weight_y0 = 2, weight_y1 = 1, weight_y2 = -1
){

  #simulation 1
  if (sim==1){

    t = c(rep(0, 25),rep(1, 25),rep(0, 25),rep(1, 25),rep(0, 50))

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

    y <- t*te + X1 + X2 + rnorm(n=length(t), mean=0, sd=jitter)

    t.char <- ifelse(t==0,"Control", "Treated")

    data <- list(t=t, Allocation=t.char, X1=X1, X2=X2, y=y) %>% as.data.frame()

    return(data)
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
    y <- te*t + X1 + X2 + rnorm(n=length(t), mean=0, sd=jitter)

    d <- list(t=t, Allocation=t.char, X1=X1, X2=X2, y=y) %>% as.data.frame()

    return(d)
  }

  #simulation 3
  if (sim==3) {

    means = c(mean1,mean2)

    varcovarMat <- matrix(c(sd1^2, sd1*sd2*rho, sd1*sd2*rho, sd2^2),2)

    for (i in 1:1000) {
      #create the data
      df <- data.frame(MASS::mvrnorm(n, mu = means, Sigma = varcovarMat)) %>%
        mutate(scaledX1 = scale(.data$X1), scaledX2 = scale(.data$X2),
           t = rbinom(n, 1,
                      prob = arm::invlogit(weight_t1*.data$scaledX1 + weight_t2*.data$scaledX2)),
           y = weight_y0 + te*t + weight_y1*X1 + weight_y2*X2 + rnorm(n=length(t), mean=0, sd=jitter),
           Allocation = ifelse(t==0,"Control","Treated")) %>%
        select(-c(.data$scaledX1,.data$scaledX2))

      #only return if there are no more than half of the units "treated"
      if(table(df$t)[[2]]<=(n/2))
        return(df[c("t", "Allocation", "X1", "X2", "y")])
    }

  }
}


