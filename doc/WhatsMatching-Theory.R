## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7,
  echo = FALSE,
  warning = FALSE,
  message = FALSE)

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
devtools::load_all()

#function to plot matches
plot.matches <- function(md, title, lims=NULL){
  
    x.var <- all.vars(md$formula)[2]
    y.var <- all.vars(md$formula)[3]
  
    p <- ggplot(md$matched.data, aes(.data[[x.var]], .data[[y.var]], group=subclass)) +
      geom_point(data=md$data, aes(.data[[x.var]], .data[[y.var]]), colour="black", alpha=0.3,
                 inherit.aes = F, size=3) + 
      geom_point(aes(color=factor(t)), alpha=0.6, size=5) + 
      geom_line(color="black") + 
      labs(title=title, color="Treated")

    if(!is.null(lims)) p <- p + xlim(lims[1]) + ylim(lims[2])
    
    p
}

#number of obs
n <- 100

#data frame with treatment and random covariates
d <- list(t=c(rep(0,n*.6), rep(1,n*.4)),
          X1=rnorm(n, 50, 5), 
          X2=rnorm(n, 40, 4)
          ) %>% as.data.frame()

m.norep <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis", replace = F)
m.rep <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis", replace = T)

ggpubr::ggarrange(
plot.matches(m.norep, "Mahalanobis Matches without Replacement"),
plot.matches(m.rep, "Mahalanobis Matches with Replacement"), common.legend = T
)


## -----------------------------------------------------------------------------

m.data <- WhatsMatching::matched.data(t~X1+X2, d, "Propensity Score", replace = F, order = "data")
m.small <- WhatsMatching::matched.data(t~X1+X2, d, "Propensity Score", replace = F, order = "smallest")
m.large <- WhatsMatching::matched.data(t~X1+X2, d, "Propensity Score", replace = F, order = "largest")
m.rand <- WhatsMatching::matched.data(t~X1+X2, d, "Propensity Score", replace = F, order = "random")


m.PO <- ggplot(m.data$matched.data, aes(X1, X2, group=subclass)) +
  geom_point(data=m.data$data, aes(X1, X2,fill=factor(t)),alpha=0.5, shape=21,
             inherit.aes = F, size=3) + 
  # geom_point(aes(color=factor(t)), alpha=0.6, size=5) + 
  geom_line(aes(linetype="data", color="data"), lwd=1, alpha=0.6) + 
  geom_line(data=m.small$matched.data, aes(linetype="smallest",color="smallest"), lwd=1, alpha=0.6) +
  geom_line(data=m.large$matched.data, aes(linetype="largest",color="largest"), lwd=1, alpha=0.6) +
  geom_line(data=m.rand$matched.data, aes(linetype="random",color="random"), lwd=1, alpha=0.6) +
  labs(title="Propensity score matching using different match ordering", fill="Treated") +
  scale_linetype_manual(name="Ordering", values = c("data"=1, "smallest"=2, "largest"=3, "random"=4)) +
  scale_color_manual(name="Ordering", values = c("data"="black", "smallest"="blue", "largest"="darkgreen", "random"="red")) 
  
m.data <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis", replace = F, order = "data")
m.small <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis", replace = F, order = "smallest")
m.large <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis", replace = F, order = "largest")
m.rand <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis", replace = F, order = "random")


m.MO <- ggplot(m.data$matched.data, aes(X1, X2, group=subclass)) +
  geom_point(data=m.data$data, aes(X1, X2,fill=factor(t)),alpha=0.5, shape=21,
             inherit.aes = F, size=3) + 
  # geom_point(aes(color=factor(t)), alpha=0.6, size=5) + 
  geom_line(aes(linetype="data", color="data"), lwd=1, alpha=0.6) + 
  geom_line(data=m.small$matched.data, aes(linetype="smallest",color="smallest"), lwd=1, alpha=0.6) +
  geom_line(data=m.large$matched.data, aes(linetype="largest",color="largest"), lwd=1, alpha=0.6) +
  geom_line(data=m.rand$matched.data, aes(linetype="random",color="random"), lwd=1, alpha=0.6) +
  labs(title="Propensity score matching using different match ordering", fill="Treated") +
  scale_linetype_manual(name="Ordering", values = c("data"=1, "smallest"=2, "largest"=3, "random"=4)) +
  scale_color_manual(name="Ordering", values = c("data"="black", "smallest"="blue", "largest"="darkgreen", "random"="red")) 






ggpubr::ggarrange(
  m.PO, m.MO, common.legend = T, legend = "right"
)



## ----simulated data-----------------------------------------------------------




#treatment has a mild effect on one covariate (1/2 std dev)
d$X1.mild <- d$t*2.5 + d$X1

#treatment has a strong effect on one covariate (3 std dev)
d$X1.strong <- d$t*15 + d$X1

#treatment has opposite effects on covariates (mild)
d$X2.mild <- d$t*-2 + d$X2

#treatment has opposite effects on covariates (strong)
d$X2.strong <- d$t*-12 + d$X2





## -----------------------------------------------------------------------------
#random, no treatment effect
psm.1 <- WhatsMatching::matched.data(t~X1+X2, d, "Propensity Score")
mdm.1 <- WhatsMatching::matched.data(t~X1+X2, d, "Mahalanobis")

psp.1 <- plot.matches(psm.1, "Propensity Score Matches - Random Data")
mdp.1 <- plot.matches(mdm.1, "Mahalanobis Distance Matches - Random Data")


psm.2 <- WhatsMatching::matched.data(t~X1.mild+X2, d, "Propensity Score")
mdm.2 <- WhatsMatching::matched.data(t~X1.mild+X2, d, "Mahalanobis")

psp.2 <- plot.matches(psm.2, "Propensity Score Matches - Mild treatment effect on X1")
mdp.2 <- plot.matches(mdm.2, "Mahalanobis Distance Matches - Mild treatment effect on X1")


psm.3 <- WhatsMatching::matched.data(t~X1.strong+X2, d, "Propensity Score")
mdm.3 <- WhatsMatching::matched.data(t~X1.strong+X2, d, "Mahalanobis")

psp.3 <- plot.matches(psm.3, "Propensity Score Matches - Strong treatment effect on X1")
mdp.3 <- plot.matches(mdm.3, "Mahalanobis Distance Matches - Strong treatment effect on X1")


psm.4 <- WhatsMatching::matched.data(t~X1.mild+X2.mild, d, "Propensity Score")
mdm.4 <- WhatsMatching::matched.data(t~X1.mild+X2.mild, d, "Mahalanobis")

psp.4 <- plot.matches(psm.4, "Propensity Score Matches - Mild treatment effect on X1 and X2")
mdp.4 <- plot.matches(mdm.4, "Mahalanobis Distance Matches - Mild treatment effect on X1 and X2")


psm.5 <- WhatsMatching::matched.data(t~X1.strong+X2.strong, d, "Propensity Score")
mdm.5 <- WhatsMatching::matched.data(t~X1.strong+X2.strong, d, "Mahalanobis")

psp.5 <- plot.matches(psm.5, "Propensity Score Matches - Strong treatment effect on X1 and X2")
mdp.5 <- plot.matches(mdm.5, "Mahalanobis Distance Matches - Strong treatment effect on X1 and X2")



# ggpubr::ggarrange(psp.1, mdp.1, common.legend = TRUE)
# ggpubr::ggarrange(psp.2, mdp.2, common.legend = TRUE)
# ggpubr::ggarrange(psp.3, mdp.3, common.legend = TRUE)
# ggpubr::ggarrange(psp.4, mdp.4, common.legend = TRUE)
# ggpubr::ggarrange(psp.5, mdp.5, common.legend = TRUE)

ggpubr::ggarrange(psp.1,psp.2,psp.3, mdp.1, mdp.2, mdp.3, common.legend = TRUE, legend = "right", align = "hv")


ggpubr::ggarrange(psp.1,psp.4,psp.5, mdp.1, mdp.4, mdp.5, common.legend = TRUE, legend = "right")


## ----setup, echo=FALSE--------------------------------------------------------
#function to load data directly to named variable instead of
#the variable being loaded into the global environment
load.data <- function(dt, pkg){
  
  # make sure the relevant library is available
  library(pkg, character.only = T)
  #create environment for loading, otherwise loads into global
  e <- new.env()
  #get data
  data(list=dt, package=pkg, envir = e)
  #store data
  d <- e[[dt]]
  #delete environment
  e <- NULL
  #return data
  return(d)
}

fig.count <- 1
#this is a custom function for loading data into named variable
dt <- load.data("fev", "mplot")


## ----preview data-------------------------------------------------------------

head(dt)


## ----data plots---------------------------------------------------------------
library(dplyr)
library(ggplot2)

#add meaningful labels
dt <- dt %>% mutate(Sex=ifelse(sex==0,"Female","Male"),
                   Smokes=ifelse(smoke==0,"No","Yes"))

p1 <- ggplot(dt, aes(x=Smokes)) 

p1.1 <- p1 + geom_boxplot(aes(y=age, fill=Smokes)) 
p1.2 <- p1 + geom_boxplot(aes(y=height, fill=Smokes))
p1.3 <- p1 + geom_count(aes(y=Sex, colour=Smokes))
p1.4 <- p1 + geom_boxplot(aes(y=fev, fill=Smokes))


p2 <- ggplot(dt, aes(y=fev)) 

p2.1 <- p2 + geom_point(aes(x=age, color=Smokes), alpha=0.5) 
p2.2 <- p2 + geom_point(aes(x=height, color=Smokes), alpha=0.5)
p2.3 <- p2 + geom_boxplot(aes(x=Sex, fill=Smokes))
p2.4 <- p2 + geom_boxplot(aes(x=Smokes, fill=Smokes))

## ----data plots1--------------------------------------------------------------
ggpubr::ggarrange(p1.1,p1.2,p1.3,p1.4, common.legend = T)
fig.count <- fig.count + 1

## ----data plots2--------------------------------------------------------------
ggpubr::ggarrange(p2.1,p2.2,p2.3,p2.4, common.legend = T)
fig.count <- fig.count + 1

## ----dag----------------------------------------------------------------------
fig.count <- fig.count + 1

#smoke, age, height, sex, fev
fev.dag <- dagitty::dagitty('dag{
                 age -> height
                 sex -> height
                 age -> smoke
                 height -> fev
                 smoke -> height
                 smoke -> fev
                 age -> fev
                 age -> smoke
                 smoke [exposure]
                 fev [outcome]
                 }', layout=FALSE)


dagitty::coordinates(fev.dag) <- 
  list(x=c(age=0.5,fev=1,height=0.5,sex=0,smoke=0),
       y=c(age=0.9,fev=0.5,height=0.1,sex=0,smoke=0.5))

plot(fev.dag)



## ----create ps matix----------------------------------------------------------
f <- smoke~age+height

#fit model
fev.mod <- glm(f, data=dt, family="binomial") 

#get matrix using the fitted values from the model (propensity scores)
#the matrix is generated using the treatment variable (y in the model output) 
ps.dm <- optmatch::match_on(x=fev.mod$fitted.values, z=fev.mod$y)


## ----create mahalanobis matrix------------------------------------------------
#Mahalanobis distance matrix
md.dm <- optmatch::match_on(x=f, data=dt, method="mahalanobis")


## ----get matches--------------------------------------------------------------

#to make this repeatable I'm just going to wrap this is a function
match.ids <- function(dm){ 
    
  #we can get the row and column names from the matrix to make looping a bit easier. 
  #It will be the same for both methods in this case.
  t.names <- attributes(dm)$dimnames$treatment
  c.names <- attributes(dm)$dimnames$control
  
  # initialise a dataframe for storing matches
  ids <- list(treatment=character(),
              control=character(),
              dist=numeric()
  ) %>% as.data.frame()
  
  #loop over the distance matrix rows using the order they are already in
  for (i in t.names) {
    #select the row
    
    d <- dm[i,!c.names %in% ids$control]
    
    c.val <- names(which.min(d))
    if (length(d)==1) c.val <- c.names[!c.names %in% ids$control]
    
    a <- list(treatment = i,
              control = c.val,
              dist = min(d))
    
    #add the matched pair to the dataframe
    ids <- rbind(ids,a)
  
    row.names(ids) <- 1:nrow(ids)
  
  }
  
  return(ids)
  
}

#we can specify @.Data to get the matrix explictly as the optmatch 
#object has some other parts we don't require
ps.dm <- ps.dm@.Data
md.dm <- md.dm@.Data


#the paired ids for all the matches
ps.ids <- match.ids(ps.dm)
md.ids <- match.ids(md.dm)

#are the matches the same?
table(ps.ids$control==md.ids$control)

## ----plot matches-------------------------------------------------------------
ps.matches <- rbind(
  cbind(dt[ps.ids$treatment,], subclass=ps.ids$treatment),
  cbind(dt[ps.ids$control,], subclass=ps.ids$treatment))

md.matches <- rbind(
  cbind(dt[md.ids$treatment,], subclass=md.ids$treatment),
  cbind(dt[md.ids$control,], subclass=md.ids$treatment))

mp1 <- ggplot(ps.matches, aes(height, age, group=subclass)) +
  geom_point(data=dt, aes(height, age), colour="black", alpha=0.3,
             inherit.aes = F, size=1) + 
  geom_point(aes(color=Smokes), alpha=0.6, size=3) + 
  geom_line(color="black") +
  labs(title="Propensity Score Matching matches")

mp2 <- ggplot(md.matches, aes(height, age, group=subclass)) +
  geom_point(data=dt, aes(height, age), colour="black", alpha=0.3,
             inherit.aes = F, size=1) + 
  geom_point(aes(color=Smokes), alpha=0.6, size=3) + 
  geom_line(color="black") +
  labs(title="Mahalanobis Distance Matching matches")

## ----plot matches1------------------------------------------------------------
mp1
fig.count <- fig.count + 1

## ----plot matches2------------------------------------------------------------
mp2
fig.count <- fig.count + 1

## ----view model estimates-----------------------------------------------------
f1 <- fev ~ smoke
f2 <- fev ~ height + smoke
f3 <- fev ~ age + smoke
f4 <- fev ~ age + height + smoke
  
mod1.1 <- lm(f1, md.matches) 
mod1.2 <- lm(f1, ps.matches) 
mod1.3 <- lm(f1, dt) 

mod2.1 <- lm(f2, md.matches) 
mod2.2 <- lm(f2, ps.matches) 
mod2.3 <- lm(f2, dt)

mod3.1 <- lm(f3, md.matches) 
mod3.2 <- lm(f3, ps.matches) 
mod3.3 <- lm(f3, dt)

mod4.1 <- lm(f4, md.matches) 
mod4.2 <- lm(f4, ps.matches) 
mod4.3 <- lm(f4, dt)

mod.comp <- rbind(
  c(model="Mahalanobis Match", formula=deparse(f1), coef(mod1.1)["smoke"], confint(mod1.1)["smoke",]),
  c(model="Propensity Score Match", formula=deparse(f1), coef(mod1.2)["smoke"], confint(mod1.2)["smoke",]),
  c(model="Unmatched", formula=deparse(f1), coef(mod1.3)["smoke"], confint(mod1.3)["smoke",]),
  
  c(model="Mahalanobis Match", formula=deparse(f2), coef(mod2.1)["smoke"], confint(mod2.1)["smoke",]),
  c(model="Propensity Score Match", formula=deparse(f2), coef(mod2.2)["smoke"], confint(mod2.2)["smoke",]),
  c(model="Unmatched", formula=deparse(f2), coef(mod2.3)["smoke"], confint(mod2.3)["smoke",]),
  
  c(model="Mahalanobis Match", formula=deparse(f3), coef(mod3.1)["smoke"], confint(mod3.1)["smoke",]),
  c(model="Propensity Score Match", formula=deparse(f3), coef(mod3.2)["smoke"], confint(mod3.2)["smoke",]),
  c(model="Unmatched", formula=deparse(f3), coef(mod3.3)["smoke"], confint(mod3.3)["smoke",]),
  
  c(model="Mahalanobis Match", formula=deparse(f4), coef(mod4.1)["smoke"], confint(mod4.1)["smoke",]),
  c(model="Propensity Score Match", formula=deparse(f4), coef(mod4.2)["smoke"], confint(mod4.2)["smoke",]),
  c(model="Unmatched", formula=deparse(f4), coef(mod4.3)["smoke"], confint(mod4.3)["smoke",])
) %>% unlist() %>% as.data.frame() %>% 
  mutate(across(smoke:`97.5 %`, as.numeric)) 

colnames(mod.comp) <- c("model", "formula", "estimate", "CI2.5", "CI97.5")

library(plotly)

ggplotly(ggplot(mod.comp, aes(x=estimate, y=paste0(model,"\n",formula), colour=model)) +
  geom_point() + geom_errorbarh(aes(xmin=CI2.5, xmax=CI97.5), height=0.2) +
  labs(y="Method and Formula for Producing Estimate", x="Estimate", color="Method"))


## -----------------------------------------------------------------------------

dagitty::localTests(fev.dag, md.matches, type="cis.chisq")


## -----------------------------------------------------------------------------
# library(WhatsMatching)
library(ggplot2)

#sample size
n <- 200
#treatment effect
te <- 4

#treatment variable with less treated units
t <- rbinom(200,1,0.4)
#covariates
X1 <- rnorm(n,20,5)
X2 <- rnorm(n,10,2)

#lets make a big treatment effect on one of the covariates
#t*10 will double the value of X2 for treated units (on average)
X2 <- X2 + t*2

y <- t*te + X1 + X2 + rnorm(n)

dt <- cbind(t, X1, X2, y) %>% as.data.frame()

dt %>% ggplot() + geom_point(aes(X1, X2, color=factor(t)))

## -----------------------------------------------------------------------------
# mod <- glm(formula=t~X1, data=dt, family="binomial")
# 
# c(var="X1", estimate=coef(mod)["X1"], confint(mod)["X1",])
# coef(mod)[["X1"]]
# 
# dt.match <- matched.data(t~X1, dt, "Mahalanobis")
# 
# get.estimates(match.data=dt.match, f=y~t+X1+X2)
# 
# lm(y~X1+X2, data=dt.match$data)
# 
# dt.match$treatment


a <- c("smoke", "fev", "t", "y")[c("fev", "smoke", "t", "y") %in% colnames(fev)]

a <- NULL



paste(NULL,a, sep="+", collapse=" ")

noquote(paste("y", "~", "t", paste(NULL,NULL, sep="+ ", collapse = " ")))


as.formula(t~y+X1)


