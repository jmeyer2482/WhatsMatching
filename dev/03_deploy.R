# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile()

## If you want to deploy to ShinyProxy
golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
golem::add_dockerfile_heroku()


rsconnect::setAccountInfo(name='jmeyer2482',
                          token='1D657AE94157F3BE40248BFFD25884BF',
                          secret='ucqnT4iv1o1QUFE6WmuBIekd5nFpAm5OdjK55fb0')

#
# rsconnect::setAccountInfo(name='jmeyer2482',
#                           token='C5C2F89630BCB30C99C721E0AD9FBC8A',
#                           secret='NMextOp1PfJCY0Vo5rcL/crlJ30bKOkIHiQDk2p4')

##INSTALL

devtools::install_github("jmeyer2482/WhatsMatching")
