# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
attachment::att_amend_desc()
devtools::document()
attachment::att_from_namespace()


## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "landing", with_test = F) # Name of the module
golem::add_module(name = "Info", with_test = F)
golem::add_module(name = "RealData", with_test = F)
golem::add_module(name = "SimData", with_test = F)

golem::add_module(name = "ViewData")

golem::add_module(name = "MatchSettings", with_test = F)
# golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("simulations", with_test = F)
golem::add_fct("matches", with_test = F)
golem::add_fct("matchplot", with_test = F)
golem::add_fct("multimatchplot", with_test = F)

# golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")

# Documentation

## Packages
# usethis::use_package("tidyverse")
# usethis::use_package("MatchIt")
# usethis::use_package("mplot")
# usethis::use_package("cobalt")
# usethis::use_package("optmatch")
# usethis::use_package("shinipsum")
# usethis::use_package("shinyBS")
# usethis::use_package("ggplot2")
# usethis::use_package("dplyr")
# usethis::use_package("mplot")
# usethis::use_package("plotly")
# usethis::use_package("shiny")
# usethis::use_package("ggforce")
# usethis::use_package("shinyBS")
# usethis::use_package("htmltools")
# usethis::use_package("DT")
# usethis::use_package("MASS")
# usethis::use_package("arm")


# usethis::use_package("cem")
# usethis::use_package("ggpubr")
# usethis::use_package("catdata")
# usethis::use_package("ggforce")
# usethis::use_package("dagitty")
# usethis::use_package("dagR")


## Vignette ----
# usethis::use_vignette("WhatsMatching-Theory")
# usethis::use_vignette("WhatsMatching-HowTo")

usethis::use_vignette("a-Background", title="Vignette 1 - Background")
usethis::use_vignette("b-TheApp", title="Vignette 2 - The App")
usethis::use_vignette("c-Applications", title="Vignette 3 - Applications")
usethis::use_vignette("d-References", title="References")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
# usethis::use_github()

# GitHub Actions
# usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# Add action for PR
# usethis::use_github_action_pr_commands()

# Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()

# AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()

# Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()

# Jenkins
# usethis::use_jenkins()

# GitLab CI
# usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

devtools::load_all()

usethis::use_pkgdown()
pkgdown::build_site()
pkgdown::build_reference(lazy=F)
pkgdown::build_articles(lazy=F)

pkgdown::build_favicons()

