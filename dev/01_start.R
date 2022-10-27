# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "WhatsMatching", # The Name of the package containing the App
  pkg_title = "What is Matching?", # The Title of the package containing the App
  pkg_description = "The WhatsMatching package demonstrates how propensity score matching and mahalanobis distance matching compare under different circumstances. The aim of the app is to provide educational insights into matching methods.", # The Description of the package containing the App
  author_first_name = "Jason", # Your First Name
  author_last_name = "Meyer", # Your Last Name
  author_email = "jmeyer2482@gmail.com", # Your Email
  repo_url = "https://github.com/jmeyer2482/WhatsMatching" # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("Jason Meyer") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Jason Meyer")
usethis::use_lifecycle_badge("Stable")
usethis::use_news_md(open = FALSE)
usethis::use_github_links()

## Use git ----
usethis::use_git(message = "testing use_git")
usethis::use_git_config(user.name= "jmeyer2482", user.email="jmeyer2482@gmail.com")
usethis::create_github_token()
credentials::set_github_pat("YourPAT")

usethis::git_sitrep()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()
golem::use_recommended_deps()
#
# golem::set_golem_wd(path="C:/Users/jmurs/OneDrive/Health Data Science Masters/Dissertation/PSM.golex")
# golem::set_golem_wd(path="D:/Jason/OneDrive/Health Data Science Masters/Dissertation/PSM.golex")

golem::get_golem_wd()
## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon(path="inst/app/www/matches.ico") # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
