
setwd_golem <- function(LT, PC){
  if (golem::get_golem_wd()==LT & file.exists(PC)) {
    golem::set_golem_wd(path=PC)
  }

  if (golem::get_golem_wd()==PC & file.exists(LT)) {
    golem::set_golem_wd(path=LT)
  }
}

setwd_golem(LT="C:/Users/jmurs/OneDrive/Health Data Science Masters/Dissertation/PSM.golex",
            PC="D:/Jason/OneDrive/Health Data Science Masters/Dissertation/PSM.golex")

here::here()

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()

