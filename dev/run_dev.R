# golem::setwd_golem(here::here())
# golem::set_golem_wd()
# Set options here
options(golem.app.prod = T) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()

# between(NULL,1,30)
