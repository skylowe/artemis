# Run ARTEMIS Visualization App
# Usage: source("run_app.R") or Rscript run_app.R

rm(list = ls(all = TRUE))
setwd("app")
source("global.R")
source("ui.R")
source("server.R")
shiny::shinyApp(ui = ui, server = server)
