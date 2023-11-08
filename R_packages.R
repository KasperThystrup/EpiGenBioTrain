req_packages <- c(
  "tidyverse",
  "ggraph",
  "igraph",
  "reshape2",
  "RColorBrewer",
  "shiny",
  "ggnetwork",
  "sna",
  "shinydashboard",
  "shinycssloaders",
  "DT",
  "shinyWidgets",
  "shinyjs",
  "shinyBS"
) 

lib_home = grep(x = .libPaths(), pattern = "\\/home\\/", value = TRUE)
install.packages(req_packages, lib = lib_home)
