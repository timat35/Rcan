shiny_folder <- "C:/Projects/Rcan/Shiny"
.libPaths(paste0(shiny_folder,"/pkg")

library(shiny)

shiny_folder <- "C:/Projects/Rcan/Shiny"
shiny::runApp(shiny_folder, launch.browser = TRUE)
