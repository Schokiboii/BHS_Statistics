# Set local library path to bundled R Portable library
.libPaths(c(file.path(getwd(), "R-Portable", "library"), .libPaths()))

# Load required packages
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(lubridate)
library(DT)
library(stringr)
library(ggplot2)

# Launch Shiny application
shiny::runApp(
  appDir = "scripts", 
  launch.browser = TRUE, 
  port = 8888
)