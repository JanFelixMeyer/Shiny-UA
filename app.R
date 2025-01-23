#-------------------------------------------------------------------------------
# Set path to working directory
#-------------------------------------------------------------------------------
LOCAL = TRUE
if (LOCAL) {
  BASE_DIR <<- '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/GitHub/Shiny-UA/'
} else {
  BASE_DIR <<- './'
}


#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)


#-------------------------------------------------------------------------------
# Source functions
#-------------------------------------------------------------------------------
source(paste0(BASE_DIR, "utils.R"))
source(paste0(BASE_DIR, "ui.R"))
source(paste0(BASE_DIR, "server.R"))
source(paste0(BASE_DIR, "ggplot_theme.R"))

DEBUG = FALSE
source(paste0(BASE_DIR, "debug.R"))


#-------------------------------------------------------------------------------
# Run the app
#-------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
