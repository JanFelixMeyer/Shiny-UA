#-------------------------------------------------------------------------------
# Set path to working directory
#-------------------------------------------------------------------------------
LOCAL = FALSE
if (LOCAL) {
  BASE_DIR <<- '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/GitHub/Shiny-UA/'
  setwd(BASE_DIR)
}


#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(data.table)
library(DT)
library(plotly)
library(scales)


#-------------------------------------------------------------------------------
# Source functions
#-------------------------------------------------------------------------------
source("ui.R")
source("server.R")


#-------------------------------------------------------------------------------
# Run the app
#-------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
