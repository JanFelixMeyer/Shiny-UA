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
# UI function
#-------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body { 
        height: 100%; 
        margin: 0;
      }
      .sidebar {
        min-height: 100vh;
        overflow-y: auto;
      }
      .center-button {
        text-align: center;
        margin-bottom: 10px;
      }
      .action-button {
        width: 100%;
        padding: 10px 20px;
        font-size: 16px;
        cursor: pointer;
        border: none;
        color: white;
      }
      #update {
        background-color: #CC0000;
      }
      #update:hover {
        background-color: #990000;
      }
      #reset {
        background-color: #333333;
      }
      #reset:hover {
        background-color: #555555;
      }
      /* For standard tables */
      table.dataTable tbody tr.selected {
        background-color: #cc5a5a !important;
      }
      /* For tables inside a scrolling container */
      .dataTables_scrollBody table.dataTable tbody tr.selected {
        background-color: #cc5a5a !important;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,
      div(class = "center-button", actionButton("update", "Update", class = "action-button")),
      div(class = "center-button", actionButton("reset", "Reset", class = "action-button")),
      div(style = "margin-top: 20px;"),
      uiOutput("dynamicMetricNameInput"),
      uiOutput("dynamicFilterInput"),
      uiOutput("FilterInputs"),
      uiOutput("dynamicColorByInput"),
      uiOutput("ColorByInputs")
    ),
    mainPanel(
      width = 9,
      conditionalPanel(
        condition = "output.chartReady", 
        plotlyOutput("lineChart", height = "600px")
      ),
      conditionalPanel(
        condition = "!output.chartReady",
        div(
          style = "text-align: center; margin-top: 50px;",
          h3("Please select anything from the filter.")
        )
      ),
      conditionalPanel(
        condition = "output.chartReady",
        DT::dataTableOutput("wideData")
      )
    )
  )
)
