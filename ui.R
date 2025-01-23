ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar {
        max-height: 90vh;
        overflow-y: auto;
      }
      .center-button {
        text-align: center;
        margin-bottom: 10px;
      }
      .action-button {
        width: 100%; /* Buttons will have equal width */
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
        background-color: #333333; /* Dark grey background */
      }
      #reset:hover {
        background-color: #555555; /* Lighter grey on hover */
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 2,
      div(class = "center-button", actionButton("update", "Update", class = "action-button")),
      div(class = "center-button", actionButton("reset", "Reset", class = "action-button")),
      div(style = "margin-top: 20px;"),
      uiOutput("dynamicFilterInput"),
      uiOutput("FilterInputs"),
      uiOutput("dynamicColorByInput"),
      uiOutput("ColorByInputs")
    ),
    mainPanel(
      width = 10,
      conditionalPanel(
        condition = "output.chartReady",
        plotOutput("lineChart", height = "600px")
      ),
      conditionalPanel(
        condition = "!output.chartReady",
        div(
          style = "text-align: center; margin-top: 50px;",
          h3("Please select anything from the filter.")
        )
      )
    )
  )
)
