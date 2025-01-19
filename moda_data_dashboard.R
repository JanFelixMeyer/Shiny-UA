library(shiny)
library(ggplot2)
library(data.table)

# Sample Data
ref_metric <- data.table(
  location_description = c("Lager-Steriltest", "Lager-Steriltest", "Lager-Steriltest"),
  metric_id = c(1, 2, 3),
  metric_nm = c("% CRR", "% CRR", "% CRR")
)

txn_metric <- data.table(
  metric_id = c(1, 1, 1),
  metric_range_start_dt = as.Date(c("2024-06-01", "2024-05-01", "2024-07-01")),
  metric_range_end_dt = as.Date(c("2024-06-30", "2024-05-31", "2024-07-31")),
  metric_actl_num = c(0.0, 0.0, 66.66667)
)

# Shiny App
ui <- fluidPage(
  titlePanel("Line Chart Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("color_by", "Color By:", choices = colnames(ref_metric), selected = "location_description"),
      selectInput("filter_by", "Filter By:", choices = colnames(ref_metric), selected = "location_description"),
      selectInput("y_metric", "Y-Axis Metric:", choices = unique(ref_metric$metric_nm), selected = "% CRR")
    ),
    mainPanel(
      plotOutput("line_chart")
    )
  )
)

server <- function(input, output, session) {
  # Reactive data for filtering
  filtered_data <- reactive({
    req(input$filter_by)
    ref_metric_subset <- ref_metric[, .(metric_id, get(input$filter_by))]
    txn_metric[metric_id %in% ref_metric_subset$metric_id]
  })
  
  # Render plot
  output$line_chart <- renderPlot({
    req(input$color_by, input$y_metric)
    
    # Match metric name to ID
    selected_metric_id <- ref_metric[metric_nm == input$y_metric, metric_id]
    
    # Subset data
    data <- filtered_data()[metric_id %in% selected_metric_id]
    
    # Get the column for color grouping
    color_column <- ref_metric[metric_id %in% data$metric_id, get(input$color_by)]
    data[, color_group := factor(color_column)]
    
    # Plot
    ggplot(data, aes(x = metric_range_end_dt, y = metric_actl_num, color = color_group)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(
        title = "Metric Line Chart",
        x = "Date",
        y = input$y_metric,
        color = input$color_by
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
