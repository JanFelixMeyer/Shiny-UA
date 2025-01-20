#-------------------------------------------------------------------------------
# Set path to working directory
#-------------------------------------------------------------------------------
LOCAL = FALSE
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


#-------------------------------------------------------------------------------
# The app
#-------------------------------------------------------------------------------
ui <- fluidPage(
  
  # Custom CSS to make the sidebar vertically scrollable and style the button
  tags$head(
    tags$style(HTML("
      .sidebar {
        max-height: 90vh;  /* Set the maximum height for the sidebar */
        overflow-y: auto;  /* Enable vertical scrolling */
        /* Remove text-align from here */
      }
      
      .center-button {
        text-align: center;  /* Center only the button */
        margin-bottom: 10px;  /* Space below the button */
      }
      
      #update {
        background-color: #CC0000;  /* Red background */
        color: white;  /* White text */
        border: none;  /* Remove border */
        padding: 10px 20px;  /* Adjust padding */
        font-size: 16px;  /* Increase font size */
        cursor: pointer;  /* Change cursor to pointer */
        display: inline-block;  /* Keep button inline */
      }
      #update:hover {
        background-color: #990000;  /* Darker red on hover */
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply custom class to sidebar
      width = 2,
      
      div(style = "margin-bottom: 10px;"),
      
      # Button centered inside the div
      div(class = "center-button", 
          actionButton("update", "Update")
      ),
      
      div(style = "margin-bottom: 30px;"),
      
      # Filter - will be dynamically generated
      uiOutput("dynamicFilterInput"),
      uiOutput("FilterInputs"),
      
      div(style = "margin-top: 20px;"),
      
      # Color By - will be dynamically generated
      uiOutput("dynamicColorByInput"),  
      uiOutput("ColorByInputs")
    ),
    
    mainPanel(
      width = 10,
      plotOutput("lineChart", height = "600px")  # Output the line chart
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Global definitions
  #-----------------------------------------------------------------------------
  DEBUG = FALSE

  
  #-----------------------------------------------------------------------------
  # Data processing
  #-----------------------------------------------------------------------------
  load_ref_data = reactive({
    data = fread(paste0(BASE_DIR, 'ref_metric.csv'))
    # TBD: Implement selection of additional metrics later
    data = data[metric_nm == '% CRR']
    data
  })
  
  load_txn_data = reactive({
    data = fread(paste0(BASE_DIR, 'txn_metric.csv'))
    data
  })
  
  
  #-----------------------------------------------------------------------------
  # Define ggplot2-theme
  #-----------------------------------------------------------------------------
  my_theme = theme_minimal()
  my_theme = my_theme + theme(

    # Title and axis labels
    plot.title = element_text(size = 16),    # Increase plot title size
    axis.title = element_text(size = 14),    # Increase axis titles size
    axis.text = element_text(size = 12),     # Increase axis tick labels size
    
    # Legend text
    legend.title = element_blank(),
    legend.text = element_text(size = 12),   # Increase legend text size
    
    # Other settings
    legend.position = "bottom",              # Place the legend below the chart
    legend.direction = "horizontal",         # Arrange legend items horizontally
    strip.text = element_text(size = 14)     # Increase facet label size (if using facets)
  )
  
  
  #-----------------------------------------------------------------------------
  # Initial load when the server starts
  #-----------------------------------------------------------------------------
  ref_data <- reactive({ 
    load_ref_data() 
  })
  
  # Define long version of the reference data for easier search
  ref_data_long <- reactive({
    req(ref_data())  # Ensure ref_data is available
    key_cols = setdiff(names(ref_data()), c("metric_id", "metric_nm", 'group_map_json_obj'))
    data_long = suppressWarnings(melt(ref_data(), id.vars = "metric_id", 
                                      measure.vars = key_cols, 
                                      variable.name = "column", 
                                      value.name = "value"))
    data_long[,group := paste0(column, ":", value)]
    data_long
  })
  
  # Ensure key_values is available
  key_values <- reactive({
    result = get_key_values(ref_data())
    return(result)
  })
  
  # Load transactional data
  txn_data <- reactive({ 
    load_txn_data() 
  })
  
  # Reactive values to store previous selections
  prev_selections <- reactiveValues(
    color_by = list(),
    filter = list()
  )
  
  
  #-----------------------------------------------------------------------------
  # Add dynamic UI generation
  #-----------------------------------------------------------------------------
  output$dynamicFilterInput <- renderUI({
    req(key_values())  # Ensure key_values() is available
    
    checkboxGroupInput("filter", " - FILTER -", 
                       choices = names(key_values()), 
                       selected = names(key_values()))  # Default to all keys
  })
  
  output$dynamicColorByInput <- renderUI({
    req(key_values())
    
    checkboxGroupInput("color_by", " - COLOR BY -",
                       choices = names(key_values()), 
                       selected = NULL)
  })
  
  
  #-----------------------------------------------------------------------------
  # Update available options in ColorBy based on Filter selections
  #-----------------------------------------------------------------------------
  
  # Update Filter selections
  observe({
    req(input$filter, key_values())

    # Update Filter
    updateCheckboxGroupInput(
      session, 
      inputId = "filter", 
      choices = names(key_values()), 
      selected = input$filter
    )
  })
  
  # Update Color By options when Filter selections change
  observe({
    req(input$filter)
    
    # Get the selected keys in Filter
    selected_filter_keys <- input$filter
    
    updateCheckboxGroupInput(
      session,
      inputId = "color_by",
      choices = selected_filter_keys,  # Limit options to selected Filter keys
      selected = intersect(input$color_by, selected_filter_keys)  # Preserve only valid selections
    )
  })

  # Dynamically generate value selection inputs based on selected keys
  output$FilterInputs <- renderUI({
    req(input$filter, length(key_values()) > 0)

    # Check if filter input is not empty
    if (length(input$filter) == 0) {
      return(NULL)  # Do not render if no filters selected
    }
    
    lapply(input$filter, function(key) {
      prev_value <- ifelse(is.null(prev_selections$filter[[key]]), key_values()[[key]], prev_selections$filter[[key]])
      selectInput(
        inputId = paste0("selectedFilterValues_", key),
        label = paste0(key, ":"),
        choices = key_values()[[key]],
        selected = prev_value,  # Use previous selection or default to first value
        multiple = TRUE  # Allow multiple selections
      )
    })
  })
  
  output$ColorByInputs <- renderUI({
    req(input$color_by, length(key_values()) > 0)
    
    isolate({
      for (key in input$color_by) {
        prev_selections$color_by[[key]] <- ifelse(
          is.null(input[[paste0("selectedColorByValues_", key)]]),
          key_values()[[key]],
          input[[paste0("selectedColorByValues_", key)]]
        )
      }
    })
    
    lapply(input$color_by, function(key) {
      selectInput(
        inputId = paste0("selectedColorByValues_", key),
        label = paste0(key, ':'),
        choices = key_values()[[key]],
        selected = prev_selections$color_by[[key]],  # Restore stored selections
        multiple = TRUE
      )
    })
  })
  
  # Reactively update the chart when the update button is clicked
  observeEvent(input$update, {
    
    if (DEBUG) {
      ref_data = fread(paste0(BASE_DIR, 'ref_metric.csv'))
      txn_data = fread(paste0(BASE_DIR, 'txn_metric.csv'))

      input = list()
      input$filter_by <- c("location_description", "Area", "Building")
      input$color_by <- c("Area", "Building")
      for (ele in input$filter_by) {
        for (val in key_values[ele]) {
          input[[paste0("selectedFilterValues_", ele)]] <- val
        }
      }
      for (ele in input$color_by) {
        for (val in key_values[ele]) {
          input[[paste0("selectedColorByValues_", ele)]] <- val
        }
      }
    }    
    
    color_by <- input$color_by
    filter_by <- input$filter
    
    if (DEBUG) {
      print(paste0(rep('-', 30), collapse = '-'))
      print(paste0('color_by = ', paste0(color_by, collapse = ', ')))
      print(paste0('filter_by = ', paste0(filter_by, collapse = ', ')))
    }
    
    # If no keys are selected, consider all keys selected by default
    if (length(filter_by) == 0) {
      filter_by <- names(key_values())  # Default to all keys
    }
    
    # Create an empty vector to store the selected key-value combinations
    selectedFilterGroups <- c()
    for (key in filter_by) {
      selectedFilterValues <- input[[paste0("selectedFilterValues_", key)]]
      if (!is.null(selectedFilterValues)) {
        # Create key-value combinations
        selectedFilterGroups <- c(selectedFilterGroups, paste(key, selectedFilterValues, sep = ":"))
      }
    }
    
    selectedColorByGroups <- c()
    if (length(color_by) > 0) {
      for (key in color_by) {
        selectedColorByValues <- input[[paste0("selectedColorByValues_", key)]]
        if (!is.null(selectedColorByValues)) {
          # Create key-value combinations
          selectedColorByGroups <- c(selectedColorByGroups, paste(key, selectedColorByValues, sep = ":"))
        }
      } 
    }
    
    if (DEBUG) {
      print(paste0('selectedFilterGroups = ', paste0(selectedFilterGroups, collapse = ', ')))
      print(paste0('selectedColorByGroups = ', paste0(selectedColorByGroups, collapse = ', ')))
    }
    
    # Apply Filter
    filteredRefData <- ref_data_long() %>% filter(group %in% selectedFilterGroups)
    metric_ids = filteredRefData[,unique(metric_id)]
    filteredTxnData = copy(txn_data() %>% filter(metric_id %in% metric_ids))
    
    # Derive groups for ColorBy
    filteredRefData = ref_data()[,.SD, .SDcols = c('metric_id', color_by)]
    filteredRefData[,(color_by) := lapply(color_by, function(col) paste0(col, ":", get(col)))]
    filteredRefData[,color_group := do.call(paste, c(.SD, sep = " + ")), .SDcols = color_by]
    filteredRefData[,in_selected_group := apply(.SD, 1, function(row) all(row %in% selectedColorByGroups)), .SDcols = color_by]
    filteredRefData[in_selected_group == FALSE, color_group := 'Others']
    num_groups <- filteredRefData[, uniqueN(color_group)]
    color_palette <- scales::hue_pal()(num_groups)  # Generates distinct colors
    filteredRefData[,color := color_palette[.GRP], by = color_group]
    filteredRefData[color_group == 'Others', color := 'grey']
    filteredRefData[,in_selected_group := NULL]
    
    # Merge to combine Filter with ColorBy
    setkeyv(filteredRefData, 'metric_id')
    setkeyv(filteredTxnData, 'metric_id')
    filteredTxnData = filteredRefData[filteredTxnData]
    
    # Define mapping and values for colors
    color_mapping <- unique(filteredTxnData[, .(color_group, color)])
    color_values <- setNames(color_mapping$color, color_mapping$color_group)
    
    # Create the line chart using ggplot2   
    output$lineChart <- renderPlot({
      u = ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id, color = color_group))
      u = u + geom_line() 
      u = u + scale_color_manual(values = color_values, labels = names(color_values))
      u = u + labs(title = "", x = "Date", y = "Value")
      u = u + my_theme
      u
    })
    
  })
  
  # Render the chart when the app first loads (show all grey lines)
  output$lineChart <- renderPlot({
    u = ggplot(txn_data(), aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id, color = 'grey'))
    u = u + geom_line()
    u = u + scale_color_manual(values = "grey")
    u = u + labs(title = "", x = "Date", y = "Value")
    u = u + my_theme
    u
  })
}

# Run the app
shinyApp(ui = ui, server = server)
