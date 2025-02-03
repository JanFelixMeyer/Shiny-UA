#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(data.table)
library(viridis)
library(DT)


#-------------------------------------------------------------------------------
# Server function
#-------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Definition of global variables
  #-----------------------------------------------------------------------------
  PRINT = FALSE
  
  
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
  # Reference Data Processing
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Reference Data Processing')
  }
  
  load_ref_data = reactive({
    data = fread('ref_metric.csv')
    #data = as.data.table(read.csv('ref_metric.csv', stringsAsFactors = FALSE))
    data
  })
  
  ref_data <- reactive({ 
    load_ref_data() 
  })
  
  metric_data <- reactive({
    req(ref_data(), input$metric_name)  # Wait for the full data and user selection
    ref_data()[metric_nm == input$metric_name]
  })
  
  metric_data_long <- reactive({
    req(metric_data())  # Now based on the user-selected metric
    key_cols = setdiff(names(metric_data()), c("metric_id", "metric_nm", 'group_map_json_obj'))
    data_long = suppressWarnings(melt(metric_data(), id.vars = "metric_id",
                                      measure.vars = key_cols,
                                      variable.name = "column",
                                      value.name = "value"))
    data_long[, group := paste0(column, ":", value)]
    data_long
  })
  
  # Ensure key_values is available
  key_values <- reactive({
    req(input)
    result = get_key_values(metric_data())
    result
  })
  
  filter_metric_ids <- reactive({
    # If no filter is selected, return all metric_ids from metric_data_long
    if (is.null(input$filter) || length(input$filter) == 0) {
      return(unique(metric_data_long()$metric_id))
    }
    
    selectedFilterGroups <- c()
    for (key in input$filter) {
      # Dynamically reference `selectedFilterValues_<key>`
      selectedFilterValues <- input[[paste0("selectedFilterValues_", key)]]
      if (!is.null(selectedFilterValues)) {
        selectedFilterGroups <- c(selectedFilterGroups, paste(key, selectedFilterValues, sep = ":"))
      }
    }
    
    # If any filter values have been selected, filter the data; otherwise, use the entire metric_data_long
    filteredRefData <- if (length(selectedFilterGroups) > 0) {
      metric_data_long() %>% filter(group %in% selectedFilterGroups)
    } else {
      metric_data_long()
    }
    
    metric_ids <- filteredRefData[, unique(metric_id)]
    return(metric_ids)
  })
  
  
  #-----------------------------------------------------------------------------
  # Transactional (txn) Data Processing
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Transactional (txn) Data Processing')
  }
  
  load_txn_data = reactive({
    data = fread('txn_metric.csv')
    #data = as.data.table(read.csv('txn_metric.csv', stringsAsFactors = FALSE))
    data
  })
  
  txn_data <- reactive({ 
    load_txn_data()  
  })
  
  
  #-----------------------------------------------------------------------------
  # Additional functions
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Additonal functions')
  }
  
  # Generate key_values list
  get_key_values = function(data, exclude_cols = c("metric_id", "metric_nm", "group_map_json_obj")) {
    key_values = lapply(setdiff(names(data), exclude_cols), function(col) unique(data[[col]]))
    names(key_values) = setdiff(names(data), exclude_cols)
    return(key_values)
  }
  
  # Reactive values to store previous selections
  prev_selections <- reactiveValues(
    color_by = list(),
    filter = list()
  )
  
  wideData <- reactive({
    # Filter txn_data using filter_metric_ids()
    dt_txn <- txn_data() %>% filter(metric_id %in% filter_metric_ids())
    dt_txn[,metric_actl_num := round(metric_actl_num, 2)]
    dt_ref <- metric_data()
    setkeyv(dt_ref, "metric_id")
    setkeyv(dt_txn, "metric_id")
    merged_dt <- dt_ref[dt_txn]
    id_vars <- names(metric_data())
    id_vars <- id_vars[! id_vars %in% c("group_map_json_obj", "metric_nm", "metric_id")]
    formula_str <- paste(paste(id_vars, collapse = " + "), "~ metric_range_end_dt")
    wide_dt <- dcast(merged_dt, as.formula(formula_str), value.var = "metric_actl_num")
    wide_dt
  })
  
  
  #-----------------------------------------------------------------------------
  # Initialize UI for filter and ColorBy dynamically (data-driven)
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Initialize UI for filter and ColorBy dynamically (data-driven)')
  }
  
  # Display chart when filter selection was made
  output$chartReady <- reactive({
    !is.null(input$filter) && length(input$filter) > 0
  })
  outputOptions(output, "chartReady", suspendWhenHidden = FALSE)
  
  output$dynamicMetricNameInput <- renderUI({
    req(ref_data())
    
    if (PRINT) {
      print('This is dynamicMetricNameInput')
    }
    
    selectInput("metric_name", "- METRIC NAME - ",
                choices = sort(unique(ref_data()$metric_nm)),
                selected = sort(unique(ref_data()$metric_nm))[1])
  })
  
  # Render filter and color-by options
  output$dynamicFilterInput <- renderUI({
    req(key_values())
    
    if (PRINT) {
      print('This is dynamicFilterInput')
    }
    
    checkboxGroupInput(
      "filter", 
      " - FILTER -",
      choices = names(key_values()),
      selected = input$filter
    )
  })
  
  output$dynamicColorByInput <- renderUI({
    req(key_values())
    
    if (PRINT) {
      print('This is dynamicColorByInput')
    }
    
    if (length(input$filter) > 0) {
      checkboxGroupInput(
        "color_by", 
        " - COLOR BY -",
        choices = input$filter,
        selected = input$color_by
      )
    } else {
      return(NULL)
    }
  })
  
  
  #-----------------------------------------------------------------------------
  # Update available options in ColorBy based on Filter selections and Reset
  #-----------------------------------------------------------------------------
  
  observeEvent(input$reset, {
    # Reset the metric name to the first available value
    updateSelectInput(session, "metric_name", 
                      selected = sort(unique(ref_data()$metric_nm))[1])
    
    # Reset the filter and color_by inputs to an empty state
    updateCheckboxGroupInput(session, "filter", selected = character(0))
    updateCheckboxGroupInput(session, "color_by", selected = character(0))
    
    # Clear stored selections so that the dynamic pickerInputs re-render with defaults
    prev_selections$filter <- list()
    prev_selections$color_by <- list()
  })
  
  # Dynamically generate value selection inputs based on selected keys
  output$FilterInputs <- renderUI({
    req(input$filter, length(key_values()) > 0)
    if (length(input$filter) == 0) {
      return(NULL)  # No filters selected
    }
    lapply(input$filter, function(key) {
      feasible_values <- ref_data() %>% 
        pull(key) %>% 
        unique()
      prev_value <- if (is.null(prev_selections$filter[[key]])) {
        character(0)
      } else {
        prev_selections$filter[[key]]
      }
      pickerInput(
        inputId = paste0("selectedFilterValues_", key),
        label = paste0(key, ":"),
        choices = feasible_values,
        selected = prev_value,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          size = 10,
          `actions-box` = TRUE,
          `noneSelectedText` = 'Select one or more values'
        )
      )
    })
  })
  
  # Update stored filter selections
  observe({
    req(input$filter)
    for (key in input$filter) {
      prev_selections$filter[[key]] <- input[[paste0("selectedFilterValues_", key)]]
    }
  })
  
  output$ColorByInputs <- renderUI({
    req(input$color_by, length(key_values()) > 0)
    
    if (PRINT) {
      print('This is ColorByInputs')
    }
    
    lapply(input$color_by, function(key) {
      available_values <- input[[paste0("selectedFilterValues_", key)]]
      if (is.null(available_values)) {
        available_values <- character(0)
      }
      pickerInput(
        inputId = paste0("selectedColorByValues_", key),
        label = paste0(key, ":"),
        choices = available_values,
        selected = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          size = 10,
          `actions-box` = TRUE,
          `noneSelectedText` = 'Select one or more values'
        )
      )
    })
  })
  
  output$lineChart <- renderPlot({
    filteredTxnData <- txn_data() %>% filter(metric_id %in% filter_metric_ids())
    
    if (is.null(input$color_by) || length(input$color_by) == 0) {
      ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id)) +
        geom_line(color = "grey", alpha = 0.5, lwd = 1.2) +
        labs(x = "Date", y = input$metric_name) +
        my_theme
    } else {
      color_by <- input$color_by
      selectedColorByGroups <- c()
      for (key in color_by) {
        selectedColorByValues <- input[[paste0("selectedColorByValues_", key)]]
        if (!is.null(selectedColorByValues) && length(selectedColorByValues) > 0) {
          selectedColorByGroups <- c(selectedColorByGroups, paste(key, selectedColorByValues, sep = ":"))
        }
      }
      
      filteredRefData <- ref_data()[, .SD, .SDcols = c("metric_id", color_by)]
      filteredRefData[,(color_by) := lapply(color_by, function(col) paste0(col, ":", get(col)))]
      filteredRefData[, color_group := do.call(paste, c(.SD, sep = " + ")), .SDcols = color_by]
      filteredRefData[, in_selected_group := apply(.SD, 1, function(row) all(row %in% selectedColorByGroups)), .SDcols = color_by]
      filteredRefData[in_selected_group == FALSE, color_group := "Others"]
      filteredRefData[, in_selected_group := NULL]
      
      setkeyv(filteredRefData, "metric_id")
      setkeyv(filteredTxnData, "metric_id")
      filteredTxnData <- filteredRefData[filteredTxnData]
      
      color_mapping <- unique(filteredTxnData[, .(color_group)])
      num_groups <- nrow(color_mapping)
      color_palette <- viridis(num_groups)
      filteredTxnData[, color := color_palette[.GRP], by = color_group]
      filteredTxnData[color_group == "Others", color := "grey"]
      
      color_values <- setNames(unique(filteredTxnData$color), unique(filteredTxnData$color_group))
      
      ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id, color = color_group)) +
        geom_line(lwd = 1.2) +
        scale_color_manual(values = color_values) +
        labs(x = "Date", y = input$metric_name) +
        my_theme
    }
  })
  
  output$wideData <- DT::renderDataTable({
    wideData()
  }, options = list(
    scrollX = TRUE,
    scrollY = "300px",
    paging = FALSE,
    dom = 't'
  ))
  
}
