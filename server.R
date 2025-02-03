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
  my_theme = theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = element_text(size = 14)
    )
  
  #-----------------------------------------------------------------------------
  # Reference Data Processing
  #-----------------------------------------------------------------------------
  if (PRINT) { print('Reference Data Processing') }
  load_ref_data = reactive({
    data = fread('ref_metric.csv')
    data
  })
  ref_data <- reactive({ load_ref_data() })
  metric_data <- reactive({
    req(ref_data(), input$metric_name)
    ref_data()[metric_nm == input$metric_name]
  })
  metric_data_long <- reactive({
    req(metric_data())
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
    if (is.null(input$filter) || length(input$filter) == 0) {
      return(unique(metric_data_long()$metric_id))
    }
    selectedFilterGroups <- c()
    for (key in input$filter) {
      selectedFilterValues <- input[[paste0("selectedFilterValues_", key)]]
      if (!is.null(selectedFilterValues)) {
        selectedFilterGroups <- c(selectedFilterGroups, paste(key, selectedFilterValues, sep = ":"))
      }
    }
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
  if (PRINT) { print('Transactional (txn) Data Processing') }
  load_txn_data = reactive({
    data = fread('txn_metric.csv')
    data = as.data.table(read.csv('txn_metric.csv', stringsAsFactors = FALSE))
    data
  })
  txn_data <- reactive({ load_txn_data() })
  
  #-----------------------------------------------------------------------------
  # Additional functions
  #-----------------------------------------------------------------------------
  if (PRINT) { print('Additonal functions') }
  get_key_values = function(data, exclude_cols = c("metric_id", "metric_nm", "group_map_json_obj")) {
    key_values = lapply(setdiff(names(data), exclude_cols), function(col) unique(data[[col]]))
    names(key_values) = setdiff(names(data), exclude_cols)
    return(key_values)
  }
  prev_selections <- reactiveValues(
    color_by = list(),
    filter = list()
  )
  
  #-----------------------------------------------------------------------------
  # Create Wide Data Table (for DT) by merging reference & txn data, then pivoting
  #-----------------------------------------------------------------------------
  wideData <- reactive({
    dt_txn <- txn_data() %>% filter(metric_id %in% filter_metric_ids())
    dt_txn[, metric_actl_num := round(metric_actl_num, 2)]
    dt_ref <- metric_data()
    setkeyv(dt_ref, "metric_id")
    setkeyv(dt_txn, "metric_id")
    merged_dt <- dt_ref[dt_txn]
    # Do not remove metric_id because it is needed for linking; remove metric_nm and group_map_json_obj later.
    id_vars <- names(metric_data())
    id_vars <- id_vars[! id_vars %in% c("group_map_json_obj", "metric_nm")]
    formula_str <- paste(paste(id_vars, collapse = " + "), "~ metric_range_end_dt")
    wide_dt <- dcast(merged_dt, as.formula(formula_str), value.var = "metric_actl_num")
    wide_dt
  })
  
  #-----------------------------------------------------------------------------
  # Define reactive for the highlighted metric (from DT selection)
  #-----------------------------------------------------------------------------
  highlight_metric <- reactive({
    selected_row <- input$wideData_rows_selected
    if (is.null(selected_row) || length(selected_row) == 0) return(NULL)
    dt <- wideData()
    return(dt[selected_row, metric_id])
  })
  
  #-----------------------------------------------------------------------------
  # Initialize UI for filter and ColorBy dynamically (data-driven)
  #-----------------------------------------------------------------------------
  if (PRINT) { print('Initialize UI for filter and ColorBy dynamically (data-driven)') }
  
  output$chartReady <- reactive({ !is.null(input$filter) && length(input$filter) > 0 })
  outputOptions(output, "chartReady", suspendWhenHidden = FALSE)
  
  output$dynamicMetricNameInput <- renderUI({
    req(ref_data())
    if (PRINT) { print('This is dynamicMetricNameInput') }
    selectInput("metric_name", "- METRIC NAME - ",
                choices = sort(unique(ref_data()$metric_nm)),
                selected = sort(unique(ref_data()$metric_nm))[1])
  })
  
  output$dynamicFilterInput <- renderUI({
    req(key_values())
    if (PRINT) { print('This is dynamicFilterInput') }
    checkboxGroupInput("filter", " - FILTER -",
                       choices = names(key_values()),
                       selected = input$filter)
  })
  
  output$dynamicColorByInput <- renderUI({
    req(key_values())
    if (PRINT) { print('This is dynamicColorByInput') }
    if (length(input$filter) > 0) {
      checkboxGroupInput("color_by", " - COLOR BY -",
                         choices = input$filter,
                         selected = input$color_by)
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "metric_name", 
                      selected = sort(unique(ref_data()$metric_nm))[1])
    updateCheckboxGroupInput(session, "filter", selected = character(0))
    updateCheckboxGroupInput(session, "color_by", selected = character(0))
    prev_selections$filter <- list()
    prev_selections$color_by <- list()
  })
  
  output$FilterInputs <- renderUI({
    req(input$filter, length(key_values()) > 0)
    if (length(input$filter) == 0) return(NULL)
    lapply(input$filter, function(key) {
      feasible_values <- ref_data() %>% pull(key) %>% unique()
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
  
  observe({
    req(input$filter)
    for (key in input$filter) {
      prev_selections$filter[[key]] <- input[[paste0("selectedFilterValues_", key)]]
    }
  })
  
  output$ColorByInputs <- renderUI({
    req(input$color_by, length(key_values()) > 0)
    if (PRINT) { print('This is ColorByInputs') }
    lapply(input$color_by, function(key) {
      available_values <- input[[paste0("selectedFilterValues_", key)]]
      if (is.null(available_values)) available_values <- character(0)
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
  
  output$lineChart <- renderPlotly({
    filteredTxnData <- txn_data() %>% filter(metric_id %in% filter_metric_ids())
    
    if (is.null(input$color_by) || length(input$color_by) == 0) {
      p <- ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id)) +
        geom_line(color = "grey", alpha = 0.5, size = 1.2) +
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
      # Use the hue palette instead of viridis
      color_palette <- hue_pal()(num_groups)
      filteredTxnData[, color := color_palette[.GRP], by = color_group]
      filteredTxnData[color_group == "Others", color := "grey"]
      
      color_values <- setNames(unique(filteredTxnData$color), unique(filteredTxnData$color_group))
      
      p <- ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id, color = color_group)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = color_values, name = "") +
        labs(x = "Date", y = input$metric_name) +
        my_theme
    }
    
    hl <- highlight_metric()
    if (!is.null(hl)) {
      hl_data <- filteredTxnData %>% filter(metric_id == hl)
      p <- p + geom_line(data = hl_data, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id),
                         color = "black", size = 2)
    }
    
    # Convert ggplot to Plotly and adjust the layout:
    ggplotly(p) %>% layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.25,      # Adjust this value to increase the gap between x-axis and legend
        xanchor = "center"
      ),
      margin = list(b = 150)  # Increase bottom margin to create more space below x-axis label
    )
  })
  
  output$wideData <- DT::renderDataTable({
    DT::datatable(
      wideData(),
      selection = "single",
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        paging = FALSE,
        dom = 't',
        columnDefs = list(
          list(visible = FALSE, targets = which(names(wideData()) %in% c("metric_id", "metric_nm")) - 1)
        )
      )
    )
  }, server = FALSE)
  
}
