server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Reference Data Processing
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Reference Data Processing')
  }
  
  load_ref_data = reactive({
    data = fread(paste0(BASE_DIR, 'ref_metric.csv'))
    # TBD: Implement selection of additional metrics later
    data = data[metric_nm == '% CRR']
    data
  })
  
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
    req(input)
    result = get_key_values(ref_data() %>% filter(metric_id %in% filter_metric_ids()))
    result
  })
  
  filter_metric_ids <- reactive({
    req(input$filter)  # Ensure filter input is available
    
    selectedFilterGroups <- c()
    for (key in input$filter) {
      # Dynamically reference `selectedFilterValues_<key>`
      selectedFilterValues <- input[[paste0("selectedFilterValues_", key)]]
      if (!is.null(selectedFilterValues)) {
        selectedFilterGroups <- c(selectedFilterGroups, paste(key, selectedFilterValues, sep = ":"))
      }
    }
    
    if (length(selectedFilterGroups) > 0) {
      filteredRefData <- ref_data_long() %>% filter(group %in% selectedFilterGroups)
    } else {
      filteredRefData <- ref_data_long()
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
    data = fread(paste0(BASE_DIR, 'txn_metric.csv'))
    data
  })
  
  txn_data <- reactive({ 
    load_txn_data() 
  })
  
  
  #-----------------------------------------------------------------------------
  # Additonal functions
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Additonal functions')
  }
  
  # Reactive values to store previous selections
  prev_selections <- reactiveValues(
    color_by = list(),
    filter = list()
  )
  
  
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
  
  # Render filter and color-by options
  output$dynamicFilterInput <- renderUI({
    req(key_values())
    
    if (PRINT) {
      print('This is dynamicFilterInput')
    }
    
    checkboxGroupInput("filter", " - FILTER -",
                       choices = names(key_values()),
                       selected = NULL)  # Start with no selection
  })
  
  output$dynamicColorByInput <- renderUI({
    req(key_values())  # Ensure key_values() is available
    
    if (PRINT) {
      print('This is dynamicColorByInput')
    }
    
    # Only render ColorBy input if any filter is selected
    if (length(input$filter) > 0) {
      checkboxGroupInput("color_by", " - COLOR BY -",
                         choices = input$filter,
                         selected = NULL)  # Start with no selection
    } else {
      # If no filter is selected, don't render ColorBy input
      return(NULL)
    }
  })
  
  
  #-----------------------------------------------------------------------------
  # Update available options in ColorBy based on Filter selections
  #-----------------------------------------------------------------------------
  
  if (PRINT) {
    print('Update available options in ColorBy based on Filter selections')
  }
  
  # Update Filter selections
  observe({
    req(input$filter, key_values())
    
    if (PRINT) {
      print('This is updateCheckboxGroupInput for filter')
    }
    
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
    
    if (PRINT) {
      print('This is updateCheckboxGroupInput for color_by')
    }
    
    # Get the selected keys in Filter
    selected_filter_keys <- input$filter
    
    updateCheckboxGroupInput(
      session,
      inputId = "color_by",
      choices = selected_filter_keys,  # Limit options to selected Filter keys
      selected = intersect(input$color_by, selected_filter_keys)  # Preserve only valid selections
    )
  })
  
  # Reset dashboard upon click of reset button
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "filter", selected = NULL)
    updateCheckboxGroupInput(session, "color_by", selected = NULL)
  })
  
  # Dynamically generate value selection inputs based on selected keys
  output$FilterInputs <- renderUI({
    req(input$filter, length(key_values()) > 0)
    
    if (length(input$filter) == 0) {
      return(NULL)  # No filters selected
    }
    
    lapply(input$filter, function(key) {
      feasible_values <- ref_data() %>%
        filter(metric_id %in% filter_metric_ids()) %>%
        pull(key) %>%
        unique()
      
      prev_value <- ifelse(is.null(prev_selections$filter[[key]]), feasible_values, prev_selections$filter[[key]])
      
      selectInput(
        inputId = paste0("selectedFilterValues_", key),
        label = paste0(key, ":"),
        choices = feasible_values,
        selected = prev_value,
        multiple = TRUE
      )
    })
  })
  
  output$ColorByInputs <- renderUI({
    req(input$color_by, length(key_values()) > 0)
    
    if (PRINT) {
      print('This is ColorByInputs')
    }
    
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
  observeEvent(c(input$filter, lapply(input$filter, function(key) input[[paste0("selectedFilterValues_", key)]])), {
    # Trigger reactivity chain for the chart
    output$lineChart <- renderPlot({
      filteredTxnData <- txn_data() %>%
        filter(metric_id %in% filter_metric_ids())
      
      ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id)) +
        geom_line() +
        labs(x = "Date", y = "Value") +
        my_theme
    })
  }, ignoreNULL = FALSE)
  
  if (FALSE) {
    observeEvent(input$update, {
      
      if (PRINT) {
        print('This is Plotting')
      }
      
      color_by <- input$color_by
      filter_by <- input$filter
      
      # If no keys are selected, consider all keys selected by default
      if (length(filter_by) == 0) {
        filter_by <- names(key_values())  # Default to all keys
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
      filteredTxnData = copy(txn_data() %>% filter(metric_id %in% filter_metric_ids()))
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
  }
  
  # Render the chart when the app first loads (show all grey lines)
  output$lineChart <- renderPlot({
    filteredTxnData <- txn_data() %>%
      filter(metric_id %in% filter_metric_ids())
    
    ggplot(filteredTxnData, aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id)) +
      geom_line() +
      labs(x = "Date", y = "Value") +
      my_theme
  })
  
  if (FALSE) {
    output$lineChart <- renderPlot({
      u = ggplot(txn_data(), aes(x = metric_range_end_dt, y = metric_actl_num, group = metric_id, color = 'grey'))
      u = u + geom_line()
      u = u + scale_color_manual(values = "grey")
      u = u + labs(title = "", x = "Date", y = "Value")
      u = u + my_theme
      u
    })
  }

}