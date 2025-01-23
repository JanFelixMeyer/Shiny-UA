if (DEBUG) {
 
   ## Functions
  load_ref_data = function() {
    data = fread(paste0(BASE_DIR, 'ref_metric.csv'))
    # TBD: Implement selection of additional metrics later
    data = data[metric_nm == '% CRR']
    return(data)
  }
  
  ref_data_long = function(data) {
    key_cols = setdiff(names(data), c("metric_id", "metric_nm", 'group_map_json_obj'))
    data_long = suppressWarnings(melt(data, id.vars = "metric_id", 
                                      measure.vars = key_cols, 
                                      variable.name = "column", 
                                      value.name = "value"))
    data_long[,group := paste0(column, ":", value)]
    return(data_long)
  }
  
  key_values <- function(input, data) {
    result = get_key_values(ref_data %>% filter(metric_id %in% filter_metric_ids(input, data)))
    return(result)
  }
  
  filter_metric_ids = function(input, data){
    # When no filter-key is selected all possible metric_ids are relevant
    if (length(input$filter) > 0) {
      selectedFilterGroups <- c()
      for (key in input$filter) {
        selectedFilterValues <- input[[paste0("selectedFilterValues_", key)]]
        if (!is.null(selectedFilterValues)) {
          # Create key-value combinations
          selectedFilterGroups <- c(selectedFilterGroups, paste(key, selectedFilterValues, sep = ":"))
        }
      }
      if (length(selectedFilterGroups) > 0) {
        filteredRefData = data %>% filter(group %in% selectedFilterGroups)        
      } else {
        filteredRefData = data
      }
      metric_ids = filteredRefData[,unique(metric_id)]
    } else {
      metric_ids = data[,unique(metric_id)]
    }
    return(metric_ids)
  }
  
  
  ## Simulation
  
  # Dashboard opens
  input = list()
  ref_data = load_ref_data()
  ref_data_long_obj = ref_data_long(ref_data)
  input$filter = c()
  
  # The user clicks on a key = location_description -> show the feasible values
  input$filter = "location_description"
  key = input$filter[1]
  feasible_values <- ref_data %>%
    filter(metric_id %in% filter_metric_ids(input, ref_data_long_obj)) %>%
    pull(key) %>% 
    unique()
  input[[paste0("selectedFilterValues_", key)]] = feasible_values[1]
    
  # The user selects another key = Area -> show the feasible values (given that location_description = Lager-Steriltest was selected before)
  input$filter = c("location_description", "Area")
  key = input$filter[2]
  feasible_values <- ref_data %>%
    filter(metric_id %in% filter_metric_ids(input, ref_data_long_obj)) %>%
    pull(key) %>% 
    unique()
  input[[paste0("selectedFilterValues_", key)]] = feasible_values[1]
  
  
}