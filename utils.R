
get_key_values = function(data, exclude_cols = c("metric_id", "metric_nm", "group_map_json_obj")) {
  # Generate key_values list
  key_values = lapply(setdiff(names(data), exclude_cols), function(col) unique(data[[col]]))
  names(key_values) = setdiff(names(data), exclude_cols)
  return(key_values)
}

filter_data <- function(data, filter) {
  filter_conditions <- strsplit(filter, ":")
  conditions_list <- lapply(filter_conditions, function(x) list(column = x[1], value = x[2]))
  filter_dt <- rbindlist(lapply(conditions_list, function(x) data.table(key = x$column, value = x$value)))
  
  
  # Apply the filters dynamically
  filtered_data <- data
  for (condition in filter_conditions) {
    filtered_data <- filtered_data %>% filter(get(condition$column) == condition$value)
  }
  
  return(filtered_data)
}