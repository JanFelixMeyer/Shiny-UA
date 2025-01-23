
get_key_values = function(data, exclude_cols = c("metric_id", "metric_nm", "group_map_json_obj")) {
  # Generate key_values list
  key_values = lapply(setdiff(names(data), exclude_cols), function(col) unique(data[[col]]))
  names(key_values) = setdiff(names(data), exclude_cols)
  return(key_values)
}
