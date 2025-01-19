
rm(list = ls())


## Load packages
library(data.table)
library(lubridate)
library(ggplot2)
library(zoo)
library(jsonlite)


## Define functions
create_json = function(data = data.table()) {
  combined_kvs <- list()
  keys = names(data)
  values = c(data)
  combined_kvs = list()
  for (i in 1:length(keys)) {
    combined_kvs[[key[i]]] = values[i]
  }
  json_obj <- toJSON(combined_kvs, auto_unbox = TRUE)
  return(json_obj)
}

flatten_jsonb = function(json_str) {
  # Preprocess the JSON string to ensure proper escaping of double quotes
  json_str_cleaned = gsub('(?<!\\\\)"', '\\"', json_str, perl = TRUE)     # Escape unescaped double quotes
  json_str_cleaned = gsub('\\"\\{', '{', json_str_cleaned, fixed = TRUE)  # Fix double escaping at the start
  json_str_cleaned = gsub('\\}\\"', '}', json_str_cleaned, fixed = TRUE)  # Fix double escaping at the end
  
  # Parse the JSON string
  json_data = tryCatch(
    fromJSON(json_str_cleaned, simplifyVector = TRUE),
    error = function(e) stop("Invalid JSON string: ", e$message)
  )
  
  # Flatten the JSON data
  json_flat = unlist(json_data, recursive = TRUE, use.names = TRUE)
  
  # Return as a data.table
  data.table(keys = names(json_flat), values = json_flat)
}


## Define global variables
PATH_DATA = '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/PIC/UA/MIBI_Trending/data/'
  

## Load data
files = list.files(PATH_DATA)
files = files[grep('gms_us_lake.', files)]
data = list()
for (file in files) {
  print(paste0('Reading ', file))
  data[[file]] = fread(paste0(PATH_DATA, file))
}


## Explore data

#  id              name                rows           cols
# ------ | --------------------- | -------------- | ---------- |
# k = 1  | batch                 |       282      |     16     |
# k = 2  | classification        |        18      |     16     |
# k = 3  | current_work_space    |    211150      |     46     |
# k = 4  | equipment             |        59      |     27     |
# k = 5  | file_attachment       |        15      |     22     |
# k = 6  | group                 |         0      |     17     |
# k = 7  | promotion_status      |         3      |     17     |
# k = 9  | incubation            |     57260      |     26     |
# k = 10 | limit_rule            |     35185      |     21     |
# k = 11 | limit                 |     10061      |     40     |
# k = 12 | limit_token           |        10      |     17     |
# k = 13 | location              |       191      |     30     |
# k = 14 | location_type         |        11      |     18     |
# k = 15 | media                 |        40      |     28     |
# k = 16 | organism_found        |      1536      |     48     |
# k = 17 | payload_queue_message |         6      |     23     |
# k = 18 | personnel_site        |        24      |     19     |
# k = 19 | plan                  |       346      |     41     |
# k = 20 | product               |        80      |     18     |
# k = 21 | rpt_sample_mart       |     54892      |     89     |
# k = 22 | sample_product        |     23212      |     18     |
# k = 23 | sample_review         |       505      |     17     |
# k = 24 | sample                |     35577      |     45     |
# k = 25 | site                  |      1328      |     23     |
# k = 26 | site_type             |        17      |     16     |
# k = 27 | table_note            |     49612      |     20     |
# k = 28 | test_category         |        17      |     17     |
# k = 29 | test                  |      7109      |     41     |
# k = 30 | test_type             |        15      |     28     |
# k = 31 | time_frame            |        89      |     49     |
# k = 32 | user                  |       243      |     32     |
# ------ | --------------------- | -------------- | ---------- |


## Table where samples are stored
rpt_sample_mart = data[[21]]
rpt_sample_mart[,location_id := as.numeric(location_id)]
rpt_sample_mart[,test_type_id := as.numeric(test_type_id)]
rpt_sample_mart[,site_id := as.numeric(site_id)]
rpt_sample_mart[,reading_1_value := as.numeric(reading_1_value)]
rpt_sample_mart[,reading_2_value := as.numeric(reading_2_value)]
rpt_sample_mart[,reading_3_value := as.numeric(reading_3_value)]
rpt_sample_mart[,action_limit_value := as.numeric(action_limit_value)]
rpt_sample_mart[,alert_limit_value := as.numeric(alert_limit_value)]

## Create wide dataset for different location types associated to a specific 
# location_id (breadcrumb structure with labels instead of ids) and merge with 
# rpt_sample_mart
location_type_tmp = data[[14]][, list(location_type_id, location_type_name = name)]
location_tmp = data[[13]][,list(location_type_id, location_id, location_parent_id = parent_id, location_id_breadcrumb, location_name = name, location_path, location_description = description)]
location_tmp[, row_id := .I]
location_tmp[, breadcrumb_ids := strsplit(location_id_breadcrumb, "\\.")]
breadcrumb_flat <- location_tmp[, .(location_id = as.integer(unlist(breadcrumb_ids)), row_id), by = .(location_id_breadcrumb)]
breadcrumb_flat <- merge(breadcrumb_flat, location_tmp[, .(location_id, location_type_id, location_description)], by = "location_id", all.x = TRUE)
breadcrumb_flat <- merge(breadcrumb_flat, location_type_tmp, by = "location_type_id", all.x = TRUE)
breadcrumb_wide <- dcast(breadcrumb_flat, row_id ~ location_type_name, value.var = "location_description", 
                         fun.aggregate = function(x) paste(unique(x), collapse = ", "))
location_tmp <- merge(location_tmp, breadcrumb_wide, by = "row_id", all.x = TRUE)
location_tmp[, c("row_id", "breadcrumb_ids") := NULL]
location_tmp[,location_id := as.numeric(location_id)]
this_key = 'location_id'
setkeyv(rpt_sample_mart, this_key)
setkeyv(location_tmp, this_key)
data_final = location_tmp[rpt_sample_mart]


## Merge data_final and test_type table to get test_type_code
test_type_tmp = data[[30]][,list(test_type_id, test_type_code = code)]
test_type_tmp[,test_type_id := as.numeric(test_type_id)]
this_key = 'test_type_id'
setkeyv(data_final, this_key)
setkeyv(test_type_tmp, this_key)
data_final = test_type_tmp[data_final]


## Merge data_final and site table to get site_description
site_tmp = data[[25]][,list(site_id, site_description = description)]
site_tmp[,site_id := as.numeric(site_id)]
this_key = 'site_id'
setkeyv(data_final, this_key)
setkeyv(site_tmp, this_key)
data_final = site_tmp[data_final]


## Remove cases where reading_1_value = reading_2_value = reading_3_value = NA
N = nrow(data_final)

ind_rem = data_final[,which(is.na(reading_1_value) & is.na(reading_2_value) & is.na(reading_3_value))]
if (length(ind_rem) > 0) {
  print(paste0(round(100 * (length(ind_rem)/N), 2), ' % of data is removed: reading_1_value = reading_2_value = reading_3_value = null'))
  if (FALSE) {
    cols = grep('read', names(rpt_sample_mart))
    data_final[ind_rem]
  }
  data_final = data_final[-ind_rem]
}

columns = c('location_type_id',
            'location_parent_id',
            'location_id',
            'location_id_breadcrumb',
            'location_full_name',
            'location_name',
            'location_path',
            'location_description',
            "Area",
            "Building",
            "Campus",
            "Company",
            "Facility",
            "Room",
            "Site",
            'product_1', 
            'product_2',
            'batch_1',
            'batch_2',
            'AUD_LD_BTCH_ID',
            'site_id',
            'site_name', 
            'site_description',
            'test_type_code',
            'test_type_desc',
            'name',
            'start_date',
            'end_date',
            'action_limit_value', 
            'alert_limit_value',            
            'reading_1_value',
            'reading_1_text',
            'reading_1_sign',
            'reading_1_uom',
            'reading_2_value', 
            'reading_2_text',
            'reading_2_sign',            
            'reading_2_uom',
            'reading_3_value',
            'reading_3_text',
            'reading_3_sign',          
            'reading_3_uom')

# For room monitoring, there are no batches and products because they are not related
data_final_long <- melt(data_final,
                        id.vars = columns[!grepl('reading', columns)], 
                        measure.vars = list(
                          param_actl_num = grep('reading_.*_value$', columns, value = TRUE),
                          param_actl_desc = grep('reading_.*_text$', columns, value = TRUE),
                          param_uom = grep('reading_.*_uom$', columns, value = TRUE)),
                        variable.name = "rec_repl_id")

# Adjust the `rec_repl_id` to reflect the replicate number
data_final_long[,rec_repl_id := as.integer(rec_repl_id)]
data_final_long[,param_actl_num := as.numeric(param_actl_num)]
data_final_long[,param_actl_desc := as.character(param_actl_desc)]
data_final_long[,param_uom := as.character(param_uom)]

# Transform additional variables
data_final_long[,product_1 := as.character(product_1)]
data_final_long[,product_2 := as.character(product_2)]
data_final_long[,start_date := ymd_hms(start_date)]

# Adding date values
data_final_long[,start_date_month := month(start_date)]
data_final_long[,start_date_year := year(start_date)]

# Define calculation key
CALC_KEY = c('start_date_month', 'start_date_year', 'location_description', 
             'Area', 'Building', 'Campus', 'Company', 'Facility', 'Room', 
             'Site', 'location_full_name', 'test_type_code', 'test_type_desc', 
             'site_description')
  
## Environmental monitoring viable data:

# % CRR (Contamination Recovery Rate = % of non-zero results)
# - Classification (Grade, ISO)                                       -> location_description
# - Room                                                              -> Area, Building, Campus, Company, Facility, Room, Site
# - Sample type (air viable, surface viable - floors/walls/other)     -> test_type_code, test_type_desc
# - Sample point                                                      -> site_description
# - Date range                                                        -> monthly, quarterly, every day using a 30-day window

## Environmental and personnel monitoring data:

# Action limit excursion rate (% of samples over the action limit)
# - Facility/suite/area
# - Classification (Grade/Zone/ISO)
# - Room/personnel
# - Sample type (air viable, surface viable - floors/ walls/other, total airborne particle, personnel)
# - Sample point
# - Date range

# Alert level excursion rate % of samples over the alert level)
# - Facility/suite/area
# - Classification (Grade/Zone/ISO)
# - Room/personnel
# - Sample type (air viable, surface viable - floors/ walls/other, total airborne particle, personnel)
# - Sample point
# - Date range

# Calculate metrics by batch_ids
data_trending = data_final_long[,list(CRR_1 = sum(param_actl_num != 0, na.rm = TRUE),
                                      Action_ER_1 = sum(param_actl_num != 0, na.rm = TRUE),
                                      Alert_ER_1 = sum(param_actl_num != 0, na.rm = TRUE),
                                      Count_Non_NA = sum(!is.na(param_actl_num)))
                                , by = CALC_KEY]
data_trending[,CRR := 100 * (CRR_1/Count_Non_NA)]
data_trending[,Action_ER := 100 * (Action_ER_1/Count_Non_NA)]
data_trending[,Alert_ER := 100 * (Alert_ER_1/Count_Non_NA)]
data_trending[,c('CRR_1', 'Action_ER_1', 'Alert_ER_1', 'Count_Non_NA') := NULL]

# Only consider data from 2024
data_trending = data_trending[start_date_year %in% c(2024)]

# Calculate group-id for plotting purposes
GRP_KEY = c('location_description', 
             'Area', 'Building', 'Campus', 'Company', 'Facility', 'Room', 
             'Site', 'location_full_name', 'test_type_code', 'test_type_desc', 
             'site_description')

# Transforming data structure to long
data_trending[,metric_range_start_dt := as.Date(paste(start_date_year, start_date_month, 1, sep = "-"), format = "%Y-%m-%d")]
data_trending[,metric_range_end_dt := as.Date(as.yearmon(paste(start_date_year, start_date_month, sep = "-")), frac = 1)]
data_trending[,c('start_date_year', 'start_date_month') := NULL]
id_vars = names(data_trending)[!names(data_trending) %in% c("CRR", "Action_ER", "Alert_ER")]
data_trending_long = melt(data_trending, 
                          id.vars = id_vars,
                          measure.vars = c("CRR", "Action_ER", "Alert_ER"), 
                          variable.name = "metric_nm", value.name = "metric_actl_num")
data_trending_long[metric_nm == 'Alert_ER', metric_nm := 'Alert level excursion rate']
data_trending_long[metric_nm == 'Action_ER', metric_nm := 'Action limit excursion rate']
data_trending_long[metric_nm == 'CRR', metric_nm := '% CRR']
data_trending_long[,metric_id := .GRP, by = c(GRP_KEY, 'metric_nm')]


## Splitting data into ref_metric and txn_metric_tbls
col_nms = names(data_trending_long)
ref_nms = col_nms[!col_nms %in% c('metric_range_start_dt', 'metric_range_end_dt', 'metric_actl_num')]
ref_metric = unique(data_trending_long[,.SD, .SDcol = ref_nms])
ref_metric[,group_map_json_obj := toJSON(.SD, auto_unbox = TRUE), .SDcols = ref_nms[!ref_nms %in% c("metric_nm", "metric_id")], by = 'metric_id']
if (FALSE) {
  ref_metric[1, flatten_jsonb(group_map_json_obj)]
}
txn_nms = c('metric_id', 'metric_range_start_dt', 'metric_range_end_dt', 'metric_actl_num')
txn_metric = data_trending_long[,.SD, .SDcol = txn_nms]


## Save data
fwrite(ref_metric, paste0(PATH_DATA, 'ref_metric.csv'))
fwrite(txn_metric, paste0(PATH_DATA, 'txn_metric.csv'))
#saveRDS(data_trending, file = paste0(PATH_DATA, 'moda_trending.RDS'))


# Check
if (FALSE) {
  k = 3
  loc_full_nm = result_by_batch[k, location_full_name]
  month = result_by_batch[k, start_date_month]
  year = result_by_batch[k, start_date_year]
  tst_desc = result_by_batch[k, test_type_desc]
  
  result_by_batch[location_full_name == loc_full_nm & start_date_month == month & start_date_year == year & test_type_desc == tst_desc]
  
  tmp = rpt_sample_mart[location_full_name == loc_full_nm & month(start_date) == month & year(start_date) == year & test_type_desc == tst_desc]
  tmp[,list(reading_1_value, reading_2_value, reading_3_value, action_limit_value, alert_limit_value)]
  
  
  
  n_non_zero = tmp[,sum(reading_1_value != 0) + sum(reading_2_value != 0) + sum(reading_3_value != 0)]
  N = tmp[!is.na(reading_1_value), .N] + tmp[!is.na(reading_2_value), .N] + tmp[!is.na(reading_3_value), .N]
  CRR = 100 * (n_non_zero / N)
  CRR;result_by_batch[k, CRR]
  
  data_final_long[location_id == loc_id & site_id == s_id, sum(param_actl_num != 0, na.rm = TRUE)]
  data_final_long[location_id == loc_id & site_id == s_id & !is.na(param_actl_num), .N]
  
  result_by_batch[AUD_LD_BTCH_ID == aud_btch_id]
}













## END
