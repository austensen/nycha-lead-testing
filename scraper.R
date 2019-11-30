library(tidyverse)
library(rvest)
library(fs)
library(here)


# Get all the possible development values from the drop-down menu. The "value" of
# each drop-down option is some JSON info about the development as a string. We
# can then use this to build the URL to generate the report for that
# development.

# These values look this: 
# {"TDS_NUM":256,"BOROUGH_NAME":"Brooklyn","DEVELOPMENT_NAME":"ATLANTIC+TERMINAL+SITE+4B"}

dev_option_vals <- read_html("https://my.nycha.info/PublicSite/Transparency/RenderReportData") %>% 
  html_nodes("div select option") %>% 
  map_chr(html_attr, "value") %>% 
  discard(~.x == "")


# Scraper -----------------------------------------------------------------

# For a given development value get all the details for that report, and return
# as a one-row dataframe
get_dev_data <- function(dev_value) {
  
  dev_info <- jsonlite::fromJSON(dev_value) %>% 
    as_tibble() %>% 
    set_names(c("tds_num", "borough_name", "development_name")) %>% 
    mutate_all(as.character)
  
  dev_page <- str_glue("https://my.nycha.info/PublicSite/Transparency/RenderReportData?Development={dev_value}") %>% 
    URLencode() %>% 
    read_html()
  
  # For some developments the testing hasn't started and there are no tables
  # displayed, so we just keep the development info
  inspections_not_started <- dev_page %>% 
    html_node("body") %>% 
    html_text() %>% 
    str_detect("Lead inspections for your Borough/Development have not started yet")
  
  if (inspections_not_started) {
    return(dev_info)
  }
  
  
  cumulative_info <- dev_page %>% 
    html_node("#PerformanceByBorough") %>% 
    html_table(fill = TRUE) %>% 
    set_names(c("borough", "cumulative_actual_to_date", "cumulative_progress_to_final_target", "cumulative_final_target")) %>% 
    select(-1) %>% 
    slice(2) %>% 
    mutate_all(as.character)
  
  
  final_info <- dev_page %>% 
    html_node("#PerformanceByBorough~ .wrap") %>% 
    html_table(fill = TRUE) %>% 
    set_names(c("num_unit_test_results_received", "num_positive", "num_negative", "num_units_pending_results")) %>% 
    mutate_all(as.character)
  
  
  bind_cols(dev_info, cumulative_info, final_info)
  
}

# Iterate over each development from the drop-down menu and run the scraper,
# collecting all the results in a single dataframe and then clear up the
# formatting of all the columns
nycha_testing_data <- dev_option_vals %>% 
  map_dfr(get_dev_data) %>% 
  mutate(
    tds_num = as.integer(tds_num),
    cumulative_actual_to_date = cumulative_actual_to_date %>% parse_number() %>% as.integer(),
    cumulative_progress_to_final_target = cumulative_progress_to_final_target %>% parse_number(),
    cumulative_final_target = cumulative_final_target %>% parse_number() %>% as.integer(),
    num_unit_test_results_received = num_unit_test_results_received %>% parse_number() %>% as.integer(),
    num_positive = num_positive %>% parse_number() %>% as.integer(),
    num_negative = num_negative %>% parse_number() %>% as.integer(),
    num_units_pending_results = num_units_pending_results %>% parse_number() %>% as.integer()
    
  )

# Export the results to csv

dir_create(here("data"))

out_file <- here("data", str_glue("nycha-xrf-lead-testing_{Sys.Date()}.csv"))

write_csv(nycha_testing_data, out_file, na = "")