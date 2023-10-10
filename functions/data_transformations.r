library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)


transform_metadata_to_df <- function(data) {
  transformed_data <-
    data[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>% 
    unnest_wider(latLon)
  return(transformed_data)
}


#TASK 4a -----

to_iso8601 <- function(dateTime,measured_days) {
  
  #Apply offset
  adjusted_dateTime <- dateTime + days(measured_days)
  
  # Convert to ISO8601 format and append "Z" for UTC timezone indication
  iso_date_format <- paste0(anytime::iso8601(adjusted_dateTime), "Z")
  
  return(iso_date_format)
}

to_iso8601(as_datetime("2016-09-01 10:11:12"),0)
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)

