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
library(glue)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 

#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>%
  ggplot(aes(x=hour, y=volume)) + 
  geom_line() +
  labs(
    title = "Traffic Volume over time",
    x = "Hour",
    y = "Traffic Volume") +
  theme_classic()

#Task 6 -----

#Seperating to get access to variables for titles and legend titles.

titles_for_plot <- 
  stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %>% 
  mutate(First_date = to_iso8601(latestData, -4),
         Last_date = to_iso8601(latestData, 0))

data_for_plot <- 
  titles_for_plot %$%  
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() 

data_for_plot %>% 
  ggplot(aes(x=hour, y=volume,group=1,colour = volume)) + 
  geom_line() +
  labs(
    title = glue("Traffic Volume from {titles_for_plot$First_date} 
                 to {titles_for_plot$Last_date}"),
    x = "Hour",
    y = "Traffic Volume",
    colour = titles_for_plot$name) +
  theme_classic()

