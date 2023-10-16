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

vol_qry <- function(id, from, to) {
  query <- glue('
  {
    trafficData(trafficRegistrationPointId: "[id]") {
      volume {
        byHour(from: "[from]", to: "[to]") {
          edges {
            node {
              from
              to
              total {
                volumeNumbers {
                  volume
                }
              }
            }
          }
        }
      }
    }
  }
  ',
  id = id, from = from, to = to,
  .open = "[", .close = "]" # use "{ and }" as delimiters to avoid conflict with GraphQL syntax
  )
  return(query)
}

