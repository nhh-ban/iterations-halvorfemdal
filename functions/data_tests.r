# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

#This function checks if the transformed data frame contains the 
#correct/expected column names.

test_stations_metadata_colnames <-
  function(df) {
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

#This function checks whether the transformed data frame contains a reasonable
#amount of rows. If this i true, it will print PASS. If outside this interval
#it will print FAIL. This functions sets the lower bound to 5000 and upper bound
#to 10 000.

test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

#This functions checks whether the transformed data frame contains the correct
#column types. #In this case, the first 2 columns should be strings,
#and the last three should be decimal numbers. 
#All is a function that checks if all elements of a logical vector are TRUE. 
#map_chr( ~ typeof(.)) applies typeof to each column of df, returning a 
#character vector of the types of all columns. This should be equal to the
#expected_coltypes. If not, all columns do not have the correct specification

test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
 
#This function checks whether the data frame has abnormal large amounts of
#missing values. In this case, it is expected to be under 200.
#Map_int( ~ sum(is.na((.)))) applies this summarizing function to each column 
#and returns a vector of the total NA counts for each column.
#%>% sum(.): Sums up the individual NA counts from all columns to get the total 
#number of NA values across the entire dataframe.

test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

#This function checks whether the data frame has the correct timezone, which
#is the latestData column. If not UTC, the print will tell FALSE
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

#Function that call all the functions
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }

