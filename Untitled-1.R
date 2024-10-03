
# Load libraries
library(jsonlite)
library(tidyverse)
library(ggplot2)

all_numer_vars = c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP")
time_vars = c("JWAP", "JWDP")
all_cat_vars = c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
all_geog = c("All", "Region", "Division", "State")
key_query <- "&key=ef57316550be8b6492df7ca01b32a193c83ed8fc"

get_tibble <- function(data_info){
  
  parsed <- fromJSON(rawToChar(data_info$content))

  num_columns <- length(parsed[1,])
  
  census_data <- data.frame(parsed[-1,])
  names(census_data) <- parsed[1,]

  return(as_tibble(census_data))
}

format_num_and_cat_vars <- function(data_tibble, numer_vars, cat_vars){

  data_tibble <- data_tibble %>%
    mutate(across(numer_vars(), as.numeric)) 
  return(data_tibble)
}

query_census_api <- function(year=2022, numer_vars=c("AGEP"), cat_vars=c("SEX"), geog="State", geog_values=c('12')){
  
  ######################INPUT VALIDATION######################
  #year
  if ((!year %in% 2010:2022)){
    stop("Year must be between 2010 and 2022.")
  }
  
  #numer_vars
  numer_vars <- union(numer_vars, "PWGTP")
  
  if(length(numer_vars) < 2) {
    stop("Not enough numerical variables specified. Options are AGEP, GASP, GRPIP, JWAP, JWDP, JWMNP, and PWGTP")
  } else if(length(union(all_numer_vars, numer_vars)) > length(all_numer_vars)){
    stop("numer_vars includes variables which are not in the allowed set, which is AGEP, GASP, GRPIP, JWAP, JWDP, JWMNP, and PWGTP")
  }
  
  #cat_vars
  if(length(cat_vars) < 1) {
    stop("Not enough categorical variables specified. Options are FER, HHL, HISPEED, JWTRNS, SCH, SCHL, and SEX")
  } else if(length(union(all_cat_vars, cat_vars)) > length(all_cat_vars)){
    stop("cat_vars includes variables which are not in the allowed set, which is FER, HHL, HISPEED, JWTRNS, SCH, SCHL, and SEX")
  }
  
  #geog
  if (!(geog %in% all_geog)){
    stop("invalid value for geog. Options are All, Region, Division, and State")
  }
  ######################INPUT VALIDATION######################

  #now to build the query URL
  vars_query <- paste0(c(numer_vars, cat_vars), collapse=",")
  
  geog_query <- character()
  
  if(geog != "All"){
    if (length(geog_values) == 0) {
      geog_query <- paste0("&for=", tolower(geog), ":*")
    } else {
      geog_query <- paste0("&for=", tolower(geog), ":", paste0(geog_values, collapse=","))
    }
  } 
  
  URL_census <- paste0("https://api.census.gov/data/", 
                      year, 
                      "/acs/acs1/pums?",
                      "get=",
                      vars_query,
                      geog_query,
                      "&SCHL=24",
                      key_query)
  
  data_info <- httr::GET(URL_census)
  data = get_tibble(data_info)
  clean_data = format_num_and_cat_vars(data, numer_vars, cat_vars)
  #class(clean_data) <- c("census",class(clean_data))
  
}

query_with_years <- function(years=c(2022), ...){
  
  combined_tibble = tibble()
  
  for(i in seq_along(years)){
    year_tibble <- query_census_api(year=years[i], ...) |>
                      mutate(year=years[i])
    
    combined_tibble <- bind_rows(combined_tibble, year_tibble)
  }
  
  return(combined_tibble)
}



#data_tibble_test <- query_census_api(year=2021, numer_vars=c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP"))
#data_tibble_test <- query_census_api(year=2021, numer_vars=c("AGEP"), cat_vars=c("HISPEED"), geog="State", geog_values=c("06", "24"))
#data_tibble_test <- query_census_api(year=2021, numer_vars=c("AGEP"), cat_vars=c("HISPEED"), geog="State")

data_tibble_test <- query_census_api(year=2022, numer_vars=c("AGEP"), cat_vars=c("HISPEED"))
data_tibble_test

data_tibble <- query_with_years(numer_vars=c("JWAP"))
data_tibble