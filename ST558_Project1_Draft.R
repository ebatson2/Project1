
##################################
# Program: ST558_Project1_Draft.R
# Software: R version 4.4.1 (2024-06-14) -- "Race for Your Life"
# Description: R script to help us plan for our Quarto doc
# Your API key is ef57316550be8b6492df7ca01b32a193c83ed8fc
# Read this document for working with Census APIs:
#https://www.census.gov/content/dam/Census/data/developers/api-user-guide/api-user-guide.pdf
##################################
# Load libraries
library(jsonlite)
library(tidyverse)

# Starting out, just a basic query following the Census examples and class notes
URL_ids <- "https://api.census.gov/data/2022/acs/acs1/pums?get=SEX,PWGTP,MAR&SCHL=24&key=ef57316550be8b6492df7ca01b32a193c83ed8fc"
id_info <- httr::GET(URL_ids)
str(id_info, max.level = 1)
parsed <- fromJSON(rawToChar(id_info$content))

#Breaking down the URL into its different components 
#host name
https://api.census.gov/data/
#data year
2022/
#dataset name acronym
acs/acs1/pums
#variables that we want to look at
?get=SEX,PWGTP,MAR&SCHL=24
#Your API Key
&key=ef57316550be8b6492df7ca01b32a193c83ed8fc"


  
  