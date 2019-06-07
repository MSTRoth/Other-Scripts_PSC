####USASpending API calls


library(jsonlite)
library(httr)
library(openxlsx)
library(plyr)
library(data.table)
library(tidyverse)

setwd("X:/1 Marielle Folder/Data Sets/USA Spending Data/API pulls")

base_url <- "https://api.usaspending.gov/api/"

url <- paste(base_url,"v1/tas/balances", sep = "")
             
url[1]

#df = data.frame(matrix(NA, nrow = 0, ncol = 0))
pages <- list()
get.data <- fromJSON(url, flatten=TRUE)
get.data$page_metadata$has_next_page=TRUE

i<-1

while(get.data$page_metadata$has_next_page==TRUE){
  get.data <- fromJSON(paste0(url, "?page=", i), flatten=TRUE)
  message("Retrieving page ",i)
  pages[[i]] <- get.data$results
  i<-i+1
}

USAspending_TAS_Data <- bind_rows(pages)

write.csv(USAspending_TAS_Data, file="USAspending_TAS_Data_4162019.csv", row.names = FALSE)








