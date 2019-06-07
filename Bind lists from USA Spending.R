library(tidyverse)
install.packages("data.table")
library(data.table)


BA_2017<-lapply(1:5, function(x) {
  BA_2017 <- read_csv(paste("X:/1 Marielle Folder/Data Sets/Government-Wide Data/xlsx/Period of Availability/2017 Account Breakdown by Award ",x,".csv", sep = ""))
} )

BA_Total <- rbindlist(BA_2017, use.names=TRUE, fill=FALSE, idcol=NULL)

write.csv(BA_Total, "X:/1 Marielle Folder/Data Sets/Government-Wide Data/xlsx/Period of Availability/2017 Account Breakdown by Award Total.csv")




group_by()

unique(BA_Total$submission_period
)       
