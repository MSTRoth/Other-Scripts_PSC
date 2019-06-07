library(tidyverse)

data <-read_csv("C:/Users/Roth/Documents/Other Requests (Co-workers)/Matt/list of members data 14-18 2-15-19.csv")

#cage_numbers <- read_csv("C:/Users/Roth/Documents/Other Requests (Co-workers)/Matt/Existing member cage.csv")
cage_numbers <- read_csv("C:/Users/Roth/Documents/Other Requests (Co-workers)/Matt/Cage_no 2-19-19.csv")

colnames(data)
colnames(cage_numbers)

#member_data <- data %>% 
#  filter(`Vendor CAGE Code` %in% cage_numbers$Existing_member_CAGE)
member_data <- data %>% 
  filter(`Vendor CAGE Code` %in% cage_numbers$CAGE_no) 
#%>% 
 # distinct(`Vendor CAGE Code`)

DPAP <- read_csv("C:/Users/Roth/Documents/Reference Tables/DPAP Crosswalk.csv")


####DPAP divisions

member_dpap <- member_data %>% 
  left_join(DPAP, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC Code")) %>% 
  select("Fiscal Year", "Funding Agency", "Contract Number", DPAP, `Vendor CAGE Code`, `Performing Vendor`) %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                contract_number = "Contract Number",
                cage_no = `Vendor CAGE Code`) %>% 
  filter(funding_agency %in% c("Department of Homeland Security (DHS)"
                                 ,"Department of Commerce (DOC)" 
                                 ,"Department of Defense (DOD)"
                                 ,"Department of the Interior (DOI)"
                                 ,"Department of State (DOS)"
                                 ,"Department of Transportation (DOT)"
                                 ,"General Services Administration (GSA)"
                                 ,"Department of Health and Human Services (HHS)"
                                 ,"National Aeronautics and Space Administration (NASA)"
                                 ,"Agency for International Development (USAID)")) %>% 
  mutate(DPAP2 = ifelse(is.na(DPAP), "Product", DPAP)) %>% 
  distinct(cage_no, DPAP2)
  
data_by_dpap <-split(member_dpap, member_dpap$DPAP2) 

####Agency divisions
agency<- member_data %>% 
  filter(`Funding Agency` %in% c("Department of Homeland Security (DHS)"
                                 ,"Department of Commerce (DOC)" 
                                 ,"Department of Defense (DOD)"
                                 ,"Department of the Interior (DOI)"
                                 ,"Department of State (DOS)"
                                 ,"Department of Transportation (DOT)"
                                 ,"General Services Administration (GSA)"
                                 ,"Department of Health and Human Services (HHS)"
                                 ,"National Aeronautics and Space Administration (NASA)"
                                 ,"Agency for International Development (USAID)")) %>% 
  select(`Funding Agency`, `Vendor CAGE Code`, `Contract Number`, `Performing Vendor`) %>% 
  distinct(`Funding Agency`, `Vendor CAGE Code`, .keep_all = TRUE)
#  summarise(count = n())

data_by_agency <-split(agency, agency$`Funding Agency`)

agency_number<-lapply(data_by_agency, function(x){
  unique(x$`Vendor CAGE Code`)

})

write.csv(data_by_agency[[1]], file = paste(unique(data_by_agency[[1]]$`Funding Agency`)," member data.csv", sep = ""))


x<-unique(data_by_agency[[1]]$`Funding Agency`)



list2env(data_by_dpap,envir=.GlobalEnv) 

"Department of the Interior (DOI)"
