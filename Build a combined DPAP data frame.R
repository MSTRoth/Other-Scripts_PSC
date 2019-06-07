##Build a combined DPAP data frame


civ_data <- read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/DPAP (services and total) Data - Civilian.csv") ###
def_data <- read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/DPAP (services and total) Data - DoD.csv") ###

civ <- civ_data %>% 
  select('Fiscal Year', 'DPAP Category', Spend)

def<-def_data %>% 
  select('Fiscal Year', 'DPAP Category', spend) %>% 
  rename(Spend = spend)

full <- bind_rows(list("Civilian" = civ, "Defense" = def), .id = 'id')

check_gov_wide <- full %>% 
  rename(fiscal_year = 'Fiscal Year',
         DPAP_Category = 'DPAP Category') %>% 
  dplyr::group_by(fiscal_year, DPAP_Category) %>% 
  summarise(sum(Spend))


full_DPAP <- full %>% 
  rename(fiscal_year = 'Fiscal Year',
         DPAP = 'DPAP Category') %>% 
  spread(DPAP, Spend) %>% 
  mutate_if(is.numeric, dollar)

write.csv(full_DPAP, "S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/DPAP combined chart - 11-18.csv")













setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")

data <- read_csv("X:/1 Marielle Folder/Data Sets/By Agency/NASA/NASA FY15-18.csv")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

agency_chart <- data %>% 
  rename("PSC" = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         "transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  filter(P.S == "Service") %>% 
  filter(fiscal_year == 2018) %>% 
  group_by(DPAP, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value))

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
