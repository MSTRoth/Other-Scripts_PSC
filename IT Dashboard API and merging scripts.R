IT_Portfolio_url <-	"https://itdashboard.gov/api/v1/ITDB2/dataFeeds/portfolio"

get.data <- fromJSON(IT_Portfolio_url, flatten=TRUE)

IT_Portfolio <- get.data$result


IT_Portfolio_Funding_Sources_url	<-"https://itdashboard.gov/api/v1/ITDB2/dataFeeds/fundingSource"
get.data <- fromJSON(IT_Portfolio_Funding_Sources_url, flatten=TRUE)

IT_Portfolio_Funding_Sources <- get.data$result




IT_Portfolio_url <-	"https://myit-2018.itdashboard.gov/api/v1/ITDB2/dataFeeds/portfolio"

get.data <- fromJSON(IT_Portfolio_url, flatten=TRUE)

IT_Portfolio_2019 <- get.data$result
IT_Portfolio_2020<-IT_Portfolio


url <- "https://myit-2019.itdashboard.gov/api/v1/ITDB2/dataFeeds/fundingSource"
get.data <- fromJSON(url, flatten=TRUE)

IT <- get.data$result

# DF_org<-IT %>% 
#   dplyr::group_by(agencyName, typeOfInvestment) %>% 
#   dplyr::summarize("Sum_Total_PY" = sum(totalITspendingPY),
#             "Sum_Total_CY" = sum(totalITspendingCY),
#             "Sum_Total_BY" = sum(totalITspendingBY))


write.csv(IT, "2018 ITPortfolio funding sources.csv")


names(IT)

#IT Portfolio	                         https://itdashboard.gov/api/v1/ITDB2/dataFeeds/portfolio
#Provisioned IT Spending               https://itdashboard.gov/api/v1/ITDB2/dataFeeds/provisionedITSpending
#IT Infrastructure Spending Summary	   https://itdashboard.gov/api/v1/ITDB2/dataFeeds/infrastructureSpendingSummary
#IT Portfolio Funding Sources          https://itdashboard.gov/api/v1/ITDB2/dataFeeds/fundingSource
#Business Case	                       https://itdashboard.gov/api/v1/ITDB2/dataFeeds/businessCase
#Technical Solution Requirements	     https://itdashboard.gov/api/v1/ITDB2/dataFeeds/technicalSolutions
#Technical Solution Requirements URLs  https://itdashboard.gov/api/v1/ITDB2/dataFeeds/technicalSolutionsURLs
#Investments Eliminated or Reduced     https://itdashboard.gov/api/v1/ITDB2/dataFeeds/investmentsEliminated
#Investment Related URLs	             https://itdashboard.gov/api/v1/ITDB2/dataFeeds/investmentRelatedURLs
#Life Cycle Costs	                     https://itdashboard.gov/api/v1/ITDB2/dataFeeds/lifeCycleCosts
#Contracts	                           https://itdashboard.gov/api/v1/ITDB2/dataFeeds/contracts
##CIO Evaluation History	             https://itdashboard.gov/api/v1/ITDB2/dataFeeds/cioRating
#Projects	                             https://itdashboard.gov/api/v1/ITDB2/dataFeeds/projects
#Activities	                           https://itdashboard.gov/api/v1/ITDB2/dataFeeds/activities
#Performance Metrics	                 https://itdashboard.gov/api/v1/ITDB2/dataFeeds/performance
#Performance Metric Actuals	           https://itdashboard.gov/api/v1/ITDB2/dataFeeds/performanceActual
#Investment Baseline History	         https://itdashboard.gov/api/v1/ITDB2/dataFeeds/investmentBaselineHistory
#Investment Trends	                   https://itdashboard.gov/api/v1/ITDB2/dataFeeds/investmentTrends
#Ref - Agency	                         https://itdashboard.gov/api/v1/ITDB2/dataFeeds/agency
#Ref - Bureau                          https://itdashboard.gov/api/v1/ITDB2/dataFeeds/bureau
#Ref - BRM Codes	                     https://itdashboard.gov/api/v1/ITDB2/dataFeeds/brmCode
#Ref - ASO/APG	                       https://itdashboard.gov/api/v1/ITDB2/dataFeeds/asoapg






library(tidyverse)

setwd("~/Vision/2019/IT B&M")

bold_2019_data <- read_csv("2019 IT Portfolio for R merging.csv")
unbold_2018_data <- read_csv("2018 IT Portfolio for R merging.csv")

full_match_bold <- read_csv("IT dashboard 2018 tests - Full Match Bold.csv")
full_match_unbold<- read_csv("IT dashboard 2018 tests - Full Match Unbold.csv")
no_match_bold <- read_csv("IT dashboard 2018 tests - No Match Bold.csv")
no_match_unbold<- read_csv("IT dashboard 2018 tests - No Match Unbold.csv")
same_name_bold <- read_csv("IT dashboard 2018 tests - Same Name Bold.csv")
same_name_unbold<- read_csv("IT dashboard 2018 tests - Same Name Unbold.csv")
similar_name_bold <- read_csv("IT dashboard 2018 tests - Similar Name Bold.csv")
similar_name_unbold<- read_csv("IT dashboard 2018 tests - Similar Name Unbold.csv")
same_number_bold <- read_csv("IT dashboard 2018 tests - Same Number Bold.csv")
same_number_unbold<- read_csv("IT dashboard 2018 tests - Same Number Unbold.csv")

colnames(unbold_2018_data)
full_match_bold_merge <- left_join(full_match_bold, select(bold_2019_data, c(investmentTitle, totalITspendingPY,
                                                                             DME_PY_AgencyFunding, DME_PY_Contributions,
                                                                             OM_PY_AgencyFunding, OM_PY_Contributions,
                                                                             agencyName, sharedServicesCategory,
                                                                             missionSupportInvestmentCategory,
                                                                             bureauName, partOfITPortfolio,
                                                                             typeOfInvestment, 
                                                                             nationalSecuritySystemsIdentifier,
                                                                             infrastructureManagementCategory,
                                                                             feaBRMservicesPrimaryServiceArea,
                                                                             derivedStatus)))

full_match_unbold_merge <- left_join(full_match_unbold, select(unbold_2018_data, c(investmentTitle, totalITspendingCY,
                                                                                   DME_CY_AgencyFunding, DME_CY_Contributions,
                                                                                   OM_CY_AgencyFunding, OM_CY_Contributions,
                                                                                   agencyName, sharedServicesCategory,
                                                                                   bureauName, partOfITPortfolio,
                                                                                   typeOfInvestment, 
                                                                                   nationalSecuritySystemsIdentifier,
                                                                                   infrastructureManagementCategory,
                                                                                   feaBRMservicesPrimaryServiceArea,
                                                                                   derivedStatus)))

no_match_bold_merge <- left_join(no_match_bold, select(bold_2019_data, c(investmentTitle, totalITspendingPY,
                                                                         DME_PY_AgencyFunding, DME_PY_Contributions,
                                                                         OM_PY_AgencyFunding, OM_PY_Contributions,
                                                                         agencyName, sharedServicesCategory,
                                                                         missionSupportInvestmentCategory,
                                                                         bureauName, partOfITPortfolio,
                                                                         typeOfInvestment, 
                                                                         nationalSecuritySystemsIdentifier,
                                                                         infrastructureManagementCategory,
                                                                         feaBRMservicesPrimaryServiceArea,
                                                                         derivedStatus)))

no_match_unbold_merge <- left_join(no_match_unbold, select(unbold_2018_data, c(investmentTitle, totalITspendingCY,
                                                                               DME_CY_AgencyFunding, DME_CY_Contributions,
                                                                               OM_CY_AgencyFunding, OM_CY_Contributions,
                                                                               agencyName, sharedServicesCategory,
                                                                               bureauName, partOfITPortfolio,
                                                                               typeOfInvestment, 
                                                                               nationalSecuritySystemsIdentifier,
                                                                               infrastructureManagementCategory,
                                                                               feaBRMservicesPrimaryServiceArea,
                                                                               derivedStatus)))

same_name_bold_merge <- left_join(same_name_bold, select(bold_2019_data, c(investmentTitle, totalITspendingPY,
                                                                           DME_PY_AgencyFunding, DME_PY_Contributions,
                                                                           OM_PY_AgencyFunding, OM_PY_Contributions,
                                                                           agencyName, sharedServicesCategory,
                                                                           missionSupportInvestmentCategory,
                                                                           bureauName, partOfITPortfolio,
                                                                           typeOfInvestment, 
                                                                           nationalSecuritySystemsIdentifier,
                                                                           infrastructureManagementCategory,
                                                                           feaBRMservicesPrimaryServiceArea,
                                                                           derivedStatus)))

same_name_unbold_merge <- left_join(same_name_unbold, select(unbold_2018_data, c(investmentTitle, totalITspendingCY,
                                                                                 DME_CY_AgencyFunding, DME_CY_Contributions,
                                                                                 OM_CY_AgencyFunding, OM_CY_Contributions,
                                                                                 agencyName, sharedServicesCategory,
                                                                                 bureauName, partOfITPortfolio,
                                                                                 typeOfInvestment, 
                                                                                 nationalSecuritySystemsIdentifier,
                                                                                 infrastructureManagementCategory,
                                                                                 feaBRMservicesPrimaryServiceArea,
                                                                                 derivedStatus)))

similar_name_bold_merge <- left_join(similar_name_bold, select(bold_2019_data, c(investmentTitle, totalITspendingPY,
                                                                                 DME_PY_AgencyFunding, DME_PY_Contributions,
                                                                                 OM_PY_AgencyFunding, OM_PY_Contributions,
                                                                                 agencyName, sharedServicesCategory,
                                                                                 missionSupportInvestmentCategory,
                                                                                 bureauName, partOfITPortfolio,
                                                                                 typeOfInvestment, 
                                                                                 nationalSecuritySystemsIdentifier,
                                                                                 infrastructureManagementCategory,
                                                                                 feaBRMservicesPrimaryServiceArea,
                                                                                 derivedStatus)))

similar_name_unbold_merge <- left_join(similar_name_unbold, select(unbold_2018_data, c(investmentTitle, totalITspendingCY,
                                                                                       DME_CY_AgencyFunding, DME_CY_Contributions,
                                                                                       OM_CY_AgencyFunding, OM_CY_Contributions,
                                                                                       agencyName, sharedServicesCategory,
                                                                                       bureauName, partOfITPortfolio,
                                                                                       typeOfInvestment, 
                                                                                       nationalSecuritySystemsIdentifier,
                                                                                       infrastructureManagementCategory,
                                                                                       feaBRMservicesPrimaryServiceArea,
                                                                                       derivedStatus)))

same_number_bold_merge <- left_join(same_number_bold, select(bold_2019_data, c(investmentTitle, totalITspendingPY,
                                                                               DME_PY_AgencyFunding, DME_PY_Contributions,
                                                                               OM_PY_AgencyFunding, OM_PY_Contributions,
                                                                               agencyName, sharedServicesCategory,
                                                                               missionSupportInvestmentCategory,
                                                                               bureauName, partOfITPortfolio,
                                                                               typeOfInvestment, 
                                                                               nationalSecuritySystemsIdentifier,
                                                                               infrastructureManagementCategory,
                                                                               feaBRMservicesPrimaryServiceArea,
                                                                               derivedStatus)))

same_number_unbold_merge <- left_join(same_number_unbold, select(unbold_2018_data, c(investmentTitle, totalITspendingCY,
                                                                                     DME_CY_AgencyFunding, DME_CY_Contributions,
                                                                                     OM_CY_AgencyFunding, OM_CY_Contributions,
                                                                                     agencyName, sharedServicesCategory,
                                                                                     bureauName, partOfITPortfolio,
                                                                                     typeOfInvestment, 
                                                                                     nationalSecuritySystemsIdentifier,
                                                                                     infrastructureManagementCategory,
                                                                                     feaBRMservicesPrimaryServiceArea,
                                                                                     derivedStatus)))


write.csv(full_match_bold_merge, "full_match_bold_merge.csv")
write.csv(full_match_unbold_merge, "full_match_unbold_merge.csv")
write.csv(no_match_bold_merge, "no_match_bold_merge.csv")
write.csv(no_match_unbold_merge, "no_match_unbold_merge.csv")
write.csv(same_name_bold_merge, "same_name_bold_merge.csv")
write.csv(same_name_unbold_merge, "same_name_unbold_merge.csv")
write.csv(same_number_bold_merge, "same_number_bold_merge.csv")
write.csv(same_number_unbold_merge, "same_number_unbold_merge.csv")
write.csv(similar_name_bold_merge, "similar_name_bold_merge.csv")
write.csv(similar_name_unbold_merge, "similar_name_unbold_merge.csv")

