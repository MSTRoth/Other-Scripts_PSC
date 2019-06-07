##Crosswalk PSC to DPAP
### Agency 

#install.packages("scales")
library(tidyverse)
library(scales)
library(RColorBrewer)

data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/Vendor Specific/Prime Industries Company Profile.csv"))
setwd("X:/1 Marielle Folder/Visualizations/Vendor Specific/")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")




with_DPAP <- data %>% 
  rename(PSC = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         transaction_value = "Transaction Value",
         fiscal_year = "Fiscal Year",
         funding_agency = "Funding Agency") %>% 
  select(PSC, transaction_value, fiscal_year, funding_agency, `Funding Bureau`) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #mutate(agency = ifelse(funding_agency == "Department of Defense (DOD)", `Funding Bureau`, funding_agency)) %>% 
  rename(agency = funding_agency) %>% 
  select(transaction_value, fiscal_year, agency, DPAP) %>% 
  group_by(DPAP, fiscal_year, agency) %>% 
  summarise(sum = sum(transaction_value)/1000000) %>% 
  filter(agency != "Department of Veterans Affairs (VA)")
  
  with_DPAP$fiscal_year <- as.character(with_DPAP$fiscal_year)

  
with_DPAP$agency<-factor(with_DPAP$agency, levels = c("Department of Homeland Security (DHS)"
                                                      ,"Department of Agriculture (USDA)"
                                                      ,"General Services Administration (GSA)"
                                                      ,"Department of the Treasury (TREAS)"
                                                      # ,"Department of the Navy (USN)"
                                                      # ,"Other Defense Agencies"
                                                      # ,"Department of the Air Force (USAF)"
                                                      # ,"Department of the Army (USA)"
                                                      ,"Department of Defense (DOD)"
                                                      ))
with_DPAP$DPAP <-factor(with_DPAP$DPAP, levels = c("Facility Related Services", "Knowledge Based Services" 
                                              ,"Equipment Related Services")) 

cc <- scales::seq_gradient_pal("#0D088A", "#DC2D08", "Lab")(seq(0,1,length.out=5))


plot <- ggplot(with_DPAP, aes(fill = agency,
                         x = fiscal_year,
                         y = sum,
                         ))+
    geom_bar(stat = "identity") +
    stat_summary(fun.y = sum, aes(label = round(..y..,3), group = fiscal_year),  
                 geom = "text", vjust = -.5, size = 4, fontface = "bold")+
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
    scale_fill_manual(name = "Department", values = c("#06008C","#0A9BA5","#0BB93B","#F3CC0A","#AF1B00")) +
    #scale_y_continuous(labels = dollar) +
    labs(y = "Contract Obligations (in Millions)", title = "PRIME Industries Obligations by DPAP (in Millions)",
         subtitle = "FY16-FY18", x = NULL)+
    #facet_grid(~DPAP, labeller = label_wrap_gen(10), scales = "free")+
  facet_wrap(DPAP ~ ., scales = "free")+
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 22, face = "bold"),        
          panel.background = element_blank(),
          plot.background = element_rect(colour = NA, fill = NA),
          # panel.border = element_rect(colour = NA, fill = NA),
          axis.title = element_text(face = "bold",size = 14),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.line = element_line(colour="#bcbcbc"),
          axis.ticks = element_line(),
          axis.text = element_text(size = 12, face = "bold"),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_line(colour="#f0f0f0"),
          # legend.key = element_rect(colour = NA),
          # legend.position = "bottom",
          # legend.direction = "horizontal",
          # legend.key.size= unit(0.2, "cm"),
          # legend.margin = unit(0, "cm"),
          legend.title = element_text(face="bold"),
          #legend.text = element_text(size = 11),
          #plot.margin=unit(c(10,5,5,5),"lines"),
          strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
          strip.text.x = element_text(face="bold", size = 13),
          panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
    )



plot


ggsave("PRIME Industries FY16-FY18.jpg", plot,
       width = 15, height = 8, units = "in") 
