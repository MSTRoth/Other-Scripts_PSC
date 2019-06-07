library(scales)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
options(scipen = 999)


data <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/USA Spending Navy FY15-FY18.csv")

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

agency_chart <- data %>% 
  rename("PSC" = "product_or_service_code",
         "transaction_value" = "federal_action_obligation",
         fiscal_year = "FY Year") %>% 
  #filter(str_detect(`BGOVMarkets`,"Health Information Technology")) %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% ##include if only Services
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000)

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=5))


plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = "Department of the Navy",
       subtitle = "FY15-FY18", x = NULL)+
  facet_grid(~DPAP_category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
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
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)


plot


ggsave("NAVY DPAP FY15-FY18 w products.jpg", plot,
       width = 15, height = 7, units = "in") 



##################################################################################



agency_chart <- data %>% 
  rename("PSC" = "product_or_service_code",
         "transaction_value" = "federal_action_obligation",
         fiscal_year = "FY Year") %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% ##include if only Services
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(P.S, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000)

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=5))


plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = "Department of the Navy",
       subtitle = "FY15-FY18", x = NULL)+
  facet_grid(~P.S, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
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
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)


plot


ggsave("NAVY DPAP FY15-FY18 P.S.jpg", plot,
       width = 15, height = 7, units = "in")

##############################################################################################

data1 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVAIR FY15-18.csv")
data2 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVSEA FY15-18.csv")
data3 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVSUP FY15-18.csv")
data4 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_SPAWAR FY15-18.csv")
data5 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_USMC FY15-18.csv")
data5.1 <- data5 %>% 
  filter(`Funding Office Level 4` == "Marine Corps Systems Command (MCSC)")
data6 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVFAC FY15-18.csv")

data <- bind_rows("NAVAIR" = data1, "NAVSEA" = data2, "NAVSUP" = data3, 
                  "SPAWAR" = data4, "MCSC" = data5.1, "NAVFAC" = data6, .id = "id")

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>%
  select(id, transaction_value, fiscal_year) %>%
  #filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  group_by(id, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000)

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$id <- factor(agency_chart$id, levels = c("NAVSEA","NAVAIR","SPAWAR","NAVFAC", "NAVSUP", "MCSC"))                                            

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=4))



plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = "US Navy Systems Commands",
       subtitle = "FY15-FY18", x = NULL)+
  facet_grid(~id, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
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
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)


plot


ggsave("Navy Systems Commands FY15-18.jpg", plot,
       width = 15, height = 7, units = "in") 


#______________________________

##DLA####

data<- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_DLA_PSC_15-18.csv")

data_for_join <- data %>% 
  gather("fiscal_year", "transaction_value", `2015`:`2018`)

data_for_join$transaction_value <- as.numeric(data_for_join$transaction_value)

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

`%!in%` = Negate(`%in%`)

agency_chart <- data_for_join %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  filter(!is.na(transaction_value)) %>% 
 # filter(PSC %!in% c("9130", "9140", "9150", "9135", "9160", "9110")) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products" & PSC %in% 
                                  c("9130", "9140", "9150", "9135", "9160", "9110"), "Fuel Products", 
                                ifelse(P.S == "Products" & PSC %!in% 
                                         c("9130", "9140", "9150", "9135", "9160", "9110"), "Other Products", DPAP))) %>% 
  filter(P.S %!in% c("Products", "Fuel Products")) %>% ##include if only Services
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000)

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$DPAP_category <- factor(agency_chart$DPAP_category, levels = c("Construction Services",
                                                                "Electronic & Communication Services",
                                                                "Equipment Related Services",
                                                                "Facility Related Services",
                                                                "Knowledge Based Services",
                                                                "Logistics Management Services",
                                                                "Medical Services",
                                                                "Research and Development",
                                                                "Transportation Services",
                                                                "Fuel Products", "Other Products"))
  
  
cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=11))


plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = "DLA",
       subtitle = "FY15-FY18", x = NULL)+
  facet_grid(~DPAP_category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
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
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)


plot


ggsave("DLA DPAP FY15-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 

ggsave("DLA DPAP FY15-FY18 wo fuel wo products.jpg", plot,
       width = 15, height = 7, units = "in")



#########################
##9 color charts ####

library(scales)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
options(scipen = 999)


data <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/USA Spending Navy FY15-FY18.csv")

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")



agency_chart <- data %>% 
  rename("PSC" = "product_or_service_code",
         "transaction_value" = "federal_action_obligation",
         fiscal_year = "FY Year") %>% 
  #filter(str_detect(`BGOVMarkets`,"Health Information Technology")) %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% ##include if only Services
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000000) %>% 
  group_by(fiscal_year) %>%
  arrange(desc(DPAP_category)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(DPAP_category=="Products","Product","Service")) %>%
  group_by(fiscal_year, pors) %>%
  mutate(`pors$` = sum(sum))

agency_chart$DPAP_category <- factor(agency_chart$DPAP_category,
                                     levels = c("Products", "Construction Services",
                                                "Electronic & Communication Services",
                                                "Equipment Related Services",
                                                "Facility Related Services",
                                                "Knowledge Based Services",
                                                "Logistics Management Services",
                                                "Medical Services",
                                                "Research and Development",
                                                "Transportation Services"),
                                     ordered = is.ordered(agency_chart$DPAP_category))
agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)

label_height_all <- agency_chart %>%
  group_by(fiscal_year, pors) %>%
  summarize(`pors$` = sum(sum)) %>%
  group_by(fiscal_year) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(agency_chart, by = c("fiscal_year", "pors", "pors$"))

plot_all <- ggplot(label_height_all, aes(x = fiscal_year, y = sum,
                                         fill = DPAP_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = fiscal_year, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_all, DPAP_category %in% c("Medical Services", "Products")), aes(x=fiscal_year, y=`pors$`), colour = "black", fill = NA, stat = "identity"
  )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)",
       title = paste("Navy", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 

ggsave("NAVY DPAP FY15-FY18 by FY.jpg", plot_all,
       width = 15, height = 7, units = "in") 



##############################################################################################

data1 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVAIR FY15-18.csv")
data2 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVSEA FY15-18.csv")
data3 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVSUP FY15-18.csv")
data4 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_SPAWAR FY15-18.csv")
data5 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_USMC FY15-18.csv")
data5.1 <- data5 %>% 
  filter(`Funding Office Level 4` == "Marine Corps Systems Command (MCSC)")
data6 <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVFAC FY15-18.csv")

data <- bind_rows("NAVAIR" = data1, "NAVSEA" = data2, "NAVSUP" = data3, 
                  "SPAWAR" = data4, "MCSC" = data5.1, "NAVFAC" = data6, .id = "id")

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")

agency_chart <- data %>% 
  rename(transaction_value = "Transaction Value",
         fiscal_year = "Fiscal Year",
         PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>%
  select(id, PSC, transaction_value, fiscal_year) %>%
  #filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% ##include if only Services
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") 

systems_commands <- split(agency_chart, agency_chart$id)


agency_chart2<-lapply(systems_commands, function(x) {
  agency_chart2<- x %>%
    group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
    summarise(sum = sum(transaction_value)/1000000000) %>% 
    group_by(fiscal_year) %>%
    arrange(desc(DPAP_category)) %>%
    mutate(pors = ifelse(DPAP_category=="Products","Product","Service")) %>%
    group_by(fiscal_year, pors) %>%
    mutate(`pors$` = sum(sum))
  
})

label_height_command <- lapply(agency_chart2, function(x) {
  x$fiscal_year <- as.character(x$fiscal_year)
  
  x$DPAP_category <- factor(x$DPAP_category,
                            levels = c("Products", "Construction Services",
                                       "Electronic & Communication Services",
                                       "Equipment Related Services",
                                       "Facility Related Services",
                                       "Knowledge Based Services",
                                       "Logistics Management Services",
                                       "Medical Services",
                                       "Research and Development",
                                       "Transportation Services"),
                            ordered = is.ordered(x$DPAP_category))
  
  label_height_command <- x %>%
    group_by(fiscal_year, pors) %>%
    summarize(`pors$` = sum(sum)) %>%
    group_by(fiscal_year) %>%
    arrange(desc(`pors`)) %>%
    mutate(label_y2 = cumsum(`pors$`)) %>%
    left_join(x, by = c("fiscal_year", "pors", "pors$"))
  
})


plot_command <- lapply(label_height_command, function(xy) {
  plot_command <- ggplot(xy, aes(x = fiscal_year, y = sum,
                                 fill = DPAP_category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = fiscal_year, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5)+
    geom_bar(data = subset(xy, DPAP_category %in% c("Transportation Services", "Products")), 
             aes(x=fiscal_year, y=`pors$`), colour = "black", fill = NA, stat = "identity"
    )+
    scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
    labs(x="Fiscal Year", y = "Contract Obligations (in Billions)",
         title = paste("Navy Systems Commands", " Contract Spending", sep = ""))+
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 
  
})

lapply(names(plot_command), function(x) {
  
  ggsave(filename = paste(x," Navy System Commands FY2015-18.jpg", sep = ""), plot=plot_command[[x]],
         width = 13, height = 6.5, units = "in")
  
  
})







#______________________________

##DLA####

data<- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_DLA_PSC_15-18.csv")

data_for_join <- data %>% 
  gather("fiscal_year", "transaction_value", `2015`:`2018`)

data_for_join$transaction_value <- as.numeric(data_for_join$transaction_value)

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

`%!in%` = Negate(`%in%`)

agency_chart <- data_for_join %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  filter(!is.na(transaction_value)) %>% 
  # filter(PSC %!in% c("9130", "9140", "9150", "9135", "9160", "9110")) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products" & PSC %in% 
                                  c("9130", "9140", "9150", "9135", "9160", "9110"), "Fuel Products", 
                                ifelse(P.S == "Products" & PSC %!in% 
                                         c("9130", "9140", "9150", "9135", "9160", "9110"), "Other Products", DPAP))) %>% 
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000000) %>% 
  group_by(fiscal_year) %>%
  arrange(desc(DPAP_category)) %>%
  mutate(pors = ifelse(DPAP_category=="Other Products","Other Products", ifelse(DPAP_category == "Fuel Products", "Fuel Products", "Service"))) %>%
  group_by(fiscal_year, pors) %>%
  mutate(`pors$` = sum(sum))

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$DPAP_category <- factor(agency_chart$DPAP_category, levels = c("Fuel Products", "Other Products",
                                                                            "Construction Services",
                                                                            "Electronic & Communication Services",
                                                                            "Equipment Related Services",
                                                                            "Facility Related Services",
                                                                            "Knowledge Based Services",
                                                                            "Logistics Management Services",
                                                                            "Medical Services",
                                                                            "Research and Development",
                                                                            "Transportation Services"))


label_height_all <- agency_chart %>%
  group_by(fiscal_year, pors) %>%
  summarize(`pors$` = sum(sum)) %>%
  group_by(fiscal_year) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(agency_chart, by = c("fiscal_year", "pors", "pors$"))

plot_DLA <- ggplot(label_height_all, aes(x = fiscal_year, y = sum,
                                         fill = DPAP_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = fiscal_year, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_all, DPAP_category %in% c("Medical Services", "Fuel Products", "Other Products")), aes(x=fiscal_year, y=`pors$`), colour = "black", fill = NA, stat = "identity"
  )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)",
       title = paste("DLA", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 


plot_DLA

ggsave("DLA DPAP FY15-FY18 by FY.jpg", plot_DLA,
       width = 15, height = 7, units = "in") 

##DLA####

data<- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_NAVSEA FY15-18.csv")


setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

`%!in%` = Negate(`%in%`)

agency_chart <- data %>% 
  rename(fiscal_year = `Fiscal Year`, 
         transaction_value = `Transaction Value`,
         PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  filter(!is.na(transaction_value)) %>% 
  # filter(PSC %!in% c("9130", "9140", "9150", "9135", "9160", "9110")) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000000) %>% 
  group_by(fiscal_year) %>%
  arrange(desc(DPAP_category)) %>%
  mutate(pors = ifelse(DPAP_category=="Products","Product","Service")) %>%
  group_by(fiscal_year, pors) %>%
  mutate(`pors$` = sum(sum))

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$DPAP_category <- factor(agency_chart$DPAP_category, levels = c("Products",
                                                                            "Construction Services",
                                                                            "Electronic & Communication Services",
                                                                            "Equipment Related Services",
                                                                            "Facility Related Services",
                                                                            "Knowledge Based Services",
                                                                            "Logistics Management Services",
                                                                            "Medical Services",
                                                                            "Research and Development",
                                                                            "Transportation Services"))


label_height_all <- agency_chart %>%
  group_by(fiscal_year, pors) %>%
  summarize(`pors$` = sum(sum)) %>%
  group_by(fiscal_year) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(agency_chart, by = c("fiscal_year", "pors", "pors$"))

plot_DLA <- ggplot(label_height_all, aes(x = fiscal_year, y = sum,
                                         fill = DPAP_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = fiscal_year, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_all, DPAP_category %in% c("Medical Services", "Products")), aes(x=fiscal_year, y=`pors$`), colour = "black", fill = NA, stat = "identity"
  )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)",
       title = paste("NAVSEA", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 


plot_DLA

ggsave("NAVSEA DPAP FY15-FY18 by FY.jpg", plot_DLA,
       width = 15, height = 7, units = "in") 
