library(tidyverse)
library(RColorBrewer)

#Location for saving charts
setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/DHS") #location charts are saved

data <- read_csv("X:/1 Marielle Folder/Data Sets/By Agency/DHS/DHS_Components_17-19Q2_byQ.csv")



data.organized <- data %>% 
  gather("fiscal_quarter", "transaction_value", `2017_Q1`:`2019_Q2`) %>% 
  separate(fiscal_quarter, c("fiscal_year", "quarter"), sep = "_") %>% 
  rename("agency_comp" = `Row Labels`)

order <- data.organized %>% 
  group_by(agency_comp) %>%
  summarise(sum = sum(transaction_value)) %>%
  arrange(desc(sum))

order.agency <- order$agency_comp

data.organized$agency_comp <- factor(data.organized$agency_comp, levels = order.agency,
                                     ordered = is.ordered(order.agency))
  

data.DHS <- data.organized %>% 
  filter(agency_comp == "Department of Homeland Security (DHS)") %>% 
  mutate(total_obligations = round((transaction_value)/1000000000, digits=2)) %>% 
  group_by(fiscal_year) %>% 
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations))%>% 
  mutate(FYYear = paste("FY", fiscal_year, sep = ""))



data.components <- data.organized %>% 
  filter(agency_comp != "Department of Homeland Security (DHS)") %>% 
  mutate(total_obligations = round((transaction_value)/1000000000, digits=2)) %>% 
  group_by(fiscal_year, agency_comp) %>% 
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations))%>% 
  mutate(FYYear = paste("FY", fiscal_year, sep = ""))

data.components1 <- data.components %>% 
  filter(agency_comp %in% order.agency[2:7]) %>% 
  filter(total_obligations != 0)

data.components2 <- data.components %>% 
  filter(agency_comp %in% order.agency[8:10]) %>% 
  filter(total_obligations != 0)

data.components3 <- data.components %>% 
  filter(agency_comp %in% order.agency[11:14]) %>% 
  filter(total_obligations != 0)

plot.DHS <- 
  ggplot(data.DHS, aes(x = FYYear, y = total_obligations, fill = factor(quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.DHS, fiscal_year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = fiscal_year),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
  #facet_grid(~agency_comp, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Billions",
       title = "DHS (Total)") +
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))
  
  
  
plot.components <-  
  ggplot(data.components, aes(x = FYYear, y = total_obligations, fill = factor(quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(data.components, fiscal_year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = fiscal_year),
                 geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    facet_wrap(~agency_comp, labeller = label_wrap_gen(10))+
    labs(y = "Contract Obligations (in) Billions",
         title = "Contract Obligations Comparison") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 10), 
          axis.title.x = element_blank(),
         # panel.spacing = unit(4, "lines")
          )

plot.components1 <-   ggplot(data.components1, aes(x = FYYear, y = total_obligations, fill = factor(quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(data.components1, fiscal_year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = fiscal_year),
                 geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    facet_grid(~agency_comp, labeller = label_wrap_gen(20), scales = "free_y")+
    labs(y = "Contract Obligations (in) Billions",
         title = "Contract Obligations Comparison") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
          plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 10), 
          axis.title.x = element_blank(),
          # panel.spacing = unit(4, "lines")
    )
  
plot.components2 <-   ggplot(data.components2, aes(x = FYYear, y = total_obligations, fill = factor(quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(data.components2, fiscal_year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = fiscal_year),
                 geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    facet_wrap(~agency_comp, labeller = label_wrap_gen(50), scales = "free_y")+
    labs(y = "Contract Obligations (in) Billions",
         title = "Contract Obligations Comparison") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 10), 
          axis.title.x = element_blank(),
          # panel.spacing = unit(4, "lines")
    )
  
plot.components3 <-   ggplot(data.components3, aes(x = FYYear, y = total_obligations, fill = factor(quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(data.components3, fiscal_year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = fiscal_year),
                 geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    facet_grid(~agency_comp, labeller = label_wrap_gen(30), scales = "free_y")+
    labs(y = "Contract Obligations (in) Billions") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 10), 
          axis.title.x = element_blank(),
          # panel.spacing = unit(4, "lines")
    )

ggsave(filename = paste("Total DHS"," Contract Obligations FY17-19Q2 by quarter - DHS.jpg", sep = ""), plot.DHS,
       width = 13, height = 6.5, units = "in")
ggsave(filename = paste("DHS Components"," Contract Obligations FY17-19Q2 by quarter - DHS.jpg", sep = ""), plot=plot.components,
       width = 13, height = 6.5, units = "in")
ggsave(filename = paste("DHS Components1"," Contract Obligations FY17-19Q2 by quarter - DHS.jpg", sep = ""), plot=plot.components1,
       width = 14, height = 8.5, units = "in")
ggsave(filename = paste("DHS Components2"," Contract Obligations FY17-19Q2 by quarter - DHS.jpg", sep = ""), plot=plot.components2,
       width = 14, height = 8, units = "in")
ggsave(filename = paste("DHS Components3"," Contract Obligations FY17-19Q2 by quarter - DHS.jpg", sep = ""), plot=plot.components3,
       width = 13, height = 8, units = "in")
  
####################TOp 7####
  #Location for saving charts
  setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/DHS") #location charts are saved
  
  data <- read_csv("X:/1 Marielle Folder/Data Sets/By Agency/DHS/DHS_Components_17-19Q2_byQ.csv")
  
  
  data.organized <- data %>% 
    gather("fiscal_quarter", "transaction_value", `2017_Q1`:`2019_Q2`) %>% 
    separate(fiscal_quarter, c("fiscal_year", "quarter"), sep = "_") %>% 
    rename("agency_comp" = `Row Labels`)
  
  order <- data.organized %>% 
    group_by(agency_comp) %>%
    summarise(sum = sum(transaction_value)) %>%
    arrange(desc(sum)) %>%
    
    
  
  order.agency <- order$agency_comp
  
  data.organized$agency_comp <- factor(data.organized$agency_comp, levels = order.agency,
                                       ordered = is.ordered(order.agency))
  
  
  
  data.topcomponents <- data.organized %>% 
    mutate(total_obligations = round((transaction_value)/1000000000, digits=2)) %>% 
    group_by(fiscal_year, agency_comp) %>% 
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations))%>% 
    mutate(FYYear = paste("FY", fiscal_year, sep = "")) 

top_components <- split(data.topcomponents, data.topcomponents$agency_comp)


plot<-lapply(top_components, function(xy) {
ggplot(xy, aes(x = FYYear, y = total_obligations, fill = factor(quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(xy, fiscal_year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = fiscal_year),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
  facet_wrap(~agency_comp, labeller = label_wrap_gen(40))+
  labs(y = "Contract Obligations (in) Billions") +
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 15), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 11))
  #+ylim(0,4)
 } )
plot

lapply(names(plot), function(x) {
  ggsave(filename = paste(x," Contract Obligations FY17-19Q2 by quarter - DHS.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 6.5, units = "in")
})

