library(scales)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
options(scipen = 999)

data<-read_csv("C:/Users/Roth/Documents/Other Requests (Co-workers)/Alan/AI/Productvservice bgov AI.csv")



 ggplot(data, aes(fill = Agency,
                                 x = Year,
                                 y = `$`))+
  geom_bar(stat = "identity") +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  #scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  #labs(y = "Contract Obligations (in Millions)", title = "DLA",
   #    subtitle = "FY15-FY18", x = NULL)+
  facet_grid(~PorS, labeller = label_wrap_gen(10), scales = "free")
  # theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
  #       axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       panel.background = element_blank(),
  #       plot.background = element_rect(colour = NA, fill = NA),
  #       # panel.border = element_rect(colour = NA, fill = NA),
  #       axis.title = element_text(face = "bold",size = 14),
  #       axis.title.y = element_text(angle=90,vjust =2),
  #       axis.line = element_line(colour="#bcbcbc"),
  #       axis.ticks.y = element_line(),
  #       axis.text.y = element_text(size = 12, face = "bold"),
  #       panel.grid.major = element_line(colour="#f0f0f0"),
  #       panel.grid.minor = element_line(colour="#f0f0f0"),
  #       # legend.key = element_rect(colour = NA),
  #       # legend.position = "bottom",
  #       # legend.direction = "horizontal",
  #       # legend.key.size= unit(0.2, "cm"),
  #       # legend.margin = unit(0, "cm"),
  #       legend.title = element_text(face="bold"),
  #       #legend.text = element_text(size = 11),
  #       #plot.margin=unit(c(10,5,5,5),"lines"),
  #       strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
  #       strip.text.x = element_text(face="bold", size = 13),
  #       panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  # )+
  # #guides(fill = guide_legend(reverse = TRUE))
  # guides(fill = FALSE)
 
 
 data.sub <- data %>% 
   filter(Agency %in% c( "Department of Defense (DOD)"                         
                         ,"National Aeronautics and Space Administration (NASA)"
                         ,"Department of the Treasury (TREAS)"                  
                         ,"Department of Health and Human Services (HHS)"       
                        ,"Department of Homeland Security (DHS)"               
                          ,"Department of Veterans Affairs (VA)"                 
                         ,"Department of State (DOS)"                           
                ))
 
 ggplot(data.sub, aes(
                  x = Year,
                  y = `$`))+
   geom_bar(stat = "identity", position = position_dodge()) +
   #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
   #scale_fill_manual(values = cc) +
   #scale_y_continuous(labels = dollar) +
   #labs(y = "Contract Obligations (in Millions)", title = "DLA",
   #    subtitle = "FY15-FY18", x = NULL)+
   facet_grid(~Agency, labeller = label_wrap_gen(10), scales = "free")
 # theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
 #       plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
 #       axis.text.x = element_blank(),
 #       axis.ticks.x = element_blank(),
 #       panel.background = element_blank(),
 #       plot.background = element_rect(colour = NA, fill = NA),
 #       # panel.border = element_rect(colour = NA, fill = NA),
 #       axis.title = element_text(face = "bold",size = 14),
 #       axis.title.y = element_text(angle=90,vjust =2),
 #       axis.line = element_line(colour="#bcbcbc"),
 #       axis.ticks.y = element_line(),
 #       axis.text.y = element_text(size = 12, face = "bold"),
 #       panel.grid.major = element_line(colour="#f0f0f0"),
 #       panel.grid.minor = element_line(colour="#f0f0f0"),
 #       # legend.key = element_rect(colour = NA),
 #       # legend.position = "bottom",
 #       # legend.direction = "horizontal",
 #       # legend.key.size= unit(0.2, "cm"),
 #       # legend.margin = unit(0, "cm"),
 #       legend.title = element_text(face="bold"),
 #       #legend.text = element_text(size = 11),
 #       #plot.margin=unit(c(10,5,5,5),"lines"),
 #       strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
 #       strip.text.x = element_text(face="bold", size = 13),
 #       panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
 # )+
 # #guides(fill = guide_legend(reverse = TRUE))
 # guides(fill = FALSE)