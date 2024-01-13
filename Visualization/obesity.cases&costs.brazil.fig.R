
#install.packages(c('readr', 'ggplot2', 'egg', 'data.table', 'tidyverse'))
install.packages('data.frame')

install.packages('data.table')

rm(list=ls())
library(tidyverse)
library(reshape2)
library(gridExtra)
library(scales)
library(egg)
library(data.table)

cases <- read.table(text="Type   SSB  Allbev    
                  Tax1     -2.8        -2.8 
                  Tax2     -3.7        -3.8        ", header=TRUE)

costs <- read.table(text="Type    SSB  Allbev    
                  Tax1     -22.3         -22.6  
                  Tax2    -30.0       -30.3        ", header=TRUE)

casesm <- melt(cases, id.var="Type")
casesm$id <- "cases"

costsm <- melt(costs, id.var="Type")
costsm$id <- "costs"

brazil <- rbind.data.frame(casesm, costsm)

labs <- c("Obesity cases (millions)", 
          "Obesity costs (US billion dollars)")

names(labs) <- c("cases", "costs")



brazil <- data.table(brazil)

brazil[,y_min := value*0.5, by = variable]
brazil[,y_max:= value*1.5, by = variable]


ggbrazil <- ggplot(brazil) + 
  
  theme_bw() +
  
  ggforce::facet_col(~id, scales = "free", labeller = labeller(id = labs)) +
  
  geom_blank(aes(y = y_min)) +
  
  geom_blank(aes(y = y_max)) +
  
  geom_bar(aes(x = Type, y = value, fill = variable, group = variable), 
           stat = "identity", position = "dodge", width = 0.5) + 
  
  
  geom_text(aes(label = sprintf("%0.1f", round(value, digits = 1)),
                x = Type, y = value, group = variable),
            position = position_dodge(0.5), vjust = 1, size = 5.5) +
  
  ylab("") +
  
  scale_fill_manual("Caloric changes", values = c("SSB" = "deepskyblue",
                               "Allbev" = "deepskyblue4"),
                    labels = c("SSB" = "SSB",
                               "Allbev" = "All beverages")) +
  
  scale_x_discrete("Tax", labels = c("Tax1" = "20%", 
                                           "Tax2" = "30%")) +
  
  
  theme(legend.title = element_text(size = 18)) +
 
  theme(legend.text = element_text(size = 18),
        axis.text = element_text(size = 16 ), 
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 19, hjust = 0.5),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(size = 20)) 

ggbrazil



ggsave(filename = "~/Dropbox/Brazil/Brazil Bloomberg/Visualization/obesitycases&costsbrazilT.png", 
       plot = ggbrazil, dpi = 300, width = 10, height = 7)


