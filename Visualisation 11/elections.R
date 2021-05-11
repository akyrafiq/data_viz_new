library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 11")

elections<-read_csv("1918_2019election_results.csv")

elections_long<-elections %>% reshape2::melt(id=c("election","constituency"))

elections_share<-elections_long %>% 
                 filter(variable %in% c("con_share","lib_share","lab_share",
                                        "natSW_share","oth_share"))
elections_share<-elections_share[complete.cases(elections_share),]

library(trelliscopejs)
ggplot(elections_share,aes(x=election,y=value,fill=variable))+
geom_area()+
facet_grid(rows = ~constituency)
  
facet_trelliscope(facets = ~constituency)
