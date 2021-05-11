library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 14")

gbw<-read_csv("gbw.csv")

gbw_views<-gbw %>%
           reshape2::melt(id="Episode") %>%
           filter(variable %in% c("yt_views","fb_views"))

gbw_views<-gbw_views[complete.cases(gbw_views),]

ggplot(gbw_views,aes(x=Episode,y=as.numeric(value),fill=variable))+
geom_col()+
ylab("Total Views")+
theme_tufte()

ggsave(paste0("plots/",Sys.Date(),"_views.png"))

ggplot(gbw,aes(x=Episode,fill=Episode))+
geom_col(aes(y=insta_like))+
geom_col(aes(y=insta_reach),alpha=0.1)+
theme_tufte()+
ylab("Instagram Reach")+
theme(legend.position = "none")

ggsave(paste0("plots/",Sys.Date(),"_instagram.png"))
