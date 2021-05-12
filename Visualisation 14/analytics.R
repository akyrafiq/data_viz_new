library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 14")

gbw<-read_csv("gbw.csv")
gbw$Update<-lubridate::dmy(gbw$Update)
gbw<-gbw %>% filter(Update==max(gbw$Update))


if (max(gbw$Update)==Sys.Date()){
  print(paste0("We already have the Analytics output for today- open the files plots/",Sys.Date(),"_views.png and plots/",
        Sys.Date(),"_instagram.png to view the results.",
        "Please update tommorrow :)"))
} else{
gbw_views<-gbw %>%
           reshape2::melt(id="Episode") %>%
           filter(variable %in% c("yt_views","fb_views"))

gbw_views<-gbw_views[complete.cases(gbw_views),]

ggplot(gbw_views,aes(x=Episode,y=as.numeric(value),fill=variable,label=value))+
geom_col()+
geom_text(size = 3, position = position_stack(vjust = 0.5))+
ylab("Total Views")+
ggtitle(paste0("Going Beyond Wolverhampton Views as of ",format(Sys.Date(), "%d/%m/%Y")))+
theme_tufte()

ggsave(paste0("plots/",Sys.Date(),"_views.png"))

ggplot(gbw,aes(x=Episode,fill=Episode,label=insta_imp))+
geom_col(aes(y=insta_like))+
geom_col(aes(y=insta_reach),alpha=0.5)+
geom_col(aes(y=insta_imp),alpha=0.1)+
theme_tufte()+
geom_text(size = 3,aes(y=insta_imp),position = position_dodge(width = 0.5))+
ylab("Instagram Impressions")+
  ggtitle(paste0("Going Beyond Wolverhampton Instagram Activity as of ",format(Sys.Date(), "%d/%m/%Y")))+
theme(legend.position = "none")

ggsave(paste0("plots/",Sys.Date(),"_instagram.png"))}
