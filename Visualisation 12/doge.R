library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 12")

doge<-read_csv("Doge_historical_data.csv")
names(doge)[2]<-"Close"
doge$Date<-lubridate::mdy(doge$Date)
doge$Year<-year(doge$Date)
doge<-doge[order(doge$Date),]

ggplot(doge %>% filter(Year==2021),aes(x=Date,y=Close))+
geom_line()+
geom_line(linetype="dashed",aes(y=High),colour="green4")+
geom_line(linetype="dashed",aes(y=Low),colour="red1")+
theme_wsj()+
ylim(0,0.7)+
theme(legend.position = "none")+
labs(title="The Rise of Dogecoin in 2021",
     subtitle = "Will the bubble burst or will Doge go to the moon?")+
  xlab("Date")+
  ylab("Dogecoin Value (in US$)")

ggsave("doge.png")
