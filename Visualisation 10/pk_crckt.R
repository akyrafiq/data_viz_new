library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 10")

wasim<-read.csv("wasim.csv")
waqar<-read.csv("waqar.csv")
yasir<-read.csv("yasir.csv")

ggplot(wasim,aes(x=as.factor(X),y=cumsum(as.numeric(Wkts))))+
geom_line()+
geom_line(data=waqar,aes(x=as.factor(X),y=cumsum(as.numeric(Wkts))))+
geom_line(data=yasir,aes(x=as.factor(X),y=cumsum(as.numeric(Wkts))))+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
