library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 7")

#read in data
reviews<-read.csv("drs_tests.csv")

ump<-reviews %>% count(Umpire,Result) 

ggplot(ump,aes(x=Umpire,y=n,fill=Result))+geom_col()

ggsave("umpires.png")

over<-reviews %>% mutate(Over2=floor(Over)) %>% count(Over2)

ggplot(over,aes(x=Over2,y=n))+geom_col()

ggsave("overs.png")
