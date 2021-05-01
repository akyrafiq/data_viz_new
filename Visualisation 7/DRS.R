library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 7")

#read in data
reviews<-read.csv("drs_tests.csv")

reviews[reviews$Umpire=="RA Kattleborough ",]$Umpire<-"RA Kettleborough"
ump<-reviews %>%
     count(Umpire,Result) %>%
     left_join(reviews %>%
               count(Umpire) %>%
               rename(Games=n)) %>% 
     mutate(Prop=n/Games) %>% 
     rename(Num=n)

ggplot(ump %>% filter(Games>20),aes(x=reorder(reorder(Umpire,Result),-Num),y=Prop*100,fill=Result))+
  geom_col()+
  theme_calc()+
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="Which umpires fair best against DRS?", 
       subtitle="The outcome of reviews against various umpires (Min. 20 reviews)")
  

ggsave("umpires.png")

over<-reviews %>% mutate(Over2=floor(Over)) %>% count(Over2)

ggplot(over,aes(x=Over2,y=n))+geom_col()

ggsave("overs.png")
