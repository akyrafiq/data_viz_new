library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz_2021/Visualisation 7")

#read in data
reviews<-read.csv("drs_tests.csv")

library(lubridate)
reviews$StartDate<-ymd(reviews$StartDate)
reviews$Year<-year(reviews$StartDate)
reviews$Match<-paste0(reviews$Team1," vs ",reviews$Team2,", ",
                      reviews$Ground," ",reviews$Year)

#Visualise impire success rate
reviews[reviews$Umpire=="RA Kattleborough ",]$Umpire<-"RA Kettleborough"
ump<-reviews %>%
     count(Umpire,Result) %>%
     left_join(reviews %>%
               count(Umpire) %>%
               rename(Games=n)) %>% 
     mutate(Prop=n/Games) %>% 
     rename(Num=n)

ggplot(ump %>% filter(Games>20),aes(x=reorder(reorder(Umpire,Result),-Num),y=Prop*100,fill=Result,label=paste0(round(Prop*100,0),"%")))+
  geom_col()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="Which umpires fair best against DRS?", 
       subtitle="The outcome of reviews against various umpires (Min. 20 reviews)")
  

ggsave("umpires.png")

old_plot<-ggplot(ump,aes(x=Umpire,y=Num,fill=Result))+geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
old_plot
ggsave("umpires_old.png")


#Visualise impire success rate
team<-reviews %>%
  count(Team,Result) %>%
  left_join(reviews %>%
              count(Team) %>%
              rename(Games=n)) %>% 
  mutate(Prop=n/Games) %>% 
  rename(Num=n)

ggplot(team,aes(x=Team,y=Prop*100,fill=Result,label=paste0(round(Prop*100,0),"%")))+
  geom_col()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,105)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="Which countries are the best at using DRS?", 
       subtitle="The outcome of reviews by each country")


ggsave("countries.png")


#players
batsmen<-reviews %>% count(Batsman,Result,TeamState) %>% left_join(reviews %>% count(Batsman,TeamState) %>% rename(Reviews=n) )

ggplot(batsmen %>% filter(TeamState=="Bowling") %>% filter(Reviews>17),aes(x=reorder(Batsman,-Reviews),y=n,fill=Result,label=n))+
  geom_col()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,40)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="Which batsman has the most reviews against them?", 
       subtitle="The amount of reviews used against a batsman")

ggsave("bat_ag.png")

ggplot(batsmen %>% filter(TeamState=="Batting") %>% filter(Reviews>12),aes(x=reorder(Batsman,-Reviews),y=n,fill=Result,label=n))+
  geom_col()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,30)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="Which batsman like to review the most?", 
       subtitle="The amount of reviews used by each batsman")

ggsave("bat_for.png")

#time of reviews

days<-reviews %>% count(Day,Result) %>% left_join(reviews %>% count(Day) %>% rename(Reviews=n) )

ggplot(days,aes(x=Day,y=n,fill=Result))+
  geom_col()+
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="Which batsman like to review the most?", 
       subtitle="The amount of reviews used by each batsman")

#Days appear to eb a bit wrong for several cases
# Eg see reviews %>% filter(MatchID==913645)

over<-reviews %>% mutate(Over2=floor(Over)) 

ggplot(over,aes(x=Over2, fill=Result))+
  geom_histogram(binwidth = 10)+
  theme_fivethirtyeight()+
  geom_vline(linetype="dashed",aes(xintercept =75))+
  geom_text(angle=90,aes(x = 80,y=350,label="2nd New Ball Available"))+
  ylim(0,450)+
  xlab("Umpire")+
  ylab("Probability")+
  labs(title="When are reviews taken in an innings?", 
       subtitle="Number of reviews taken in each 10 over interval")

ggsave("overs.png")

#Which matches had the most reviews?
match<-reviews %>%
       count(MatchID,Match,Result) %>% 
       left_join(reviews %>% count(MatchID,Match) %>% rename(Reviews=n))

ggplot(match %>% filter(Reviews>15),aes(x=reorder(Match,-Reviews),y=n,fill=Result,label=n))+
  geom_bar(stat = "identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,30)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  # geom_text(data=reviews %>% count(Match) %>% rename(Reviews=n) %>% filter(Reviews>15),
  #           aes(y=Reviews,label=Reviews),size = 6, position = position_stack(vjust = 0.5))+
  labs(title="Which matches had the most reviews?", 
       subtitle="The number of reviews and their outcome in each match")

ggsave("match.png")

#how has it changed over time?
year<-reviews %>%
  count(Year,Result) %>% 
  left_join(reviews %>% count(Year) %>% rename(Reviews=n)) %>% 
  mutate(Prop=n/Reviews)

ggplot(year,aes(x=as.factor(Year),y=n,fill=Result,label=n))+
  geom_bar(stat = "identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,550)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  # geom_text(data=reviews %>% count(Match) %>% rename(Reviews=n) %>% filter(Reviews>15),
  #           aes(y=Reviews,label=Reviews),size = 6, position = position_stack(vjust = 0.5))+
  labs(title="Which year had the most reviews?", 
       subtitle="The number of reviews and their outcome in each year")

ggsave("year.png")

ggplot(year,aes(x=as.factor(Year),y=Prop*100,fill=Result,label=paste0(round(Prop*100,0),"%")))+
  geom_bar(stat = "identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_fivethirtyeight()+
  ylim(0,105)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Umpire")+
  ylab("Probability")+
  # geom_text(data=reviews %>% count(Match) %>% rename(Reviews=n) %>% filter(Reviews>15),
  #           aes(y=Reviews,label=Reviews),size = 6, position = position_stack(vjust = 0.5))+
  labs(title="Have teams got better at using their reviews over time??", 
       subtitle="The results of reviews over the years")

ggsave("year_prop.png")
