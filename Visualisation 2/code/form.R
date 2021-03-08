library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)
library(gganimate)
library(gifski)
library(png)
library(tidyverse)

setwd("~/Documents/GitHub/data_viz/Visualisation 2")

#read in data
files_form<-list.files("data/form/")
tables_form<-lapply(paste0("data/form/",files_form), read.csv)

mls_form<-tables_form[[5]]
epl_form<-tables_form[[3]]
laliga_form<-tables_form[[4]]
bund_form<-tables_form[[2]]
all_tab<-bund_form<-tables_form[[1]]

epl_form_long<-epl_form %>%
               dplyr::select(-Matches) %>%
               reshape2::melt(id=c("Season","Team")) %>% 
               mutate(Points=ifelse(value=="L",0,ifelse(value=="W",3,1)))%>%
               dplyr::select(-value)
lal_form_long<-laliga_form %>%
  dplyr::select(-Matches) %>%
  reshape2::melt(id=c("Season","Team")) %>% 
  mutate(Points=ifelse(value=="L",0,ifelse(value=="W",3,1)))%>%
  dplyr::select(-value)

ggplot(epl_form_long %>% dplyr::filter(Team=="Arsenal") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Manchester United") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Chelsea") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Manchester City") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Liverpool") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Everton") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Tottenham Hotspur") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(epl_form_long %>% dplyr::filter(Team=="Southampton") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season),alpha=.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(lal_form_long %>% dplyr::filter(Team=="Valencia") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(lal_form_long %>% dplyr::filter(Team=="Real Madrid") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(lal_form_long %>% dplyr::filter(Team=="Barcelona") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(lal_form_long %>% dplyr::filter(Team=="Atletico Madrid") %>% group_by(Season) %>%
         mutate(cumsum = cumsum(Points)),aes(x=variable,y=cumsum))+
  geom_point(aes(color=Season))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



ggplot(all_tab %>% filter(League != "Major League Soccer"), aes(GoalsScored, GoalsConceded, size = -Position, color = League)) +
  geom_point() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Variaiton of goals across Seasons', x = 'Goals Scored', y = 'Goals Conceded') +
  transition_states(Season) +
  ease_aes('linear')+
  guides(size = FALSE)+
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
anim_save("anim/goalsv1.gif")
