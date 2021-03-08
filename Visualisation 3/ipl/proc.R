library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz/Visualisation 3")

#read data
ipl<-read.csv("data/matchesv2.csv")
ipl_del<-read.csv("data/deliveries.csv")

#replace rising pune supergiants
ipl[ipl$winner=="Rising Pune Supergiants",]$winner<-"Rising Pune Supergiant"
ipl[ipl$toss_winner=="Rising Pune Supergiants",]$toss_winner<-"Rising Pune Supergiant"
ipl[ipl$team1=="Rising Pune Supergiants",]$team1<-"Rising Pune Supergiant"
ipl[ipl$team2=="Rising Pune Supergiants",]$team2<-"Rising Pune Supergiant"
ipl_del[ipl_del$batting_team=="Rising Pune Supergiants",]$batting_team<-"Rising Pune Supergiant"
ipl_del[ipl_del$bowling_team=="Rising Pune Supergiants",]$bowling_team<-"Rising Pune Supergiant"

ggplot(ipl %>% filter(win_by_runs != 0), aes(x=win_by_runs)) + 
  geom_histogram(aes(fill=winner), 
                 binwidth = 5 
                 #,col="black"
  ) +  # change binwidth
  labs(title="IPL Wins", 
       subtitle="The margin of victory by runs in the IPL between 2007 and 2019")+ 
  #theme_wsj()+
  theme_classic()+
  xlab("Runs won by")+
  ylab("Number of Games")+
  #scale_fill_manual(values = sc2,guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())

ggsave("hist/win_by_runs_hist.png")

ggplot(ipl %>% filter(win_by_wickets != 0), aes(x=win_by_wickets)) + 
  geom_histogram(aes(fill=winner), 
                 binwidth = 1 
                 #,col="black"
  ) +  # change binwidth
  labs(title="IPL Wins", 
       subtitle="The margin of victory by wickets in the IPL between 2007 and 2019")+ 
  #theme_wsj()+
  theme_classic()+
  xlab("Runs won by")+
  ylab("Number of Games")+
  #scale_fill_manual(values = sc2,guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())

ggsave("hist/win_by_wickets_hist.png")

#calculate distribution
dis_ipl<-data.frame(x=0:1500/10,
                    Runs=dweibull(0:1500/10,
                                  shape=fitdistr(ipl[ipl$win_by_runs!=0,]$win_by_runs, "weibull")$estimate["shape"],
                                  scale=fitdistr(ipl[ipl$win_by_runs!=0,]$win_by_runs, "weibull")$estimate["scale"]),
                    Wickets=dweibull(0:1500/10,
                                     shape=fitdistr(ipl[ipl$win_by_wickets!=0,]$win_by_wickets, "weibull")$estimate["shape"],
                                     scale=fitdistr(ipl[ipl$win_by_wickets!=0,]$win_by_wickets, "weibull")$estimate["scale"])) %>% 
         reshape2::melt(id="x")

#plot
ggplot(dis_ipl %>% filter(variable=="Runs"),aes(x=x,y=value))+
  geom_line()+
  xlab("Margin of Victory")+
  ylab("Probability")+
  labs(title="Distribution of Results", 
       subtitle="Estimated distribution of margin of victory by runs in the IPL between 2007 and 2019")+ 
  theme_tufte()+
  scale_fill_manual(values = sc,guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())

ggsave("dist/win_by_runs_dist.png")
ggplot(dis_ipl %>% filter(variable=="Wickets",x<10.1),aes(x=x,y=value))+
  geom_line()+
  xlab("Margin of Victory")+
  ylab("Probability")+
  labs(title="Distribution of Results", 
       subtitle="Estimated distribution of margin of victory by wickets in the IPL between 2007 and 2019")+ 
  theme_tufte()+
  scale_fill_manual(values = sc,guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())
ggsave("dist/win_by_wickets_dist.png")

ipl_del %>% dplyr ::select(batsman,batting_team,bowling_team,batsman_runs) %>% group_by(batsman,batting_team,bowling_team) %>% dplyr::summarise_each(funs(sum),batsman_runs) %>% ungroup %>% top_n(50) %>% as.data.frame() %>% ggplot(aes(x=batsman,y=bowling_team))+geom_point(aes(size=batsman_runs,color=batting_team))+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

toss_wins<-ipl %>% count(toss_winner) %>% rename(Team=toss_winner,toss_wins=n)
wins<-ipl %>% count(winner) %>% rename(Team=winner,Wins=n)
pld<-left_join(ipl %>% count(team1) %>% 
                       rename(Team=team1),
               ipl %>% count(team2) %>%
                       rename(Team=team2,
                              n2=n)) %>%
     mutate(Played=n+n2) %>% 
     left_join(toss_wins) %>% 
     left_join(wins) %>% 
     mutate(toss_success=toss_wins/Played,
            win_rate=Wins/Played)


ggplot(pld %>% filter(Played>30),
       aes(x=reorder(Team,-toss_success),
           y=toss_success,
           fill=toss_success,label = scales::percent(toss_success)))+
  geom_col()+
  labs(title="IPL Toss Luck",
       subtitle = "How often a team won the toss in the IPL")+
  #theme_clean()+
  xlab("Toss Winner")+ylab("Toss Wins")+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ 
  geom_hline(yintercept = 0.5,linetype="dashed")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(vjust=-.5,
            position = position_dodge(width = .9))

ggsave("toss luck.png")

ggplot(pld ,#%>% filter(Played>30),
       aes(x=reorder(Team,-win_rate),
           y=win_rate,
           fill=win_rate,label = scales::percent(win_rate)))+
  geom_col()+
  labs(title="IPL Success",
       subtitle = "Percentage of games won by each IPL Team")+
  #theme_clean()+
  xlab("Winner")+ylab("Win Rate")+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+ 
  geom_hline(yintercept = 0.5,linetype="dashed")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(vjust=-.5,
            position = position_dodge(width = .9))

ggsave("success.png")

ipl %>% count(city) %>% ggplot(
  aes(x=reorder(city,-n),
      y=n,fill=n,label=n))+
  geom_col()+
  geom_text(hjust=1.2,angle = 90,aes(color="gray88"))+
  labs(title="IPL Venues")+
  xlab("Venue")+ylab("No. of games at venues")+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("venues.png")

#delievery stats

balls<-left_join(ipl_del %>% count(batsman) %>% rename(balls_faced=n),
                 ipl_del %>% count(non_striker) %>% rename(non_striker_balls=n,batsman=non_striker))
boundaries<-left_join(ipl_del %>% filter(batsman_runs==4) %>% count(batsman) %>% rename(fours=n),
                      ipl_del %>% filter(batsman_runs==6) %>% count(batsman) %>% rename(sixes=n))
balls<-left_join(balls,boundaries)
balls[is.na(balls)]<-0

balls<-balls %>% mutate(boundaries=fours+sixes,
                        ballsperboundary=ifelse(boundaries==0,0,balls_faced/boundaries),
                        ballsperfour=ifelse(fours==0,0,balls_faced/fours),
                        ballspersix=ifelse(sixes==0,0,balls_faced/sixes))
                        
ggplot(balls %>% filter(boundaries !=0, boundaries>20) %>% top_n(25,-ballsperboundary),
       aes(x=reorder(batsman,ballsperboundary),y=ballsperboundary,fill=batsman))+geom_col()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Biggest IPL Boundary Hitter",
       subtitle="Minimum 20 boundaries hit")+
  xlab("Batsman")+ylab("Balls per Boundary")
ggsave("boundaries.png")

ggplot(balls %>% filter(sixes !=0, sixes>20) %>% top_n(25,-ballspersix),
       aes(x=reorder(batsman,ballspersix),y=ballspersix,fill=batsman))+geom_col()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Biggest IPL Six Hitter",
       subtitle="Minimum 20 sixes hit")+
  xlab("Batsman")+ylab("Balls per six")
ggsave("sixes.png")

ggplot(balls %>% filter(fours !=0, fours>20) %>% top_n(25,-ballsperfour),
       aes(x=reorder(batsman,ballsperfour),y=ballsperfour,fill=batsman))+geom_col()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="Biggest IPL Four Hitter",
       subtitle="Minimum 20 fours hit")+
  xlab("Batsman")+ylab("Balls per four")
ggsave("fours.png")


#create noball and wide bll collumn dummies
ipl_del$noball<-ifelse(ipl_del$noball_runs==0,0,1)
ipl_del$wide<-ifelse(ipl_del$wide_runs==0,0,1)
ipl_del$del<-ipl_del$ball+(ipl_del$over-1)*6


# New Plots
ggplot(ipl_del %>% filter(wide==1) %>% count(del),
       aes(x=del,y=n))+geom_col(aes(fill="blue"))+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="IPL Wides",
       subtitle="Wide Balls are more frequently bowled at the backend of the innings")+
  xlab("Delivery")+ylab("No. of Wides")
ggsave("wide_del.png")

ggplot(ipl_del %>% filter(wide==1) %>% count(over,bowling_team),
       aes(x=over,y=n))+geom_col(aes(fill=bowling_team))+
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())+
  labs(title="IPL Wides",
       subtitle="Wide Balls are more frequently bowled at the backend of the innings")+
  xlab("Over")+ylab("No. of Wides")
ggsave("wide_over.png")

ggplot(ipl_del %>% filter(noball==1) %>% count(del),
       aes(x=del,y=n))+geom_col(aes(fill="blue"))+
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title="IPL No Balls",
       subtitle="No Balls are more frequently bowled at the backend of the innings")+
  xlab("Delivery")+ylab("No. of No Balls")
ggsave("nb_del.png")

ggplot(ipl_del %>% filter(noball==1) %>% count(over,bowling_team),
       aes(x=over,y=n))+geom_col(aes(fill=bowling_team))+
  theme(legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank())+
  labs(title="IPL No Balls",
       subtitle="No Balls are more frequently bowled at the backend of the innings")+
  xlab("Over")+ylab("No. of No Balls")
ggsave("nb_over.png")
