library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz/Visualisation 2")

#read in data

#latestEPL Set
mls_new<-read.csv("data/form/all_leaguetables.csv") %>%
  filter(League=="Major League Soccer")

mls_new<-mls_new[,3:12]
names(mls_new)<-names(combo)

#Combine two sets together
combo_mls<-mls_new
#start plots
########## Histogramsss
#points
pal <- wes_palette("Zissou1", 100, type = "continuous")
sc2<-colorRamps::"green2red"(18)
sc3<-colorRamps::"green2red"(24)

ggplot(combo_mls, aes(x=P)) + 
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 4 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Points", 
       subtitle="The number of points obtained by teams in MLS between 2015 and 2020")+ 
  #theme_wsj()+
  theme_economist()+
  xlab("Goals Scored")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/point_hist.png")

#scored
ggplot(combo_mls, aes(x=F)) + 
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 4 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Goals Scored", 
       subtitle="The amount of goals scored by teams in MLS between 2015 and 2020")+ 
  #theme_fivethirtyeight()+
  theme_economist()+
  xlab("Goals Scored")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/score_hist.png")

#conceded
ggplot(combo_mls, aes(x=A)) +
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 4 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Goals Conceded", 
       subtitle="The amount of goals conceded by teams in MLS between 2015 and 2020")+ 
  #theme_fivethirtyeight()+
  theme_economist()+
  xlab("Goals Conceded")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/con_hist.png")

#Difference
ggplot(combo_mls, aes(x=D.1)) + 
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 4 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Goals Difference", 
       subtitle="The Goal Differences conceded by teams in MLS between 2015 and 2020")+ 
  #theme_fivethirtyeight()+
  theme_economist()+
  xlab("Goals Difference")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/gd_hist.png")

#Draws
ggplot(combo_mls, aes(x=D)) + 
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 1) +  # change binwidth
  labs(title="Draws", 
       subtitle="The Number of Draws by teams in MLS between 2015 and 2020")+ 
  #theme_fivethirtyeight()+
  theme_economist()+
  xlab("Draws")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/draw_hist.png")
#Wins
ggplot(combo_mls, aes(x=W)) + 
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 1 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Wins", 
       subtitle="The Number of Wins by teams in MLS between 2015 and 2020")+ 
  #theme_fivethirtyeight()+
  theme_economist()+
  xlab("Draws")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/win_hist.png")

#Losses
ggplot(combo_mls, aes(x=L)) + 
  geom_histogram(aes(fill=as.factor(X.)), 
                 binwidth = 1 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Losses", 
       subtitle="The Number of Losses by teams in MLS between 2015 and 2020")+ 
  #theme_fivethirtyeight()+
  theme_economist()+
  xlab("Draws")+
  ylab("Count")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="none")
ggsave("hist/mls/loss_hist.png")

###Ok so that is histograms- lets try and fit these onto some probability distributions
#Fitted distributions
dis_mls<-data.frame(x=0:1200/10,
                       Points=dweibull(0:1200/10,shape=fitdistr(combo_mls$P, "weibull")$estimate["shape"],
                                       scale=fitdistr(combo_mls$P, "weibull")$estimate["scale"]),
                       Wins=dweibull(0:1200/10,shape=fitdistr(combo_mls$W, "weibull")$estimate["shape"],
                                     scale=fitdistr(combo_mls$W, "weibull")$estimate["scale"]),
                       Losses=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$L>0,]$L, "weibull")$estimate["shape"],
                                       scale=fitdistr(combo_mls[combo_mls$L>0,]$L, "weibull")$estimate["scale"]),
                       Draws=dweibull(0:1200/10,shape=fitdistr(combo_mls$D, "weibull")$estimate["shape"],
                                      scale=fitdistr(combo_mls$D, "weibull")$estimate["scale"]),
                       Scored=dweibull(0:1200/10,shape=fitdistr(combo_mls$F, "weibull")$estimate["shape"],
                                       scale=fitdistr(combo_mls$F, "weibull")$estimate["scale"]),
                       Conceded=dweibull(0:1200/10,shape=fitdistr(combo_mls$A, "weibull")$estimate["shape"],
                                         scale=fitdistr(combo_mls$A, "weibull")$estimate["scale"]),
                       Scored_High=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<10,]$F, "weibull")$estimate["shape"],
                                            scale=fitdistr(combo_mls[combo_mls$X.<10,]$F, "weibull")$estimate["scale"]),
                       Scored_Low=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>11,]$F, "weibull")$estimate["shape"],
                                           scale=fitdistr(combo_mls[combo_mls$X.>11,]$F, "weibull")$estimate["scale"]),
                       Conceded_High=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<10,]$A, "weibull")$estimate["shape"],
                                              scale=fitdistr(combo_mls[combo_mls$X.<10,]$A, "weibull")$estimate["scale"]),
                       Conceded_Low=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>11,]$A, "weibull")$estimate["shape"],
                                             scale=fitdistr(combo_mls[combo_mls$X.>11,]$A, "weibull")$estimate["scale"]),
                       Conceded_3=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<4,]$A, "weibull")$estimate["shape"],
                                           scale=fitdistr(combo_mls[combo_mls$X.<4,]$A, "weibull")$estimate["scale"]),
                       Scored_3=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<4,]$F, "weibull")$estimate["shape"],
                                         scale=fitdistr(combo_mls[combo_mls$X.<4,]$F, "weibull")$estimate["scale"]),
                       Conceded_Rel=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>17,]$A, "weibull")$estimate["shape"],
                                             scale=fitdistr(combo_mls[combo_mls$X.>17,]$A, "weibull")$estimate["scale"]),
                       Scored_Rel=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>17,]$F, "weibull")$estimate["shape"],
                                           scale=fitdistr(combo_mls[combo_mls$X.>17,]$F, "weibull")$estimate["scale"]),
                       Points_High=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<10,]$P, "weibull")$estimate["shape"],
                                            scale=fitdistr(combo_mls[combo_mls$X.<10,]$P, "weibull")$estimate["scale"]),
                       Points_Low=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>11,]$P, "weibull")$estimate["shape"],
                                           scale=fitdistr(combo_mls[combo_mls$X.>11,]$P, "weibull")$estimate["scale"]),
                       Points_3=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<4,]$P, "weibull")$estimate["shape"],
                                         scale=fitdistr(combo_mls[combo_mls$X.<4,]$P, "weibull")$estimate["scale"]),
                       Points_Rel=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>17,]$P, "weibull")$estimate["shape"],
                                           scale=fitdistr(combo_mls[combo_mls$X.>17,]$P, "weibull")$estimate["scale"]),
                       GD=dweibull(0:1200/10,shape=fitdistr(combo_mls$D.1-min(combo_mls$D.1)+1, "weibull")$estimate["shape"],
                                   scale=fitdistr(combo_mls$D.1-min(combo_mls$D.1)+1, "weibull")$estimate["scale"]),
                       GD_High=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<10,]$D.1-min(combo_mls[combo_mls$X.<10,]$D.1)+1, "weibull")$estimate["shape"],
                                        scale=fitdistr(combo_mls[combo_mls$X.<10,]$D.1-min(combo_mls[combo_mls$X.<10,]$D.1)+1, "weibull")$estimate["scale"]),
                       GD_Low=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>11,]$D.1-min(combo_mls[combo_mls$X.>11,]$D.1)+1, "weibull")$estimate["shape"],
                                       scale=fitdistr(combo_mls[combo_mls$X.>11,]$D.1-min(combo_mls[combo_mls$X.>11,]$D.1)+1, "weibull")$estimate["scale"]),
                       GD_3=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.<4,]$D.1-min(combo_mls[combo_mls$X.<4,]$D.1)+1, "weibull")$estimate["shape"],
                                     scale=fitdistr(combo_mls[combo_mls$X.<4,]$D.1-min(combo_mls[combo_mls$X.<4,]$D.1)+1, "weibull")$estimate["scale"]),
                       GD_Rel=dweibull(0:1200/10,shape=fitdistr(combo_mls[combo_mls$X.>17,]$D.1-min(combo_mls[combo_mls$X.>17,]$D.1)+1, "weibull")$estimate["shape"],
                                       scale=fitdistr(combo_mls[combo_mls$X.>17,]$D.1-min(combo_mls[combo_mls$X.>17,]$D.1)+1, "weibull")$estimate["scale"])) %>% 
  reshape2::melt(id="x")

#untransform goal difference
dis_mls[dis_mls$variable=="GD",]$x<-dis_mls[dis_mls$variable=="GD",]$x+min(combo_mls$D.1)-1
dis_mls[dis_mls$variable=="GD_High",]$x<-dis_mls[dis_mls$variable=="GD_High",]$x+min(combo_mls[combo_mls$X.<10,]$D.1)-1
dis_mls[dis_mls$variable=="GD_Low",]$x<-dis_mls[dis_mls$variable=="GD_Low",]$x+min(combo_mls[combo_mls$X.>11,]$D.1)-1
dis_mls[dis_mls$variable=="GD_3",]$x<-dis_mls[dis_mls$variable=="GD_3",]$x+min(combo_mls[combo_mls$X.<4,]$D.1)-1
dis_mls[dis_mls$variable=="GD_Rel",]$x<-dis_mls[dis_mls$variable=="GD_Rel",]$x+min(combo_mls[combo_mls$X.>17,]$D.1)-1
#PLot distributions
#Results
ggplot(dis_mls %>% filter(x<35,variable %in% c("Wins","Losses","Draws")),aes(x=x,y=value))+
  geom_line(aes(color=variable))+
  xlab("Frequency of Results")+
  ylab("Probability")+
  labs(title="Distribution of Results", 
       subtitle="Estimated distribution of results outcomes in MLS between 2015 and 2020")+ 
  theme_tufte()+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())
ggsave("dist/mls/result_dist.png")

#Points
#Results
ggplot(dis_mls %>% filter(x<90,variable %in% c("Points_Low","Points_High","Points","Points_3","Points_Rel")),aes(x=x,y=value))+
  geom_line(aes(color=variable,
                linetype=variable))+
  xlab("Points")+
  ylab("Probability")+
  labs(title="Distribution of Points", 
       subtitle="Estimated distribution of points in MLS between 2015 and 2020")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),guide = guide_legend(),
                     breaks = c("Points_Low","Points_High","Points_3","Points_Rel"),
                     labels = c("Points gained by lower table teams",
                                "Points gained by upper table teams",
                                "Points gained by Teams in the Top 3",
                                "Points gained by relegated teams"))+
  theme(legend.position="bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values=c("dashed", "solid", "solid","solid","solid"))+
  guides(linetype = FALSE)
ggsave("dist/mls/points_dist.png")
#Goals
ggplot(dis_mls %>% filter(x<80,variable %in% c("Scored","Conceded")),aes(x=x,y=value))+
  geom_line(aes(color=variable))+
  xlab("Goals")+
  ylab("Probability")+
  labs(title="Distribution of Goals", 
       subtitle="Estimated distribution of goals in MLS between 2015 and 2020")+ 
  theme_tufte()+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  theme(legend.position="right",
        legend.title = element_blank())
ggsave("dist/mls/goal_dist.png")

#scoreed
ggplot(dis_mls %>% filter(x<100,x>15,variable %in% c("Scored_Low","Scored_High","Scored","Scored_3","Scored_Rel")),aes(x=x,y=value))+
  geom_line(aes(color=variable,
                linetype=variable))+
  xlab("Goals Scored")+
  ylab("Probability")+
  labs(title="Distribution of Goals", 
       subtitle="Estimated distribution of goals scored in MLS between 2015 and 2020")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),guide = guide_legend(),
                     breaks = c("Scored_Low","Scored_High","Scored_3","Scored_Rel"),
                     labels = c("Goal scored by lower table teams",
                                "Goal scored by upper table teams",
                                "Goal scored by Teams in the Top 3",
                                "Goal scored by relegated teams"))+
  theme(legend.position="bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values=c("dashed", "solid", "solid","solid","solid"))+
  guides(linetype = FALSE)
ggsave("dist/mls/score_dist.png")

#conceded
ggplot(dis_mls %>% filter(x<100,x>15,variable %in% c("Conceded","Conceded_Low","Conceded_High","Conceded_3","Conceded_Rel")),aes(x=x,y=value))+
  geom_line(aes(color=variable,
                linetype=variable))+
  xlab("Goals Conceded")+
  ylab("Probability")+
  labs(title="Distribution of Goals", 
       subtitle="Estimated distribution of goals conceded in MLS between 2015 and 2020")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),guide = guide_legend(),
                     breaks = c("Conceded_Low","Conceded_High","Conceded_3","Conceded_Rel"),
                     labels = c("Goal Conceded by lower table teams",
                                "Goal Conceded by upper table teams",
                                "Goal Conceded by teams in the Top 3",
                                "Goal Conceded by relegated teams"))+
  theme(legend.position="bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values=c("dashed", "solid", "solid", "solid", "solid"))+
  guides(linetype = FALSE)
ggsave("dist/mls/conc_dist.png")
#goal diffrence
ggplot(dis_mls %>% filter(x<90,variable %in% c("GD_Low","GD_High","GD","GD_3","GD_Rel")),aes(x=x,y=value))+
  geom_line(aes(color=variable,
                linetype=variable))+
  xlab("Goal difference")+
  ylab("Probability")+
  labs(title="Distribution of Goal Difference", 
       subtitle="Estimated distribution of goal difference in MLS between 2015 and 2020")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),guide = guide_legend(),
                     breaks = c("Points_Low","Points_High","Points_3","Points_Rel"),
                     labels = c("Goal difference of lower table teams",
                                "Goal difference of upper table teams",
                                "Goal difference of Teams in the Top 3",
                                "Goal difference of relegated teams"))+
  theme(legend.position="bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values=c("dashed", "solid", "solid","solid","solid"))+
  guides(linetype = FALSE)

ggsave("dist/mls/gd_dist.png")
##Comparisonn
ggplot(combo_mls, aes(x=X.,y=P)) + 
  #geom_hex(aes(fill=as.factor(X.)))+
  stat_ydensity(aes(fill=as.factor(X.)))+
  geom_smooth(aes(colour="blue"),linetype="dashed",colour="black")+
  theme_few()+
  xlab("Position")+
  ylab("Points")+
  theme(legend.position="none")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  labs(title="How points vary by positions", 
       subtitle="How points varied with league position in MLS between 2015 and 2020")+
  scale_x_continuous(trans = "reverse")
ggsave("plot/mls/XvP.png")


#XvGD
ggplot(combo_mls, aes(x=X.,y=D.1)) + 
  #geom_hex(aes(fill=as.factor(X.)))+
  stat_ydensity(aes(fill=as.factor(X.)))+
  geom_smooth(aes(colour="blue"),linetype="dashed",colour="black")+
  theme_few()+
  xlab("Position")+
  ylab("Goal Difference")+
  theme(legend.position="none")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  labs(title="How goal difference varied by positions", 
       subtitle="How goal difference varied with league position in MLS between 2015 and 2020")+
  scale_x_continuous(trans = "reverse")
ggsave("plot/mls/XvGD.png")

#XvF
ggplot(combo_mls, aes(x=X.,y=F)) + 
  #geom_hex(aes(fill=as.factor(X.)))+
  stat_ydensity(aes(fill=as.factor(X.)))+
  geom_smooth(aes(colour="blue"),linetype="dashed",colour="black")+
  theme_few()+
  xlab("Position")+
  ylab("Goals Scored")+
  theme(legend.position="none")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  labs(title="How goals scored varied by positions", 
       subtitle="How goals scored varied with league position in MLS between 2015 and 2020")+
  scale_x_continuous(trans = "reverse")
ggsave("plot/mls/XvGS.png")

#XvF
ggplot(combo_mls, aes(x=X.,y=A)) + 
  #geom_hex(aes(fill=as.factor(X.)))+
  stat_ydensity(aes(fill=as.factor(X.)))+
  geom_smooth(aes(colour="blue"),linetype="dashed",colour="black")+
  theme_few()+
  xlab("Position")+
  ylab("Goals Conceded")+
  theme(legend.position="none")+
  scale_fill_manual(values = sc3,guide = guide_legend())+
  labs(title="How goals conceded varied by positions", 
       subtitle="How goals conceded varied with league position in MLS between 2015 and 2020")+
  scale_x_continuous(trans = "reverse")
ggsave("plot/mls/XvGC.png")

#XvFnA
#XvF
ggplot(combo_mls, aes(x=X.)) + 
  #geom_hex(aes(fill=as.factor(X.)))+
  stat_ydensity(aes(colour=as.factor(X.),y=F,fill="green"),alpha=.5)+
  stat_ydensity(aes(colour=as.factor(X.),y=A,fill="red"),alpha=.5)+
  #geom_smooth(aes(colour="blue"),linetype="dashed",colour="black")+
  theme_few()+
  xlab("Position")+
  ylab("Goals")+
  theme(legend.position="none")+
  #scale_colour_manual(values = sc3,guide = guide_legend())+
  labs(title="How goals conceded and scored varied by positions", 
       subtitle="How goals conceded and scored varied with league position in MLS between 2015 and 2020")+
  scale_x_continuous(trans = "reverse")
ggsave("plot/mls/XvGAnGC.png")

combo_mls_agg<-aggregate(cbind(MP,P,F,A) ~ Team,data=combo_mls,sum)
#Best Attack
ggplot(combo_mls_agg, aes(x=reorder(Team,-F))) + 
  geom_col(aes(y=F,fill=F),alpha=.5)+
  geom_col(aes(y=-A,fill=-A),alpha=.5)+
  geom_point(aes(y=F-A))+
  theme_wsj()+
  xlab("Position")+
  ylab("Goals")+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #scale_colour_manual(values = sc3,guide = guide_legend())+
  labs(title="Goals conceded and scored by each team", 
       subtitle="Number goals conceded and scored by each team in MLS between 2015 and 2020")
ggsave("plot/mls/TeamvGAnGC.png")
##
#Best Attack
ggplot(combo_mls_agg, aes(x=reorder(Team,A/F))) + 
  #geom_hex(aes(fill=as.factor(X.)))+
  geom_col(aes(y=F/A,fill=F),alpha=.5)+
  theme_wsj()+
  xlab("Position")+
  ylab("Goals")+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #scale_colour_manual(values = sc3,guide = guide_legend())+
  labs(title="How goals conceded and scored by each team", 
       subtitle="How goals conceded and scored by each team in MLS between 2015 and 2020")


ggplot(dis_mls %>% filter(x<100,variable %in% c("Points_Low","Points_High","Points","Points_3","Points_Rel")),aes(x=x,y=value))+
  geom_line(aes(color=variable,
                linetype=variable))+
  geom_point(data=dis %>% filter(x<100,variable %in% c("Points_Low","Points_High","Points","Points_3","Points_Rel")),
             aes(color=variable,
                 linetype=variable),alpha=.5)+
  xlab("Points")+
  ylab("Probability")+
  labs(title="Distribution of Points", 
       subtitle="Estimated distribution of points in MLS between 2015 and 2020 and EPL 2007 to 2017")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),guide = guide_legend(),
                     breaks = c("Points_Low","Points_High","Points_3","Points_Rel"),
                     labels = c("Points gained by lower table teams",
                                "Points gained by upper table teams",
                                "Points gained by Teams in the Top 3",
                                "Points gained by relegated teams"))+
  theme(legend.position="bottom",
        legend.title = element_blank())+
  scale_linetype_manual(values=c("dotted", "solid", "solid","solid","solid"))+
  guides(linetype = FALSE)
ggsave("dist/comp_league.png")
##other stuff to do laterr
ggplot(combo_mls, aes(x=F,y=A)) + 
  geom_smooth(method = "lm",linetype="dotted",colour="black",se=F)+
  geom_point(alpha=0.5,aes(fill=as.factor(X.)))+
  scale_colour_manual(values = sc3,guide = guide_legend())+
  #theme_gdocs()+
  xlab("Goals Scored")+
  ylab("Goals Conceded")+
  theme(legend.position="none")
