library(cricketr)


library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(fitdistrplus)


setwd("~/Documents/GitHub/data_viz/Visualisation 3/cricinfo")
#creta reofmrattign fucntion for cricingfo data
crictidy<-function(df){
  df[1:8,]<-as.numeric(df[1:8,])
}
tendulkar <- getPlayerData(35320,dir="..",file="tendulkar.csv",type="batting",homeOrAway=c(1,2),
                           result=c(1,2,4))
tenudlkar<-read_csv("tendulkar.csv")
tendulkar$Runs<-as.numeric(tendulkar$Runs)
tendulkar2<-left_join(tendulkar  %>% group_by(Opposition) %>% summarise(Runs=sum(Runs)),
                      tendulkar  %>% count(Opposition) %>% rename(Innings=n)) %>% 
  left_join(tendulkar %>% filter(Dismissal=="not out") %>% count(Opposition) %>% rename(Not_Outs=n)) %>% 
  mutate(Average=ifelse(is.na(Not_Outs),Runs/Innings,Runs/(Innings-Not_Outs)))

tendulkar<-tendulkar[complete.cases(tendulkar),]

ggplot(tendulkar2,aes(x=reorder(Opposition,-Average),y=Average,fill=Opposition))+
  geom_col()+
  geom_hline(aes(yintercept=sum(tendulkar$Runs)/(tendulkar %>% filter(Dismissal!="not out") %>% nrow())),linetype="dotted")+
  labs(title="Sachin Tendulkar", 
       subtitle="Tendulkar's average against various Test Opposition")+ 
  #theme_wsj()+
  theme_classic()+
  xlab("Opposition")+
  ylab("Average")+
  #scale_fill_manual(values = sc2,guide = guide_legend())+
  theme(legend.position="none",
        legend.title = element_blank())
ggsave('tendulkar_avg.png')

ssmith <- getPlayerData(267192,dir="..",file="ssmith.csv",type="batting",homeOrAway=c(1,2),
                           result=c(1,2,4))
ssmith$Runs<-as.numeric(ssmith$Runs)
ssmith<-ssmith[complete.cases(ssmith),]

ssmith2<-left_join(ssmith  %>% group_by(Opposition) %>% summarise(Runs=sum(Runs)),
                      ssmith  %>% count(Opposition) %>% rename(Innings=n)) %>% 
  left_join(ssmith %>% filter(Dismissal=="not out") %>% count(Opposition) %>% rename(Not_Outs=n)) %>% 
  mutate(Average=ifelse(is.na(Not_Outs),Runs/Innings,Runs/(Innings-Not_Outs)))


batsmanRunsFreqPerf("./tendulkar.csv","Sachin Tendulkar")
ggsave('tendulkar_rfp.png')
batsmanMeanStrikeRate("./tendulkar.csv","Sachin Tendulkar")
ggsave('tendulkar_msr.png')
batsmanRunsRanges("./tendulkar.csv","Sachin Tendulkar")
ggsave('tendulkar_rr.png')
batsmanAvgRunsOpposition("./tendulkar.csv","Tendulkar")
ggsave('tendulkar_avg2.png')
batsmanAvgRunsOpposition("./ssmith.csv","Steve Smith")
ggsave('smith_avg.png')
batsmanPerfBoxHist("./tendulkar.csv","Sachin Tendulkar")
ggsave('tendulkar_pbh.png')
batsmanPerfBoxHist("./ssmith.csv","Steve Smith")
ggsave('smith_pbh.png')
batsmanPerfForecast("./ssmith.csv","Steve Smith")
ggsave('smith_pf.png')

#New player pull
wasim <- getPlayerData(43547,dir="..",file="wasim.csv",type="bowling",homeOrAway=c(1,2),
                       result=c(1,2,4))
waqar <- getPlayerData(43543,dir="..",file="waqar.csv",type="bowling",homeOrAway=c(1,2),
                       result=c(1,2,4))

yasir <- getPlayerData(43685,dir="..",file="yasir.csv",type="bowling",homeOrAway=c(1,2),
                       result=c(1,2,4))

rand<- getPlayerData(43691,dir="..",file="rand.csv",type="batting",homeOrAway=c(1,2),
                     result=c(1,2,4))

