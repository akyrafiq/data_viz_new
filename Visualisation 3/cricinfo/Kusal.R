#sidetrack- Kusal Mendis
kusal<-read.csv("kusal_mendis.csv")

kusal$Opposition <-gsub("v\xca","",kusal$Opposition)
kusal2<-left_join(kusal %>% group_by(Opposition) %>% summarise(Runs=sum(Runs)),
                  kusal %>% count(Opposition) %>% rename(Innings=n)) %>% 
  left_join(kusal %>% filter(Dismissal=="not out") %>% count(Opposition) %>% rename(Not_Outs=n)) %>% 
  mutate(Average=ifelse(is.na(Not_Outs),Runs/Innings,Runs/(Innings-Not_Outs)))
kusal2[is.na(kusal2)]<-0

ggplot(kusal2,aes(x=reorder(Opposition,-Average),y=Average,fill=Opposition))+
  geom_col()+
  geom_hline(aes(yintercept=sum(kusal$Runs)/(kusal %>% filter(Dismissal!="not out") %>% nrow())),linetype="dotted")+
  labs(title="Kusal Mendis: Feast and Famine", 
       subtitle="Mendis' average against various Test Opposition")+ 
  #theme_wsj()+
  theme_classic()+
  xlab("Opposition")+
  ylab("Average")+
  #scale_fill_manual(values = sc2,guide = guide_legend())+
  theme(legend.position="none",
        legend.title = element_blank())

ggsave("kusal2.png")

ggplot(kusal2,aes(x=reorder(Opposition,-Runs),y=Runs,fill=Average))+
  geom_col()+
  labs(title="Kusal Mendis: Feast and Famine", 
       subtitle="Mendis' runs against various Test Opposition")+ 
  #theme_wsj()+
  theme_classic()+
  xlab("Opposition")+
  ylab("Runs Scored")+
  #scale_fill_manual(values = sc2,guide = guide_legend())+
  theme(legend.position="none",
        legend.title = element_blank())
ggsave("kusal3.png")

ggplot(kusal, aes(x=Runs)) + 
  geom_histogram(aes(fill=Opposition), 
                 binwidth = 10 
                 #,col="black"
  ) +  # change binwidth
  labs(title="Kusal Mendis: Feast and Famine", 
       subtitle="The distribution of scores of Kusal Mendis' Test score")+ 
  #theme_wsj()+
  theme_classic()+
  xlab("Number of Innings")+
  ylab("Runs")+
  #scale_fill_manual(values = sc2,guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())
ggsave("kusal1.png")
