#setd wd
setwd("~/Documents/GitHub/data_viz/Visualisation 4")

#read in data and give names
ptgl<-read_tsv("ptgl_fire.txt",col_names = FALSE)

names(ptgl)<-c("District code","District","County", "Parish", "Public shrubland burnt","Private shrubland burnt","Total shrubland burnt area", "Public forest burnt",
  "Private forest burnt" ,	"Total forest burnt area" ,	"Total burnt area" ,"Agricultural burnt area",
  "Type of fire","Fire cause code","Fire ignition date excel code","Fire ignition time excel code",
   "Fire ignition hour","Fire ignition minute","Fire extinction date excel code","Fire extinction time excel code",
   "Fire extinction hour",	"Fire extinction minute","Fire duration","Flag1 ","Flag2","Flag3" ,"Flag4" ,"Time","Day" )

#basic plots
ptgl %>%
filter(`Total shrubland burnt area`>0) %>%
top_n(25,`Total shrubland burnt area`) %>% 
ggplot(aes(x=paste0(County," ",Time),y=`Total shrubland burnt area`,
                    fill=`Fire duration`))+
  geom_col()+
  xlab("County of fire")+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dff<-data.frame(Area=0:120,
           Probability=dweibull(0:120,
                       shape=fitdistr(ptgl$`Total burnt area`, "weibull")$estimate["shape"],
                       scale=fitdistr(ptgl$`Total burnt area`, "weibull")$estimate["scale"])) 
  ggplot(dff,aes(x=Area,y=Probability,color="red")) +
    geom_line()+
    scale_x_continuous(trans='log10')+
    xlab("Area of Wildfire in Acres")+theme_clean()+labs(title="Distribution of Wildfire size", 
                                                         subtitle="Estimated distribution of the size of a wildfire in Portugal")+
    theme(legend.position="none")
  ggsave("dist.png") 
  
  ptgl %>% 
    filter(`Total burnt area`>1000) %>% 
    count(District) %>% 
    ggplot(aes(x=District,y=n,fill=District)) +
    geom_col()+
  theme_fivethirtyeight()+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(title="Districts with largest wildfires", 
       subtitle="The number of fires over 1000 acres in these districts")
  ggsave("top_fires.png") 
    
  