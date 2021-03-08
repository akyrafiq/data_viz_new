library(tidyverse)
library(ggthemes)
library(plotly)

#Set Parameters
dr<-0.001 #Death rate of virus
r0<-1.1 #Rough guess of current
a<-1.7 #BMJ state new variant is 70% more tranmissable https://www.bmj.com/content/371/bmj.m4944
b<-0.2 #Assuming fatality rate of new variant if a fifth of original virus

#Dataframe
df<-data.frame(Day=0:28) %>% 
     mutate(Infected=r0^Day,
            New_Infected=(r0*a)^Day, #Assuming no restrictions
            Deaths=Infected*dr,
            New_Deaths=New_Infected*(dr*b))
df<-df %>% reshape2::melt(id="Day")

#Plot of infections- no log axis
ggplot(df %>% filter(variable %in% c("New_Infected","Infected")),aes(x=Day,y=value,color=variable))+
geom_line()+
theme_solarized(light=FALSE)+
xlab("Days since first infection")+
ylab("Infections")+
scale_colour_solarized('blue')+
theme( legend.key = element_rect(colour = NA, fill = NA))+
ggtitle("Comparison between Infections of New & Old Variants of Coronavirus")+
scale_color_manual(name="Variant", labels = c("Infected at original virus", "Infected at new variant"), values = c("blue", "red"))

#Plot of infections
ggplot(df %>% filter(variable %in% c("New_Infected","Infected")),aes(x=Day,y=value,color=variable))+
  geom_line()+
  theme_solarized(light=FALSE)+
  xlab("Days since first infection")+
  ylab("Infections")+
  scale_colour_solarized('blue')+
  theme( legend.key = element_rect(colour = NA, fill = NA))+
  ggtitle("Comparison between Infections of New & Old Variants of Coronavirus")+
  scale_color_manual(name="Variant", labels = c("Infected at original virus", "Infected at new variant"), values = c("blue", "red"))+
  scale_y_continuous(trans='log10')


#Plot of Deaths
ggplot(df %>% filter(variable %in% c("New_Deaths","Deaths")),aes(x=Day,y=value,colour=variable))+
  geom_line()+
  theme_solarized(light=FALSE)+
  xlab("Days since first infection")+
  ylab("Deaths")+
  scale_colour_solarized('blue')+
  theme( legend.key = element_rect(colour = NA, fill = NA))+
  ggtitle("Comparison between Deaths of New & Old Variants of Coronavirus")+
  scale_color_manual(name="Variant", labels = c("Deaths at original virus", "Deaths at new variant"), values = c("blue", "red"))+
  scale_y_continuous(trans='log10')


