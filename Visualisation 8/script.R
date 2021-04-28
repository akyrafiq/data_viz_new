library(tidyverse)
library(plotly)

setwd("~/Documents/TESLA/")

#Read in data
#Census Data
files<-list.files("DECENNIALCD1132010.P12_2021-04-14T110537/")
tables<-lapply(paste0("DECENNIALCD1132010.P12_2021-04-14T110537/",files[1:2]), read_csv)

cens1<-tables[[1]]
cens2<-tables[[2]]
cens3<-read.delim(paste0("DECENNIALCD1132010.P12_2021-04-14T110537/",files[3]))

glimpse(cens1)
cens1$`Geographic Area Name` %>% unique

#Zipcode data
zip_nyc<-read.delim("zip_income_returncount_new_york.csv",sep="|")

#More Census Data
files2<-list.files("NY 2010 Census Data Race By 5-digit Zip/")
tables2<-lapply(paste0("NY 2010 Census Data Race By 5-digit Zip/",files2[1:13]), read_csv)

for (i in tables2) {
  i<-i[complete.cases(i),]
}

actual_data<-read_csv("NYSERDA_Electric_Vehicle_Drive_Clean_Rebate_Data__Beginning_2017.csv")

#Actually starting some data vis...
make<-actual_data %>% count(Make) %>% mutate(Perc=n/36833*100) 

make %>% filter(Perc>5) %>% ggplot(aes(x=Make,y=Perc))+geom_col()

make_5<-make %>% filter(Perc>5)

actual_data %>% filter(Make %in% c("Tesla","Chevrolet","Ford","Honda","Hyundai","Toyota")) %>%
  ggplot(aes(x=`Rebate Amount (USD)`, fill=Make)) +geom_histogram(binwidth = 100)

ZIP<-actual_data %>% count(ZIP)

ZIP  %>% ggplot(aes(x=as.factor(ZIP),y=n))+geom_col()

county<-actual_data %>% count(County,Make) 

county%>% filter(Make %in% c("Tesla","Chevrolet","Ford","Honda","Hyundai","Toyota"))  %>% ggplot(aes(x=County,y=n,fill=Make))+geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

income<-read.csv("Income By Location.csv") %>% filter(Year==2018)
income

county%>% filter(Make %in% c("Tesla","Chevrolet","Ford","Honda","Hyundai","Toyota"),
                 County %in% c("Suffolk", "Nassau","Westchester","Monroe","Erie","Queens","Kings","New York"))  %>%
  ggplot(aes(x=reorder(County,-n),y=n,fill=Make))+geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
xlab("County")+
  ylab("Number of rebates in each county")+
labs(title="Number of rebates by county", 
     subtitle="How many rebates were acquired by each county in New York (Min 1000 rebates in the county)")+
  theme_clean()
ggsave("Rebates by County.png")
require(ggthemes)

income2<-income %>%
  filter(Geography %in% paste0(c("Suffolk", "Nassau","Westchester","Monroe","Erie","Queens","Kings","New York")," County, NY")) 


income %>% filter(Race=="Total") %>% top_n(10,Household.Income.by.Race)%>% ggplot(aes(x=reorder(Geography,-Household.Income.by.Race),y=Household.Income.by.Race,fill=Race))+geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("County")+
  ylab("Median Household Income")+
  labs(title="Wealthiest Counties in New York", 
       subtitle="These counties fall within the top earning counties in the State")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("Wealthiest counties.png")

income2 %>% filter(Race!="Total") %>% ggplot(aes(x=Geography,y=Household.Income.by.Race,fill=Race))+geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("County")+
  ylab("Number of rebates in each county")+
  labs(title="Number of rebates by county", 
       subtitle="How many rebates were acquired by each county in New York (Min 1000 rebates in the county)")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("Wealthiest counties by race.png")
