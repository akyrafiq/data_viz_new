library(tidyverse)
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library(plotly)

setwd("~/Documents/GitHub/data_viz/Visualisation 6")

GTE<-read.csv("#GretaThunbergExposed.csv",stringsAsFactors = FALSE)
IAP<-read.csv("#IndiaAgainstPropaganda.csv",stringsAsFactors = FALSE)
abuse<-read.csv("abuse.csv",stringsAsFactors = FALSE)
all_tweets<-rbind(GTE%>% mutate(trend="GTE"),
                  IAP %>% mutate(trend="IAP"),
                  abuse %>% mutate(trend="abuse"))

library(stringr)

numb<-rbind(abuse %>% mutate(number=str_count(author, "[0-9]")) %>% count(number) %>% mutate(trend="abuse"),
      GTE %>% mutate(number=str_count(author, "[0-9]")) %>% count(number)%>% mutate(trend="GTE"),
      IAP %>% mutate(number=str_count(author, "[0-9]")) %>% count(number)%>% mutate(trend="IAP"))

require(ggthemes)
#chart by trend
ggplot(numb,aes(x=number,y=n,fill=trend))+
geom_col(position = "dodge")+
theme_excel_new()

ggplot(numb,aes(x=number,y=n,fill=as.factor(number)))+
geom_col(position = "dodge")+
theme_few()+
xlab("Number of digits in username")+
ylab("Number of users")+
theme(legend.position="none")+
labs(title="Notable bot activity", 
     subtitle="There is an irregular number of users with 8 digits in their username")
ggsave("bots1.png")

GTE$description %>% unique() %>% length()
IAP$description %>% unique() %>% length()
abuse$description %>% unique() %>% length()
GTE %>% count(description) %>% top_n(5)

all_tweets$user_created_at<-all_tweets$user_created_at %>% as.POSIXct()

ggplot(all_tweets,aes(x=user_created_at))+geom_histogram(binwidth = 5)
user_created<-all_tweets %>% mutate(year=year(user_created_at),number=str_count(author, "[0-9]"))%>% count(year,number)
user_created2<-all_tweets %>% mutate(year=year(user_created_at),number=str_count(author, "[0-9]"))%>% count(year,trend)

#aggregated plotly
plot_ly(numb,x=~number,y=~n/2500,color=~trend) %>% add_bars()
plot_ly(user_created ,x=~year,y=~n/2500,color =~as.factor(number))%>% add_bars()
plot_ly(user_created2 ,x=~year,y=~n/2500,color =~trend)%>% add_bars()

#Printed plots
sc<-grDevices::"heat.colors"(12)
ggplot(user_created,aes(x=year,y=n,fill=reorder(as.factor(number),-number)))+
geom_col()+
theme_few()+
xlab("Year of account creation")+
ylab("Number of users")+
scale_fill_manual(values = sc,guide = guide_legend(),name = "Number of Digits in Tweet")+
theme(legend.position="bottom")+
labs(title="Notable bot activity", 
       subtitle="Most of these accounts have risen up over the past two years")
ggsave("bots2.png")

#Create word cloud
library(wordcloud)
wordcloud(all_tweets$description, min.freq=100,random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(GTE$description, min.freq=50,random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(IAP$description, min.freq=50,random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

wordcloud(abuse$description, min.freq=50,random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#text analysis
library("tm")

docs <- Corpus(VectorSource(all_tweets$description))

#clean text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
toErase <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
NoHypLinks <- content_transformer(function (x) gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x))

docs <- tm_map(docs, NoHypLinks)
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "\\'")
docs <- tm_map(docs, toErase, "\\")
docs <- tm_map(docs, toErase, "\\p{So}|\\p{Cn}")

#Check
inspect(docs)

#Count hashtags
hashtags<-str_extract_all(docs, "#\\S+")[[1]] %>% table() %>% as.data.frame()
names(hashtags)[1]<-"Hashtag"

clean_hashtags<-hashtags %>%
                mutate(Hashtag=gsub("([#])|[[:punct:]]", "",
                                    gsub("\\\\", "",
                                         gsub("\\u0001f1ee","",
                                              gsub("\\u0001f1f3","",
                                                   gsub("\"","",
                                                        gsub(",","",
                                                             tolower(hashtags$Hashtag))))))) %>%
                         as.character()) %>%
                group_by(Hashtag) %>%
                summarise_all(.funs = "sum") %>%
                as.data.frame() %>% 
                mutate(Hashtag=paste0("#",Hashtag))

#Find biggest hashtags
top_hashtags<-clean_hashtags %>% filter(Freq>10)

#Another wordcloud
wordcloud(words = top_hashtags$Hashtag,freq = top_hashtags$Freq, rot.per=0.35,
          colors=brewer.pal(15, "Dark2"))


library("SnowballC")
#reccomended packages
library(quanteda)
library(text2vec)
library(tidytext)
library(spacyr)

