library(tidyverse)
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")


setwd("~/Documents/GitHub/data_viz/Visualisation 5")

  
# x<-read.csv("https://raw.githubusercontent.com/cwhaley112/capitol-riot-tweets/main/tweets_2021-01-06.csv",stringsAsFactors = FALSE)
# write.csv(x,"riot_tweets.csv")

x<-read.csv("riot_tweets.csv",stringsAsFactors = FALSE)

x %>% dplyr::count(user_name) %>% top_n(15,n)

library(leaflet)

#Initial Map
basic_location<-leaflet(data = x) %>% addTiles() %>%
  addCircleMarkers(~longitude, ~latitude)

#attempting to create contours
kde <- bkde2D(x[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
heatmap<-leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])%>%
  addCircles(lng = x$longitude, lat = x$latitude,
             radius = .5, opacity = .2, col = "blue")

library(htmlwidgets)
saveWidget(heatmap, file="heatmap.html")

x$size<-ifelse(x$follower_count>100,
               ifelse(x$follower_count>500,
                      ifelse(x$follower_count>1000,
                             ifelse(x$follower_count>10000,
                                    ifelse(x$follower_count>100000,"Huge",
                                           "Very Large"),
                                    "Large"),
                             "Medium"),
                      "Small"),
               "Very Small")

x %>%
count(query,size)%>%
ggplot(aes(x=query,fill=size,y=n))+
geom_col()+  # change binwidth
  labs(title="Queries by follower count", 
       subtitle="Most tweeters have no more than 1000 followers")+ 
  #theme_fivethirtyeight()+
  theme_base()+
  xlab("Query")+
  ylab("Number of Tweets")+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("folowers.png")

x %>%
  count(size,query)%>%
  ggplot(aes(x=size,y=n,fill=query))+
  geom_col()+  # change binwidth
  labs(title="Queries by follower count", 
       subtitle="Most tweeters have no more than 1000 followers")+ 
  #theme_fivethirtyeight()+
  theme_base()+
  xlab("Number of followers")+
  ylab("Count")+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("folowers2.png")


top_tweets<-x %>% count(text) %>% top_n(10)
