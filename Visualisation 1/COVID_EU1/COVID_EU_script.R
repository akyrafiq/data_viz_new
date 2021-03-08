library('dplyr')

setwd("~/Documents/GitHub/data_viz/Visualisation 1/COVID_EU1/")

X <- read.csv("owid-covid-data.csv")



## Note: For this group the focus is on European countries at the national level
# You can use the code below to get different subsets of data out.

X.eu = subset(X, continent == 'Europe')

X.uk = subset(X.eu, iso_code == 'GBR') 

data = X.uk %>% select(date, new_cases, total_cases, new_deaths)

par(mfrow=c(1,1))
plot(data$date, data$new_cases)
lines(data$date, data$new_deaths)
plot(data$date, data$total_cases)


#Akmal stuff
library(plotly)
#Create interactive chart of all country case data
plot_ly(X.eu,x=~date,y=~new_cases,color = ~iso_code) %>% add_lines()

#find negative spikes
X.eu %>% filter(new_cases<0)

#reformat dates
X.eu$date<-X.eu$date %>% lubridate::ymd()

#Find what days negative spikes occur
X.eu %>% filter(new_cases<0,iso_code=="FRA") %>% mutate(day=weekdays(date)) %>% count(day)

#creat a MA function
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

#view with a MA(7)
X.eu$new_cases_ma<-ma(X.eu$new_cases,7)
plot_ly(X.eu,x=~date,y=~new_cases_ma,color = ~iso_code) %>% add_lines()

#view with a MA(4)
X.eu$new_cases_ma<-ma(X.eu$new_cases,4)
plot_ly(X.eu,x=~date,y=~new_cases_ma,color = ~iso_code) %>% add_lines()
# If you want to plot stuff on a map you might want to check out this tutorial:
# https://egallic.fr/en/european-map-using-r/

X.eu = subset(X, continent == 'Europe')

X.uk = subset(X.eu, iso_code == 'GBR')

X.deu = subset(X.eu, iso_code == 'DEU')

X.fra = subset(X.eu, iso_code == 'FRA')

X.ita = subset(X.eu, iso_code == 'ITA')


data.uk = X.uk %>% select(date, new_cases, total_cases, new_deaths)

data.deu = X.deu %>% select(date, new_cases, total_cases, new_deaths)

data.fra = X.fra %>% select(date, new_cases, total_cases, new_deaths)

data.ita = X.ita %>% select(date, new_cases, total_cases, new_deaths)


data.uk$date = as.Date(data.uk$date)
data.uk = arrange(data.uk,date)

data.deu$date = as.Date(data.deu$date)
data.deu = arrange(data.deu,date)

data.fra$date = as.Date(data.fra$date)
data.fra = arrange(data.fra,date)

data.ita$date = as.Date(data.ita$date)
data.ita = arrange(data.ita,date)


plot(data.uk$date, data.uk$new_cases, type="l", main="COVID in the UK")
lines(data.uk$date, data.uk$new_deaths, col="blue")
plot(data.uk$date, data.uk$total_cases, type="l")

plot(data.deu$date, data.deu$new_cases, type="l", main="COVID in Germany")
lines(data.deu$date, data.deu$new_deaths, col="blue")
plot(data.deu$date, data.deu$total_cases, type="l")

plot(data.fra$date, data.fra$new_cases, type="l", main="COVID in France")
lines(data.fra$date, data.fra$new_deaths, col="blue")
plot(data.fra$date, data.fra$total_cases, type="l")

plot(data.ita$date, data.ita$new_cases, type="l", main="COVID in Italy")
lines(data.ita$date, data.ita$new_deaths, col="blue")
plot(data.ita$date, data.ita$total_cases, type="l")


ukNewCasests=ts(data.uk$new_cases)
ukNewDeathsts=ts(data.uk$new_deaths)
ukTotalCasests=ts(data.uk$total_cases)
plot(ukNewCasests)
plot(ukNewDeathsts)
plot(ukTotalCasests)
acf(ukNewCasests)
acf(ukNewDeathsts, na.action = na.pass)#lag 7
acf(ukTotalCasests)#lag 7
pacf(ukNewCasests)#peak:1 sf:1,2,13,16,23,25
pacf(ukNewDeathsts, na.action = na.pass)#peak:1 sf:3,5,6,7,8,9,11,15
pacf(ukTotalCasests)#peak:1

deuNewCasests=ts(data.deu$new_cases)
deuNewDeathsts=ts(data.deu$new_deaths)
deuTotalCasests=ts(data.deu$total_cases)
plot(deuNewCasests)
plot(deuNewDeathsts)
plot(deuTotalCasests)
acf(deuNewCasests)#lg 7
acf(deuNewDeathsts, na.action = na.pass)#lg7
acf(deuTotalCasests)
pacf(deuNewCasests)#pk:1 sf:3,4,5,6,7,8,9,10,16,21,23,24
pacf(deuNewDeathsts, na.action = na.pass)#pk:1 sf:3,4,5,6,7,8,9,22
pacf(deuTotalCasests)#pk:1

fraNewCasests=ts(data.fra$new_cases)
fraNewDeathsts=ts(data.fra$new_deaths)
fraTotalCasests=ts(data.fra$total_cases)
plot(fraNewCasests)
plot(fraNewDeathsts)
plot(fraTotalCasests)
acf(fraNewCasests)#lg?
acf(fraNewDeathsts, na.action = na.pass)#lg
acf(fraTotalCasests)
pacf(fraNewCasests)#pk:1 sf:2,3,4,5,6,7,9,11,13
pacf(fraNewDeathsts, na.action = na.pass)#pk:1 sf:2,3,4,,6,7,9,10,13,14,15,17
pacf(fraTotalCasests)#pk:1

itaNewCasests=ts(data.ita$new_cases)
itaNewDeathsts=ts(data.ita$new_deaths)
itaTotalCasests=ts(data.ita$total_cases)
plot(itaNewCasests)
plot(itaNewDeathsts)
plot(itaTotalCasests)
acf(itaNewCasests, na.action = na.pass)
acf(itaNewDeathsts, na.action = na.pass)
acf(itaTotalCasests, na.action = na.pass)
pacf(itaNewCasests, na.action = na.pass)#pk:1 4,5,8
pacf(itaNewDeathsts, na.action = na.pass)#pk:1,2,3,5,6,7,8,9,10,22
pacf(itaTotalCasests, na.action = na.pass)#pk:1

#lag of 7 throughout - data dependant on the day of the week


