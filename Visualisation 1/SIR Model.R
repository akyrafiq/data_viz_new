library(plotly)
library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value ='value', NodeID = "name",
             Group = "group", opacity = 0.4)
#these libraries need to be loaded
library(utils)

## Load deSolve package
library(deSolve)

## Create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1.4247, gamma = 0.14286)
## Time frame
times      <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Susceptible and Recovered", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)

## Add legend
legend(40, 0.7, c("Susceptible", "Infected", "Recovered"), pch = 1, col = 2:4, bty = "n")


library(tidyverse)
#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

write.csv(data,"Corona.csv")

#Data exploration
names(data)
unique(data %>% select(geoId,countriesAndTerritories,countryterritoryCode,popData2018))
data[data$geoId=="JPG11668",]


df <- data %>% select(countriesAndTerritories,dateRep,deaths)

forecast(ets(wdi_time_series()),
         h = input$forecast_n_months / 12,
         level = 95) %>% hchart %>%
  #This part allows for more inof to be included as we hover over the graph
  hc_tooltip(
    valuePrefix = "There were ",
    valueSuffix = " births per women",
    valueDecimals = 2
  ) %>%
  hc_title(text = "Fertility Forecast for the US")
