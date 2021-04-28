library(tidyverse)

sw03 <- read_csv("https://shiny.cepremap.fr/data/EA_SW_rawdata.csv") %>%
  filter(period >="1980-01-01")

fipu <- read_csv("https://shiny.cepremap.fr/data/EA_Fipu_rawdata.csv")

finance <- read_csv("https://shiny.cepremap.fr/data/EA_Finance_rawdata.csv")

open <- read_csv("https://shiny.cepremap.fr/data/EA_Open_rawdata.csv") 

EA_rawdata <-
  sw03 %>% 
  inner_join(fipu,by="period") %>% 
  inner_join(finance,by="period") %>% 
  inner_join(open,by="period") %>% 
  rename(unempbenef=unemp) %>% 
  mutate(pop=1000*pop) %>% 
  add_column(country="EA")

library("rdbnomics")
wage_de_fr <- rdb("Eurostat","namq_10_a10",mask = "Q.CP_MEUR.SA.TOTAL.D1.DE+FR") %>% 
              add_column(var="wage")

wage_es_it <- rdb("Eurostat","namq_10_a10", mask = "Q.CP_MEUR.SCA.TOTAL.D1.ES+IT") %>% 
              add_column(var="wage")

hours <-rdb("Eurostat","namq_10_a10_e",mask = "Q.THS_HW.TOTAL.SCA.EMP_DC.IT+DE+FR+ES") %>% 
        add_column(var="hours")

gdp <- rdb("Eurostat","namq_10_gdp",mask = "Q.CLV10_MEUR.SCA.B1GQ.IT+DE+FR+ES") %>% 
       add_column(var="gdp")

conso <- rdb("Eurostat","namq_10_gdp",mask = "Q.CLV10_MEUR.SCA.P31_S14_S15.IT+DE+FR+ES") %>% 
         add_column(var="conso")

inves <- rdb("Eurostat","namq_10_gdp",mask = "Q.CLV10_MEUR.SCA.P51G.IT+DE+FR+ES") %>% 
         add_column(var="inves")

defgdp <- rdb("Eurostat","namq_10_gdp",mask = "Q.PD10_EUR.SCA.B1GQ.IT+DE+FR+ES") %>% 
          add_column(var="defgdp")

definves <- rdb("Eurostat","namq_10_gdp",mask = "Q.PD10_EUR.SCA.P51G.IT+DE+FR+ES") %>% 
            add_column(var="definves")

pop_recent <-  rdb("Eurostat","lfsq_pganws",mask = "Q.THS.T.TOTAL.Y15-64.POP.IT+DE+FR+ES") %>% 
               add_column(var="pop_recent")

pop_old <- rdb("Eurostat","demo_pjanbroad",mask = "A.NR.Y15-64.T.IT+DE+FR+ES") %>% 
           add_column(var="pop_old")

pubcons_recent_fr <-  rdb("Eurostat","gov_10q_ggnfa",mask = "Q.MIO_EUR.SCA.S13.P3.FR") %>% 
                      add_column(var="pubcons_recent")

pubcons_recent_it_de_es_nsa <- rdb("Eurostat","gov_10q_ggnfa",mask = "Q.MIO_EUR.NSA.S13.P3.IT+DE+ES") %>% 
                               add_column(var="pubcons_recent")

pubcons_old_it_de <- rdb("Eurostat","gov_10a_main",mask = "A.MIO_EUR.S13.P3.IT+DE") %>% 
                     add_column(var="pubcons_old")

df_nsa_q <- pubcons_recent_it_de_es_nsa %>%
            select(period,country=geo, value)

to_deseason <- df_nsa_q %>%
               spread(country, value)

deseasoned_q <-  bind_rows(lapply(unique(df_nsa_q$country), 
                           function(var)deseason(var_arrange = var,
                                          source_df = to_deseason)))%>% 
                 mutate(Origin = "Adjusted Series",country=var)%>%
                 select(-var)

df_nsa_q %<>% mutate(Origin = "Unadjusted Series")

plot_df <- bind_rows(df_nsa_q,deseasoned_q) %>%
           na.omit()

ggplot(plot_df,aes(period,value,colour=Origin))+
  geom_line()+
  facet_wrap(~country ,scales ="free_y",ncol = 2)+
  scale_x_date(expand = c(0.01,0.01)) +
  theme + xlab(NULL) + ylab(NULL) + 
  theme(legend.title=element_blank()) +
  theme(strip.text = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  ggtitle("Government Consumption")