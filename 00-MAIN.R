####################################################################################
####################################################################################
#                                 Kickstarter project                              #
####################################################################################
####################################################################################

####################################################################################
#                                      Package                                     #
####################################################################################
library(data.table)
library(dplyr)
library(ggplot2)
library(highcharter)
####################################################################################
#                                      Import data                                 #
####################################################################################

live<-data.frame(read.csv(file = "00-BASE/live.csv"),row.names = TRUE)
most_backed<-data.frame(read.csv(file = "00-BASE/most_backed.csv"),row.names = TRUE)

####################################################################################
#                                     Analyse data                                 #
####################################################################################

summary(live)
summary(most_backed)
colnames(live)

live %>%
  group_by(country)%>%
  summarize(n=n()) %>%
  
  ggplot()+
  geom_bar(aes(x=country,y=n,fill=country),stat="identity")

live %>%
  group_by(country)%>%
  summarize(n= n()) %>%
  mutate(n   = round(n/sum(n)*100,2))-> live_by_country


highchart() %>% 
  hc_add_series_labels_values(live_by_country$country, 
                              live_by_country$n, 
                              type = "pie",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_country$country) %>%
  hc_title(text = "Kickstart by country") %>%
  hc_legend(enabled = FALSE)


library(treemap)
data("GNI2014")
GNI2014$iso2<-substr(GNI2014$iso3,1,2)

live_by_country %>%
  mutate(iso2 = as.character(live_by_country$country)) %>%
  left_join(GNI2014,by=c("iso2"="iso2")) -> bla2


highchart() %>% 
  hc_add_series_map(worldgeojson, bla2,
                    value = "n", joinBy = "iso3")
