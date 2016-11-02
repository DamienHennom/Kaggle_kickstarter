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
  mutate(n   = round(n/sum(n)*100,2))-> bla


highchart() %>% 
  hc_add_series_labels_values(bla$country, 
                              bla$n, 
                              type = "pie",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = bla$country) %>%
  hc_title(text = "Kickstart by country") %>%
  hc_legend(enabled = FALSE)



