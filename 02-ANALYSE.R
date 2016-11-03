####################################################################################
####################################################################################
#                                 Kickstarter project                              #
####################################################################################
####################################################################################

####################################################################################
#                                     Analyse data                                 #
####################################################################################

summary(live)
summary(most_backed)
colnames(live)

####################################################################################
#                                     Graphs                                       #
####################################################################################

#############################################
#           Per country                     #
#############################################
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
  hc_title(text = "Kickstarter by country") %>%
  hc_legend(enabled = FALSE)

pass<-data.frame(iso2=c("AE",
                        "AF",
                        "AT",
                        "AU",
                        "AU",
                        "BE",
                        "BE",
                        "BG",
                        "BG",
                        "BW",
                        "BY",
                        "CA",
                        "CA",
                        "CH",
                        "CH",
                        "CH",
                        "CL",
                        "CN",
                        "CO",
                        "CO",
                        "CO",
                        "CO",
                        "CR",
                        "CU",
                        "CY",
                        "CZ",
                        "DE",
                        "DK",
                        "EC",
                        "EE",
                        "EG",
                        "ES",
                        "ES",
                        "FR",
                        "GB",
                        "GR",
                        "GR",
                        "GT",
                        "HK",
                        "HT",
                        "HU",
                        "ID",
                        "IE",
                        "IL",
                        "IN",
                        "IR",
                        "IR",
                        "IR",
                        "IS",
                        "IS",
                        "IT",
                        "JP",
                        "KE",
                        "KH",
                        "KR",
                        "LB",
                        "LB",
                        "LB",
                        "LT",
                        "LU",
                        "MX",
                        "NL",
                        "NO",
                        "NP",
                        "NZ",
                        "PE",
                        "PH",
                        "PL",
                        "PT",
                        "RO",
                        "RU",
                        "RW",
                        "SE",
                        "SG",
                        "SI",
                        "SV",
                        "SV",
                        "TH",
                        "TR",
                        "TW",
                        "UA",
                        "US",
                        "VC",
                        "VI",
                        "VN"),
                 iso3=c("UAE",
                        "AFG",
                        "ATG",
                        "AUS",
                        "AUT",
                        "BEL",
                        "BEN",
                        "BGR",
                        "BGD",
                        "BWA",
                        "BLR",
                        "CAN",
                        "CAF",
                        "CHE",
                        "CHL",
                        "CHN",
                        "CHL",
                        "CHN",
                        "COL",
                        "COG",
                        "COM",
                        "COD",
                        "CRI",
                        "CUB",
                        "CYP",
                        "CZE",
                        "DEU",
                        "DNK",
                        "ECU",
                        "EST",
                        "EGY",
                        "ESP",
                        "EST",
                        "FRA",
                        "GBR",
                        "GRC",
                        "GRD",
                        "GTM",
                        "HKG",
                        "HTI",
                        "HUN",
                        "IDN",
                        "IRL",
                        "ISR",
                        "IND",
                        "IRL",
                        "IRQ",
                        "IRN",
                        "ISL",
                        "ISR",
                        "ITA",
                        "JPN",
                        "KEN",
                        "KHM",
                        "KOR",
                        "LBN",
                        "LBY",
                        "LBR",
                        "LTU",
                        "LUX",
                        "MEX",
                        "NLD",
                        "NOR",
                        "NPL",
                        "NZL",
                        "PER",
                        "PHL",
                        "PLW",
                        "PRT",
                        "ROU",
                        "RUS",
                        "RWA",
                        "SEN",
                        "SGP",
                        "SVN",
                        "SVN",
                        "SVK",
                        "THA",
                        "TUR",
                        "TWN",
                        "UKR",
                        "USA",
                        "VCT",
                        "VIR",
                        "VNM"))
#Map
pass%>%
  distinct(iso2)-> pass
live_by_country %>%
  mutate(iso2 = as.character(live_by_country$country)) %>%
  left_join(pass,by=c("iso2"="iso2"))-> map
  
highchart() %>% 
  hc_add_series_map(map=worldgeojson, df=map,
                    value = "n", joinBy = "iso3") %>%
  hc_title(text = "Kickstarter by country") %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_colors(heat.colors(2))

#############################################
#           Per currency                    #
#############################################
live %>%
  group_by(currency)%>%
  summarize(n= n()) %>%
  mutate(n   = round(n/sum(n)*100,2)) %>%
  arrange(n)-> live_by_currency


highchart() %>% 
  hc_add_series_labels_values(live_by_currency$currency, 
                              live_by_currency$n, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_currency$currency) %>%
  hc_title(text = "Kickstarter by currency") %>%
  hc_legend(enabled = FALSE)

most_backed %>%
  group_by(currency)%>%
  summarize(n= n()) %>%
  mutate(n   = round(n/sum(n)*100,2)) %>%
  arrange(n)-> most_backed_by_currency


highchart() %>% 
  hc_add_series_labels_values(most_backed_by_currency$currency, 
                              most_backed_by_currency$n, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = most_backed_by_currency$currency) %>%
  hc_title(text = "Kickstarter most by currency") %>%
  hc_legend(enabled = FALSE)
#############################################
#                Per Type                   #
#############################################
live %>%
  group_by(type)%>%
  summarize(n= n()) %>%
  mutate(n   = round(n/sum(n)*100,2)) %>%
  arrange(n)-> live_by_type


highchart() %>% 
  hc_add_series_labels_values(live_by_type$type, 
                              live_by_type$n, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_type$type) %>%
  hc_title(text = "Kickstarter by type") %>%
  hc_legend(enabled = FALSE)


#############################################
#                Per Category               #
#############################################
most_backed %>%
  group_by(category)%>%
  summarize(n= n()) %>%
  mutate(n   = round(n/sum(n)*100,2)) %>%
  arrange(n)-> most_backed_by_category


highchart() %>% 
  hc_add_series_labels_values(most_backed_by_category$category, 
                              most_backed_by_category$n, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = most_backed_by_category$category) %>%
  hc_title(text = "Kickstarter most by category") %>%
  hc_legend(enabled = FALSE)

#############################################
#                Per amount                 #
#############################################
live %>%
  group_by(country)%>%
  summarize(amt= mean(amt.pledged)) %>%
  arrange(amt)-> live_by_country


highchart() %>% 
  hc_add_series_labels_values(live_by_country$country, 
                              live_by_country$amt, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_country$country) %>%
  hc_title(text = "Kickstarter average amt by country") %>%
  hc_legend(enabled = FALSE)

most_backed %>%
  group_by(country)%>%
  summarize(amt= mean(amt.pledged)) %>%
  arrange(amt)-> live_by_country


highchart() %>% 
  hc_add_series_labels_values(live_by_country$country, 
                              live_by_country$amt, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_country$country) %>%
  hc_title(text = "Kickstarter average amt by country") %>%
  hc_legend(enabled = FALSE)

#############################################
#                Per goal                   #
#############################################
live %>%
  group_by(country)%>%
  summarize(amt= mean(amt.pledged)) %>%
  arrange(amt)-> live_by_country


highchart() %>% 
  hc_add_series_labels_values(live_by_country$country, 
                              live_by_country$amt, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_country$country) %>%
  hc_title(text = "Kickstarter average amt by country") %>%
  hc_legend(enabled = FALSE)

#############################################
#                Per percent                #
#############################################
live %>%
  group_by(country)%>%
  summarize(pct = mean(percentage.funded)) %>%
  arrange(pct)-> live_by_pct


highchart() %>% 
  hc_add_series_labels_values(live_by_pct$country, 
                              live_by_pct$pct, 
                              type = "bar",
                              colorByPoint = TRUE) %>%
  hc_xAxis(categories = live_by_pct$country) %>%
  hc_title(text = "Kickstarter average percentage.funded by country") %>%
  hc_legend(enabled = FALSE)

#############################################
#                    Blurb                  #
#############################################
word<-unlist(strsplit(as.character(unlist(live$title[1:10])),split=" "))
wordcloud(word,freq=rep(1,length(word)))
live$title[1]
