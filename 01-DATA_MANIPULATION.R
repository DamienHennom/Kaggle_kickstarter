####################################################################################
####################################################################################
#                                 Kickstarter project                              #
####################################################################################
####################################################################################

####################################################################################
#                                      Import data                                 #
####################################################################################

live<-data.frame(read.csv(file = "00-BASE/live.csv"),row.names = TRUE)
most_backed<-data.frame(read.csv(file = "00-BASE/most_backed.csv"),row.names = TRUE)

#MAP
data(worldgeojson,package="highcharter")

