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
pass<-data.frame(read.csv(file = "00-BASE/Matrix.csv",sep=";"))
data(worldgeojson)


#country
most_backed$country<-unlist(lapply(strsplit(as.character(most_backed$location),
                                            split = ","),
                                   FUN = function(x) x[2]))

bla<-as.data.frame(cbind(as.character(most_backed$location),most_backed$country))
write.csv(bla,file="most_backed.csv")

levels(unique(live$country))
