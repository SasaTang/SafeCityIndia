# Basic maps of data
install.packages("tidyverse")
install.packages("png")
library(tidyverse)
library(dplyr)
maindf <- read_csv("SafeCityClean.csv")

# Break down by year 
table1 <- ggplot(scdf, aes(YEAR))
table1 + geom_bar() + labs(title = "Figure 1. Incident Report Frequency by Year",
                           x = "Year", y = "Incident count")
# By Month since 2013? DOES NOT WORK B/C MONTH... do I need this?
tabl2 <- ggplot(maindf, aes(date)) 
table2 + geom_bar() + labs(title = "Figure 1. Incident Report Frequency by Date",
                           x = "Year", y = "Incident count")

# Remove flag units
names(maindf)
maindf <- select(maindf, id, date, year, month, time, X, Y, catcall, comment, sexualinvite,
                 staring, takepic, indecentexpo, touch,
                  stalking, sexassault, others, verbalabuse, phyabuse, seriousphyabuse,
                  otherabuse, citycode, areacode, taxi, car, train, bus, metro, auto,
                  rail, airport, road, travel)
# subset non transit to transit coordinates
maindf$transit<-ifelse(maindf$taxi == 1,1, 
                                 ifelse(maindf$car == 1,1, 
                                        ifelse(maindf$train== 1,1,
                                               ifelse(maindf$bus== 1,1,
                                                      ifelse(maindf$metro== 1,1,
                                                             ifelse(maindf$auto== 1,1,   
                                                                    ifelse(maindf$rail== 1,1,   
                                                                           ifelse(maindf$airport== 1,1,   
                                                                                  ifelse(maindf$travel== 1,1,0   
                                               )))))))))

# remove observations outside of India
maindf <- maindf %>%
  filter(id != 5799) %>%
  filter(id != 5870) %>%
  filter(id != 5806) %>%
  filter(id != 5982) %>%
  filter(id != 5721) %>%
  filter(id != 5807) %>%
  filter(id != 9498) %>%
  filter(id != 1007) %>%
  filter(id != 5856) %>%
  filter(id != 5916) %>%
  filter(id != 6704) %>%
  filter(id != 5822) %>%
  filter(id != 5924) %>%
  filter(id != 5836) %>%
  filter(id != 5826) %>%
  filter(id != 6672) %>%
  filter(id != 6685) %>%
  filter(id != 5831) %>%
  filter(id != 5757) %>%
  filter(id != 5784) %>%
  filter(id != 1009) %>%
  filter(id != 1015) %>%
  filter(id != 3182) %>%
  filter(id != 9637) %>%
  filter(id != 179)  %>%
  filter(id != 7386) %>%
  filter(id != 9749) %>%
  filter(id != 9464) %>%
  filter(id != 9787) %>%
  filter(id != 2975) %>%
  filter(id != 1115) %>%
  filter(id != 3288) %>%
  filter(id != 4066) %>%
  filter(id != 7460) %>%
  filter(id != 7452) %>%
  filter(id != 1092) %>%
  filter(id != 1076) %>%
  filter(id != 10927) %>%
  filter(id != 10766) %>%
  filter(id != 11150) 

nontransit <- filter(maindf, transit == 0)
names(nontransit)
nontransit <- nontransit %>%
  select(id, date, year, month, time, X, Y, catcall, comment, sexualinvite, 
         staring, takepic, indecentexpo, touch, stalking, sexassault, others,
         verbalabuse, phyabuse, seriousphyabuse, otherabuse, citycode, areacode)


transit <- filter(maindf, transit == 1)
names(transit)
transit <- transit %>%
  select(id, date, year, month, time, X, Y, catcall, comment, sexualinvite, 
         staring, takepic, indecentexpo, touch, stalking, sexassault, others,
         verbalabuse, phyabuse, seriousphyabuse, otherabuse, citycode, areacode)


write.csv(transit, file = "transit.csv")

# testing numbers
# only "others"
test1<-filter(nontransit, others ==1 & catcall ==0 & comment == 0 & sexualinvite == 0
              & staring == 0 & takepic == 0 & indecentexpo == 0 & touch == 0 & stalking == 0
              & sexassault ==0) 
stalkingtest <- filter(nontransit, others ==0 & catcall ==0 & comment == 0 & sexualinvite == 0
                      & staring == 0 & takepic == 0 & indecentexpo == 0 & touch == 0 & stalking == 1
                      & sexassault ==0) 

stalkingtest <- filter(nontransit, stalking == 1
                       & sexassault ==1) 


# Noninteractive incidents
# Indecent exposure, staring, taking pictures
nontransit$noninteractive <-ifelse(nontransit$indecentexpo == 1,1,
                                    ifelse(nontransit$staring ==1,1,
                                           ifelse(nontransit$takepic ==1,1,0)))

# stalking

# DirectVerbal Incidents
# Catcall, comment, sexual invite
nontransit$directverbal <- ifelse(nontransit$catcall == 1,1,
                                   ifelse(nontransit$comment ==1,1,
                                          ifelse(nontransit$sexualinvite ==1,1,0)))

# Physical incidents
# touch, sexual assault
test2<-filter(nontransit, touch ==0 & sexassault ==1) #104
nontransit$physical <- ifelse(nontransit$touch == 1,1,
                               ifelse(nontransit$sexassault ==1,1,0))
                                     
write.csv(nontransit, file = "nontransit.csv")
View(nontransit)

nontransit1 <- read.csv("nontransit.csv")

#Cluster
testcluster <-cbind(gincidents$X, gincidents$Y)
testcluster1 <-kmeans(testcluster, centers = 4)

png("cluster.png")
plot(testcluster, col = testcluster1$cluster +1, xlab = "Longitude",
     ylab = "Latitide", 
     main = "Clustering Incidents in New Delhi")
plot(delhi.roads, add=TRUE, col = rgb(.5,.5,.5, alpha = 0.3)) 
plot(delhi.waterbody, add = TRUE, col= "lightblue") 
dev.off()

names(gincidents)
table(catagory = gincidents$catagory, cluster =testcluster1$cluster )

# understanding time and category of sexual harassment
# create own category via gather
Delhi.nontransit <- read.csv("Delhi.csv")
names(Delhi.nontransit)
gatherincidents <- Delhi.nontransit %>%
  gather('noninteractive', 'directverbal', 'physical', 'stalking', 'others', key = "catagory", value = 'cases')
View(gatherincidents)
names(gatherincidents)
sum(is.na(gatherincidents$catagory)) #none
gatherincidents$cat <-as.factor(gatherincidents$catagory)
class(gatherincidents$cat)
# remove 0 in cases
gincidents <- filter(gatherincidents, cases == 1)
class(gatherincidents$cases) #4429
names(gatherincidents)
head(gincidents)
View(gincidents)
class(gincidents$cat) # factor

data(mtcars)
View(mtcars)
?mtcars
class(mtcars$vs) #numeric, this is my categories
class(mtcars$gear) #numeric, this is my x
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))
View(counts)
# therefore, need to change my time into categories 
class(gincidents$time)# factor
View(gincidents)
library(hms)
gincidents$time <-parse_time(gincidents$time) #does not produce numeric value
# let's bin time
gincidents$timebin <- ifelse(gincidents$touch == 1,1,
                             ifelse(nontransit$sexassault ==1,1,0))
timebin <- gincidents$time
cut(timebin, 4)
class(timebin) #not numeric
# 3-8am
# 8-1pm
# 1-6pm
# 6-11
# 11-3

# Do I need to change my SH categories into numeric?
