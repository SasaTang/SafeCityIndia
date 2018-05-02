delhiAll <- read.csv("delhiincidents.csv")
Delhi.nontransit <- read.csv("Delhi.csv")
mumbaiAll<- read.csv("mumbaiincidents.csv")
Mumbai.nontransit <- read.csv("Mumbai.csv")
# need to sort into nontransit
delhiAll$transit<-ifelse(delhiAll$taxi == 1,1, 
                       ifelse(delhiAll$car == 1,1, 
                              ifelse(delhiAll$train== 1,1,
                                     ifelse(delhiAll$bus== 1,1,
                                            ifelse(delhiAll$metro== 1,1,
                                                   ifelse(delhiAll$auto== 1,1,   
                                                          ifelse(delhiAll$rail== 1,1,   
                                                                 ifelse(delhiAll$airport== 1,1,   
                                                                        ifelse(delhiAll$travel== 1,1,0   
                                                                        )))))))))
View(Delhi.nontransit)
Delhi.nontransit <- filter(delhiAll, transit == 0)
names(Delhi.nontransit)
Delhi.nontransit <- Delhi.nontransit %>%
  select(id, date, year, month, time, X, Y, catcall, comment, sexualinvite, 
         staring, takepic, indecentexpo, touch, stalking, sexassault, others,
         verbalabuse, phyabuse, seriousphyabuse, otherabuse, citycode, areacode, noninteractive, directverbal, physical)


# write csv
write.csv(Delhi.nontransit, file = "Delhi.csv")

# KDE of verbal Delhi 
india.subdis <- readShapePoly("IND_adm3.shp")
index <- india.subdis$NAME_3 == "Delhi"
AoI <-india.subdis[index,]
verbal.df <- filter(Delhi.nontransit, directverbal == 1)
vsp <- cbind(Delhi.nontransit$X, Delhi.nontransit$Y) #of everything
SHverbal <- SpatialPointsDataFrame(vsp, Delhi.nontransit)
AoI.SHverbal<-gIntersection(AoI, SHverbal)
verbal.dens <-kde.points(AoI.SHverbal)
delhi.roads <- readShapeLines("delhi_highway.shp")
delhi.waterbody <- readShapePoly("WATER_BODY.shp")

png("kdeverbal.png")
level.plot(verbal.dens) 
title("Direct Verbal Harassment")
plot(AoI, add = TRUE)
plot(delhi.roads, add = TRUE, col = rgb(.5,.5,.5, alpha = 0.3)) 
plot(delhi.waterbody, add = TRUE, col= "lightblue") 
dev.off() 

# KDE of non-interactive
noninter.df <- filter(Delhi.nontransit, noninteractive == 1)
nonintersp <- cbind(noninter.df$X, noninter.df$Y) #of everything
SHnoninteractive <- SpatialPointsDataFrame(nonintersp, noninter.df)
AoI.noninteractive<-gIntersection(AoI, SHnoninteractive)
noninteractive.dens <-kde.points(AoI.noninteractive)

png("kdenoninteractive.png")
level.plot(noninteractive.dens)
title("Non-Interactive Harassment")
plot(AoI, add = TRUE)
plot(delhi.roads, add = TRUE, col = rgb(.5,.5,.5, alpha = 0.3)) 
plot(delhi.waterbody, add = TRUE, col= "lightblue") 
dev.off() 

# KDE of physical
physical.df <- filter(Delhi.nontransit, physical == 1)
physicalsp <- cbind(physical.df$X, physical.df$Y)
SHphysical <- SpatialPointsDataFrame(physicalsp, physical.df)
AoI.physical<-gIntersection(AoI, SHphysical)
Physical.dens <-kde.points(AoI.physical)

png("kdephysical.png")
level.plot(Physical.dens)
title("Physical Harassment")
plot(AoI, add = TRUE)
plot(delhi.roads, add = TRUE, col = rgb(.5,.5,.5, alpha = 0.3)) 
plot(delhi.waterbody, add = TRUE, col= "lightblue") 
dev.off() 

# KDE of stalking
stalk.df <- filter(Delhi.nontransit, stalking == 1)
stalksp <- cbind(stalk.df$X, stalk.df$Y) 
SHstalk <- SpatialPointsDataFrame(stalksp, stalk.df)
AoI.stalk<-gIntersection(AoI, SHstalk)
Stalk.dens <-kde.points(AoI.stalk)

png("kdestalk.png")
level.plot(Stalk.dens) 
title("Stalking Harassment")
plot(AoI, add = TRUE)
plot(delhi.roads, add = TRUE, col = rgb(.5,.5,.5, alpha = 0.3)) 
plot(delhi.waterbody, add = TRUE, col= "lightblue") 
dev.off() 

# KDE of other
names(nontransit)
other.df <- filter(Delhi.nontransit, others == 1)
othersp <- cbind(other.df$X, other.df$Y) 
SHothers <- SpatialPointsDataFrame(othersp, other.df)
AoI.other<-gIntersection(AoI, SHothers)
Other.dens <-kde.points(AoI.other)

png("kdeother.png")
level.plot(Other.dens) 
title("Other Harassment")
plot(AoI, add = TRUE)
plot(delhi.roads, add = TRUE, col = rgb(.5,.5,.5, alpha = 0.3)) 
plot(delhi.waterbody, add = TRUE, col= "lightblue") 
dev.off() 






# Mumbai All
mumbaiAll$transit<-ifelse(mumbaiAll$taxi == 1,1, 
                         ifelse(mumbaiAll$car == 1,1, 
                                ifelse(mumbaiAll$train== 1,1,
                                       ifelse(mumbaiAll$bus== 1,1,
                                              ifelse(mumbaiAll$metro== 1,1,
                                                     ifelse(mumbaiAll$auto== 1,1,   
                                                            ifelse(mumbaiAll$rail== 1,1,   
                                                                   ifelse(mumbaiAll$airport== 1,1,   
                                                                          ifelse(mumbaiAll$travel== 1,1,0   
                                                                          )))))))))
dim(Mumbai.nontransit)
Mumbai.nontransit <- filter(mumbaiAll, transit == 0)
names(Mumbai.nontransit)
Mumbai.nontransit <- Mumbai.nontransit %>%
  select(id, date, year, month, time, X, Y, catcall, comment, sexualinvite, 
         staring, takepic, indecentexpo, touch, stalking, sexassault, others,
         verbalabuse, phyabuse, seriousphyabuse, otherabuse, citycode, areacode, noninteractive, directverbal, physical)


# need to remove few
dim(Mumbai.nontransit)
Mumbai.nontransit <- Mumbai.nontransit %>%
  filter(id != 124) %>%
  filter(id != 4826) %>%
  filter(id != 3215)

# write csv
write.csv(Mumbai.nontransit, file = "Mumbai.csv")

india.districts<- readShapePoly("IND_adm2.shp")
class(india.districts) #SpatialPolygonsDataFrame
View(india.districts)
# AoI you want ObjectID 307 (the most concentrated place), 316 (top right), 319 (side), 320 (bottom), 326 (top)
plot(india.districts) 
index <- india.districts$NAME_2 == "Mumbai Suburban" 

  #india.districts$NAME_2 == "Neemuch" 
  
  #india.districts$NAME_2 == "Panna" |
  #india.districts$NAME_2 == "Satna" 
AoI <-india.districts[index,]
plot(AoI) # But onl Mumbai 
View(AoI) 

names(Mumbai.nontransit)
verbal.df <- filter(Mumbai.nontransit, directverbal == 1)
vsp <- cbind(Mumbai.nontransit$X, Mumbai.nontransit$Y) #of everything
SHverbal <- SpatialPointsDataFrame(vsp, Mumbai.nontransit)
class(SHverbal) #SpatialPointsDataFrame

AoI.SHverbal<-gIntersection(AoI, SHverbal)
verbal.dens <-kde.points(SHverbal)

level.plot(verbal.dens) #add labs
title("KDE of Direct Verbal Harassment Mumbai")
plot(AoI, add = TRUE) #somehow, cannot plot 
