install.packages(c("maps", "mapdata", "maptools"))
install.packages("GISTools", dependencies = T)
install.packages(c("OpenStreetMap", "RgoogleMaps", "RBSmapping"), depend = T)
library(maps)
library(mapdata)
library(maptools)
library(RColorBrewer)
library(GISTools)
library(RgoogleMaps)

# Map 1: All India incidents/ only Delhi
nontransit <- read_csv("nontransit.csv")
lat <- as.vector(nontransit$Y)
long <- as.vector(nontransit$X)
tmp <- nontransit$X1

pdf("all.pdf")
mymap <- MapBackground(lat= lat, lon= long, zoom = 20)
PlotOnStaticMap(mymap,lat,long, cex = 0.5, pch =1, zoom = 20)
dev.off() 

# Map 2: Map with fill of catagories
library(tidyr)
gatherincidents <- nontransit %>%
  gather('noninteractive', 'directverbal', 'physical', 'stalking', 'others', key = "catagory", value = 'cases')
gatherincidents$cat <-as.factor(gatherincidents$catagory)
col=as.numeric(gatherincidents$cat) 
gatherincidents<- rename(gatherincidents, lon = X)
gatherincidents<- rename(gatherincidents, lat = Y)
# remove 0 in cases
gincidents <- filter(gatherincidents, cases == 1)
col=as.numeric(gincidents$cat) 
SHincidents <- select(gincidents, cat, lon, lat)
col<-as.numeric(SHincidents$cat)
write_csv(SHincidents, "Sexualharassment.csv")
write_csv(incidenttry, "tryagain.csv")
tryagain<- read.csv("tryagain.csv")

pdf("allcatagories.pdf")
mapSF_Z15 = plotmap(lat, lon, zoom = 5, col = col, pch=20, data = tryagain)
dev.off()

# Map 3: kernal density map of verbal
# first, clip to Delhi
india.subdis <- readShapePoly("IND_adm3.shp")
plot(india.subdis) 
index <- india.subdis$NAME_3 == "Delhi"
AoI <-india.subdis[index,]
plot(AoI)
sp <- cbind(nontransit$X, nontransit$Y) #of everything
SHincident <- SpatialPointsDataFrame(sp, nontransit)
class(SHincident) #SpatialPointsDataFrame
AoI.SH<-gIntersection(AoI, SHincident)
plot(AoI)
map<-plot(AoI.SH, add = T, pch = 1, byid= TRUE) 

# KDE of verbal
verbal.df <- filter(nontransit, directverbal == 1)
vsp <- cbind(verbal.df$X, verbal.df$Y) #of everything
SHverbal <- SpatialPointsDataFrame(vsp, verbal.df)

AoI.SHverbal<-gIntersection(AoI, SHverbal)
verbal.dens <-kde.points(AoI.SHverbal)
delhi.roads <- readShapeLines("delhi_highway.shp")
delhi.waterbody <- readShapePoly("WATER_BODY.shp")

pdf("kdeverbal.pdf")
level.plot(verbal.dens) #add labs
title("KDE of Direct Verbal Harassment")
plot(AoI, add = TRUE)
dev.off() 

pdf("kdeverbal2.pdf")
title("KDE of Direct Verbal Harassment")
plot(AoI)
plot(delhi.roads, add = TRUE) 
plot(delhi.waterbody, add = TRUE, col= "blue") 
level.plot(verbal.dens) #add labs
dev.off() #maybe I can add road?


# KDE of non-interactive
names(nontransit)
noninter.df <- filter(nontransit, noninteractive == 1)
nonintersp <- cbind(noninter.df$X, noninter.df$Y) #of everything
SHnoninteractive <- SpatialPointsDataFrame(nonintersp, noninter.df)
class(SHnoninteractive) #SpatialPointsDataFrame

AoI.noninteractive<-gIntersection(AoI, SHnoninteractive)
noninteractive.dens <-kde.points(AoI.noninteractive)

pdf("kdenoninteractive.pdf")
level.plot(noninteractive.dens) #add labs
title("KDE of Non-Interactive Harassment")
plot(AoI, add = TRUE)
dev.off() 

# KDE of physical
names(nontransit)
physical.df <- filter(nontransit, physical == 1)
physicalsp <- cbind(physical.df$X, physical.df$Y) #of everything
SHphysical <- SpatialPointsDataFrame(physicalsp, physical.df)
class(SHphysical) #SpatialPointsDataFrame

AoI.physical<-gIntersection(AoI, SHphysical)
Physical.dens <-kde.points(AoI.physical)

pdf("kdephysical.pdf")
level.plot(Physical.dens) #add labs
title("KDE of Physical Harassment")
plot(AoI, add = TRUE)
dev.off() 

# KDE of stalking
names(nontransit)
stalk.df <- filter(nontransit, stalking == 1)
stalksp <- cbind(stalk.df$X, stalk.df$Y) #of everything
SHstalk <- SpatialPointsDataFrame(stalksp, stalk.df)
class(SHstalk) #SpatialPointsDataFrame

AoI.stalk<-gIntersection(AoI, SHstalk)
Stalk.dens <-kde.points(AoI.stalk)

pdf("kdestalk.pdf")
level.plot(Stalk.dens) #add labs
title("KDE of Stalking Harassment")
plot(AoI, add = TRUE)
dev.off() #why are porportions off?

# KDE of other
names(nontransit)
other.df <- filter(nontransit, others == 1)
othersp <- cbind(other.df$X, other.df$Y) #of everything
SHothers <- SpatialPointsDataFrame(othersp, other.df)
class(SHothers) #SpatialPointsDataFrame

AoI.other<-gIntersection(AoI, SHothers)
Other.dens <-kde.points(AoI.other)

pdf("kdeother.pdf")
level.plot(Other.dens) #add labs
title("KDE of Other Harassment")
plot(AoI, add = TRUE)
dev.off() #why are porportions off?
