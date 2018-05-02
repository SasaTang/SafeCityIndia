# Load shapefiles
india.states<- readShapePoly("IND_adm1.shp")
india.districts<- readShapePoly("IND_adm2.shp")
india.subdis <- readShapePoly("IND_adm3.shp")
plot(india.states)

india.roads <- readShapeLines("roads.shp")
india.water <- readShapeLines("waterways.shp")

# Delhi files
delhi.roads <- readShapeLines("delhi_highway.shp")
delhi.water <- readShapeLines("delhi_water.shp")
delhi.fishnet <-readShapeLines("fishnet7.shp")
delhi.waterbody <- readShapePoly("WATER_BODY.shp")
try1 <-readShapeLines("Join_Output.shp")
try2 <-readShapePoly("Join_Output.shp")


# event points
#using google maps
nontransit <- read_csv("nontransit.csv")
View(nontransit)
lat <- as.vector(nontransit$Y)
long <- as.vector(nontransit$X)
tmp <- nontransit$X1
par(mar = c(0,0,0,0))

mymap <- MapBackground(lat= lat, lon= long, zoom = 1)
PlotOnStaticMap(mymap,lat,long, cex = 0.5, pch =1, zoom = 1)
try9<-get_map(location = c(lon = 28.6139, lat = 77.2090), zoom = 12, maptype = 'roadmap')
ggmap(try9)
ggmap(try9) + geom_point(data=nontransit, aes(x=X, y= Y), color = "red") 

delhi <- plotmap(lat = 28.6139, lon =77.2090 , zoom = 12)

# new try
# example
data(incidents)
class(incidents) #df
class(gatherincidents)
gatherincidents2 <-as.data.frame(gatherincidents)
class(gatherincidents2) # as dataframe doesn't help produce map
write.csv(incidents, file = "incidents.csv")
write.csv(gincidenttry, file = "merge2.csv")
write_csv(df4, file = "SafeCityClean.csv")

View(incidents)
col=as.numeric(incidents$Category)
class(incidents$Category)
par(pty="s")
mapSF_Z15 = plotmap(lat, lon, zoom = 13, col = col, pch=20, data = incidents)
mapSF_Z15 = plotmap(lat, lon, zoom = 10, col = col, pch=20, data = gincidents)

# try clean incident with lat, long and category
names(incidents)
incidenttry <- select(incidents, Category, lon, lat)
col<-as.numeric(tryagain$Category)
summary(tryagain$Category) 
# these are the same but does not graph similarly? 
range(tryagain$lon) 
range(incidents$lon)
View(tryagain)
summary(col)

col<-as.numeric(tryagain$Category)
pdf("allcatagories.pdf")
mapSF_Z15 = plotmap(lat, lon, zoom = 5, col = col, pch=20, data = tryagain)
dev.off()
col<-as.numeric(incidents$Category)
mapSF_Z15 = plotmap(lat, lon, zoom = 8, col = col, pch=20, data = incidents)
?plotmap

View(incidenttry)
names(incidenttry)
class(incidenttry$lat)
names(gincidents)
SHincidents <- select(gincidents, cat, lon, lat)
names(SHincidents)
View(SHincidents)
class(SHincidents$cat)
class(col)
col<-as.numeric(SHincidents$cat)
mapDelhi = plotmap(lat, lon, zoom = 1, col = col, pch=20, data = SHincidents) #does not work
write_csv(SHincidents, "Sexualharassment.csv")
write_csv(incidenttry, "tryagain.csv")
tryagain<- read.csv("tryagain.csv")
tryagain<- tryagain[1:5000,]
View(tryagain)

class(incidents$lon)

# create own category via gather
names(nontransit)
gatherincidents <- nontransit %>%
  gather('noninteractive', 'directverbal', 'physical', 'stalking', 'others', key = "catagory", value = 'cases')
View(gatherincidents)
names(gatherincidents)
sum(is.na(gatherincidents$catagory)) #none
gatherincidents$cat <-as.factor(gatherincidents$catagory)
class(gatherincidents$cat)
col=as.numeric(gatherincidents$cat) 
gatherincidents<- rename(gatherincidents, lon = X)
gatherincidents<- rename(gatherincidents, lat = Y)
# remove 0 in cases
gincidents <- filter(gatherincidents, cases == 1)
class(gatherincidents$cases)
names(gatherincidents)
head(gincidents)
View(gincidents)
col=as.numeric(gincidents$cat) 
gincidenttry<- gincidents[1:5000,]
names(gincidents)
levels(gincidents$cat)
summary(gincidents$cat)
# Does not work 
mapDelhi = plotmap(lat, lon, zoom = NULL, col = col, pch=20, data = gincidents)
class(gatherincidents$lat)
mergetry <-read.csv("pleaseword.csv")
names(mergetry)
col=as.numeric(mergetry$cat) 
mapDelhi = plotmap(lat, lon, zoom = NULL, col = col, pch=20, data = mergetry)
# still error?
range(incidents$lat)
sum(is.na(incidents$lat)) #0
range(mergetry$lat) #looks fine
sum(is.na(mergetry$lat)) #0
#WHY DOES THIS NOT WORK?????

mergetry2<-mergetry[1:5000,]
col=as.numeric(mergetry2$cat) 
mapDelhi = plotmap(lat, lon, zoom = NULL, col = col, pch=20, data = mergetry)


# delhi2<- GetMap(center= center, zoom = 15) #nothing
# delhi3<- plotmap(Y, X, zoom = 15,  maptype = "roadmap", col=col, pch= 20, data= nontransit) #nothing
# delhi4<- RgoogleMaps::plotmap(lat = lat, lon = long, zoom = 15, col=col, pch= 20, data= mymap) #nothing


install.packages("ggmap")
library(ggmap) #does not work

#play with map type
mymap <- MapBackground(lat= lat, lon= long, zoom = 10, maptype = "satellite")
PlotOnStaticMap(mymap, lat, long, cex = 1, pch =1, col = '#FB6A4A80') 

#create clip area
data(newhaven)
class(newhaven)
xmin <- bbox(roads)[1,1]

table(nontransit$directverbal)

data(tornados)
class(tornados)
class(torn)
View(torn)

# kernel density estimates
class(breach) #polypoints, SpatialPoints
View(breach) #just lat and long
breach.dens <-kde.points(breach, lims= tracts)
level.plot(breach.dens)
masker <-poly.outer(breach.dens, tracts, extend = 100)
add.masking(masker)
plot(tracts, add= TRUE)
head(breach.dens)
plot(breach.dens)

# data transformation
# from dataframe into SpatialPointsDataFrame, can it have words?
sp <- cbind(nontransit$X, nontransit$Y)
incident <- SpatialPointsDataFrame(sp, nontransit)
incident.dens <-kde.points(incident, lims= tracts)
level.plot(incident.dens) #this does not work
masker <-poly.outer(incident.dens, tracts, extend = 100)
add.masking(masker)
plot(tracts, add= TRUE)
class(incident)

# try just lat and long
try1 <- select(nontransit, Y, X)
View(try1)
try1 <-rename(Y = "Lat", X = "Long", try1) # leave off now
try2 <- SpatialPoints(try1)
View(try2)
class(try2) #SpatialPoints
incident.dens <-kde.points(try2, lims= tracts) #works
level.plot(incident.dens) #does not work
?breach
class(breach)
plot(incident.dens) #this is nothing, very different than breach.dens
incident.dens

# another try
install.packages("spatstat")
library(spatstat)
try3<- ppp(x = nontransit$X,y = nontransit$Y, nontransit)
den=density(try3,kernel="gaussian",edge=T,diggle=T,adjust=0.6)
plot(den,main='try',col=plasma)
contour(den,add=T,col=plasma(15))

# another try
m <- ggplot(nontransit, aes(x = X, y = Y)) +
  geom_point()
m + geom_density_2d() 
# how to crop it to only Delhi?
clip <- gIntersection(try2, incident, byid = TRUE) # do not do, takes forever!

class(breach) #polypoints, SpatialPoints, but lat and long are strange values
class(AoI.SH) #SpatialPoints
View(AoI.SH)
test <-as.data.frame(breach)
class(test)
View(test)

# using ggplot
View(gincidents) # use this
ggplot() + geom_point(data=gincidents, aes(x=lon, y=lat), color="red") #Error in plot_clone(p) : attempt to apply non-function
ggplot(aes(x=lon, y = lat), data = gincidents) #does this mean I need a map?

Delhionly<- qmap(location = "marrs mclean science, waco, texas")
qmap(qstate, googleMap = TRUE)
qmap(location = "baylor university",  googleMap = TRUE, zoom = 14, maptype = "toner-lite", source = "stamen")
houston <- get_map('houston', zoom = 14) 
HoustonMap <- ggmap(houston, extent = 'device', legend = 'topleft') #Error in plot_clone(p) : attempt to apply non-function

# is there something wrong with my ggplot? YES
ggplot(diamonds) 
ggplot(diamonds, aes(x=carat)) 
library(ggplot2)
ggplot(diamonds, aes(x=carat, color=cut))
dim(diamonds)

# figure out how many datapoints are in New Delhi
