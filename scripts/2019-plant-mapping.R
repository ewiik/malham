## cleaning of 2019 survey data for mapping
## note that for UK,  WGS 84 / UTM zone 29N has code EPSG:32629 
##    (https://epsg.io/?q=United%20Kingdom%20%28UK%29%20kind%3APROJCRS)

## get libraries
library('ggplot2')
library('reshape2')
library('viridis')
library("grid")
library('gridExtra')
library("extrafont")
library('plyr')
library("dplyr")
library('gtable')
library(cowplot)
library(akima) # for generating regular information for the bathy

require(sf)
require(rgdal)
loadfonts()
require(gsl) 


## create function to create points along polygon
rPointOnPerimeter <- function(n, poly) {
  poly <- attr(poly,'polygons')
  xy <- poly[[1]]@Polygons[[1]]@coords
  dxy <- diff(xy)
  h <- hypot(dxy[,1], dxy[,2])
  
  e <- sample(nrow(dxy), n,replace=TRUE, prob=h)
  
  u <- runif(n)
  p <- xy[e,] + u * dxy[e,]
  
  p
}

## read in data and unify coordinate system
## (note https://gis.stackexchange.com/questions/166876/alternatives-of-sptransform)
plants <- read.csv('../dat-orig/wiiketal/2019-macrophytes-survey.csv', stringsAsFactors = F)

mal <- readOGR(dsn = "../dat-orig/shapes", layer = "Malham Tarn polygon_region")
mal <- spTransform(mal, CRS("+init=epsg:32629"))
maldf<- fortify(mal)

malpoints <- rgdal::readOGR("../dat-orig/wiiketal/2019-macrophytes-samplepoints.gpx",
                            layer = "waypoints")
malpoints <- spTransform(malpoints, CRS("+init=epsg:32629"))

## tidy up plants and make appropriate date column
plants$species <- tolower(plants$species)
plants$species[which(plants$species == 'potluc')] <- 'pluc'

plants$date <- as.POSIXct(plants$date, tz='UK', format='%d.%m.%Y')

## isolate points that need coordinates computed then remove archaic column from plants,
##    prepare df for bathy
news <- plants[-which(plants$sdval==''),]
plants <- plants[,-which(names(plants)=='sdval')]
bathprep <- unique(plants[,c('point','depth')])

## deal with SD coordinates in news 
## ===========================================================================================
##    3 needs to go before easting, and 4 before northing
news$Easting <- paste0(3,substr(news$sdval, 3, 7))
news$Northing <- paste0(4,substr(news$sdval, 8, 12))
news$Northing[news$point==209] <- '466581' # we lost one number from this one; this is best guess

### Create coordinates variable
coords <- cbind(Easting = as.numeric(news$Easting),
                Northing = as.numeric(news$Northing))

### Create the SpatialPointsDataFrame
coords <- SpatialPointsDataFrame(coords,
                                data = news,
                                proj4string = sp::CRS("+init=epsg:27700")) # bng

coords <- spTransform(coords, sp::CRS("+init=epsg:32629"))

## ==================================================================================================
## make sample points and perimeter into data frame
ptdf <- data.frame(long = c(coordinates(malpoints)[,1], coordinates(coords)[,1]),
                   lat = c(coordinates(malpoints)[,2],coordinates(coords)[,2]),
                   point = c(as.numeric(as.character(malpoints@data$name)),coords@data$point))


## one version for bathy, one version for plants
bath <- ptdf[-which(ptdf$point>201),]
bath[which(ptdf$point==138),c('long','lat')] <- NA
bath <- merge(bath, unique(plants[,c('point','depth')]))

bath <- bath[complete.cases(bath),]

plants <- merge(plants, ptdf) # plants now have coordinates

## =================================================================================================
## create perimeter depth of 0 for bathymetry
perip <- as.data.frame(rPointOnPerimeter(1000,mal))
names(perip) <- c('long','lat')
perip$depth <- 0
perip$point <- seq.int(from=max(bath$point, na.rm = T), length.out=nrow(perip))

## create separate data frame for bathymetry purposes (recall 138 replaced by new point)
bath <- rbind.fill(bath,perip) # dealsl with different column orders
plants <- merge(plants, ptdf, all=T)

## interpolate depth for geom_contour which likes regular intervals
fld <- with(bath, interp(x = long, y = lat, z = depth))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "z")

df$long <- fld$x[df$x]
df$lat <- fld$y[df$y]

ggplot(data = df, aes(x = long, y = lat)) +
  geom_path(data=maldf, col='black', aes(group=group)) +
  stat_contour(aes(z = z))

## save the tidied data frames
saveRDS(plants, "../dat-mod/mal-plantsurvey-2019-plants.rds")
saveRDS(df, "../dat-mod/mal-plantsurvey-2019-bathy.rds")
saveRDS(maldf, "../dat-mod/mal-plantsurvey-2019-perimeter.rds")


