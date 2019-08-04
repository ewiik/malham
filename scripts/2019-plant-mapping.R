##  figures for livelihood paper

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

require(sf)
require(rgdal)
loadfonts()
require(gsl) 
## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

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

## read in data
#mal <- st_read("shapes/Malham Tarn polygon_region.shp")
plants <- read.csv('shapes/mal-plants.csv', stringsAsFactors = F)
plants$species <- tolower(plants$species)
plants$species[which(plants$species == 'potluc')] <- 'pluc'
plants$date <- as.POSIXct(plants$date, tz='UK', format='%d.%m.%Y')

mal <- readOGR(dsn = "shapes", layer = "Malham Tarn polygon_region")
malwgs <- spTransform(mal, CRS("+proj=longlat +datum=WGS84"))

malpoints <- rgdal::readOGR("test.gpx",
                            layer = "waypoints")
malpoints <- as.data.frame(coordinates(malpoints))
malpoints$point <- rownames(malpoints)
  
set.seed(200)
malpn <- spsample(malwgs, 200, type = 'regular', nsig=8)
malpndf <- as.data.frame(malpn)
malpndf$point <- c(1:nrow(malpndf))
malpndf <- base::merge(malpndf, plants, all.x =T)

testdf<- fortify(malwgs)

perip <- as.data.frame(rPointOnPerimeter(1000,malwgs))
names(perip) <- c('x1','x2')
perip$depth <- 0
perip$id <- seq.int(from=max(malpndf$point, na.rm = T), length.out=nrow(perip))

malpndf <- rbind.fill(malpndf, perip)
malpndf <- merge(malpndf, malpoints, all.x=T)
### 
ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='pluc',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Potamogeton lucens')
  
ggplot(testdf) +
    papertheme +
    aes(long,lat,group=group) + 
    #geom_polygon(data=testdf, aes(long,lat,group=group)) +
    geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='nflex',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  coord_equal() + ggtitle('Nitella flexilis')
  
ggplot(testdf) +
    papertheme +
    aes(long,lat,group=group) + 
    #geom_polygon(data=testdf, aes(long,lat,group=group)) +
    geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='chara1',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Chara 1')
  
  ggplot(testdf) +
    papertheme +
    aes(long,lat,group=group) + 
    #geom_polygon(data=testdf, aes(long,lat,group=group)) +
    geom_path(color="black") +
    geom_point(data = malpndf[malpndf$species=='caspera',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
    geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
    ggtitle('Chara aspera')
  
  
ggplot(testdf) +
    papertheme +
    aes(long,lat,group=group) + 
    #geom_polygon(data=testdf, aes(long,lat,group=group)) +
    geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='ecan',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Elodea canadensis')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='cglob',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Chara globularis')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='uvul',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Utricularia vulgaris')

malsum <- ddply(malpndf, .(point), summarise, plantcover=sum(plantcover, na.rm=T), substrate = substrate[1],
                coords.x1=coords.x1[1], coords.x2=coords.x2[1], depth=depth[1], height=mean(plantheight))
subcodes <- data.frame(substrate = unique(malsum$substrate))
subcodes$GrainSize <- c(9,8,4,7,2,10,NA,5,3,1,6,10)
malsum <- merge(malsum, subcodes)

ggplot(testdf) +
    papertheme +
    aes(long,lat,group=group) + 
    #geom_polygon(data=testdf, aes(long,lat,group=group)) +
    geom_path(color="black") +
    geom_point(data = malsum, inherit.aes = F, aes(x=coords.x1, y=coords.x2, col=GrainSize, size=plantcover)) +
  scale_color_viridis() +
    geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth))
  coord_equal()

  ggplot(testdf) +
    papertheme +
    aes(long,lat,group=group) + 
    #geom_polygon(data=testdf, aes(long,lat,group=group)) +
    geom_path(color="black") +
    geom_point(data = malsum, inherit.aes = F, aes(x=coords.x1, y=coords.x2, col=height<20, size=GrainSize)) +
    #scale_color_viridis() +
    geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth))
  coord_equal()
  
ggplot(malsum[grep('silt', malsum$substrate),],aes(x=depth, y=plantcover)) +
    papertheme +
    geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',formula = y ~ s(x, bs = "cs")) +
  ylab('Plant cover (%)') + xlab('Depth (cm)')
  
  
max(plants$depth[grep('chara', plants$species)], na.rm = T)  
