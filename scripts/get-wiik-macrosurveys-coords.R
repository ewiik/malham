## create approximate coordinates for wiik phd macrophyte surveys

## packages
library(rgdal)
library(sp)
library(sf)

library(ggplot2)

library(plyr)
library(dplyr)
library(tidyr)

## read in surveys and coordinates
where <- read.csv("../dat-orig/wiiketal/mal-plantsurveys-wiikphd-coordinates.csv")
mac <- read.csv("../dat-orig/wiiketal/mal-plantsurveys-wiikphd.csv")

## these coordinates are in bng with letters, which needs changing
##    4 needs to go before easting, and 3 before northing
where$Easting <- paste0(3,substr(where$bng, 3, 7))
where$Northing <- paste0(4,substr(where$bng, 8, 12))

### Create coordinates variable
coords <- cbind(Easting = as.numeric(where$Easting),
                Northing = as.numeric(where$Northing))

### Create the SpatialPointsDataFrame
where <- SpatialPointsDataFrame(coords,
                                 data = where,
                                 proj4string = CRS("+init=epsg:27700")) # code for bng

### Convert
where <- spTransform(where, CRS("+init=epsg:32629")) #utm suitable for uk

## create Lat, Long
where@data$Long <- coordinates(where)[, 1]
where@data$Lat <- coordinates(where)[, 2]

## check where things map to:
dat <- data.frame(where)

ggplot(dat, aes(x=Long, y=Lat, color=factor(transect), shape=location)) +
  geom_point() #+
  #geom_label(aes(label=transect))

## are the number of points always the same?
summ <- ddply(mac,.(transect,location,date), summarise, pts=length(unique(section)))
ggplot(summ[summ$location=='boat',], aes(x=transect, col=date, y=pts)) +
  geom_point()

## nope. need to approximate equal distance between points along transect as best-guess
## instead pf sbDists function can use tidyr sneakily and create lists with start end
##    and pts
bsum <- summ[summ$location=='boat',]
bsum <- split(bsum, bsum$transect)

bdat <- split(dat[dat$location=='boat',], dat[dat$location=='boat','transect'], drop = T)

sdat <- split(dat[dat$location=='shore',],dat[dat$location=='shore','transect'], drop = T)
spts <- data.frame(pts = 5, date=unique(mac$date))

## function to get the sample points
getPts <- function(df,lendf) {
  tran <- df$transect[1]
  whichlen <- lendf$pts
  dflist <- list()
  for (i in 1:length(whichlen)) {
    tempdf <- df[,c('Long','Lat')] %>% group_by(group = Long %in% range(Long)) %>% 
      # summarize lat and lon for each group into a list of a sequence from the first to the second
      summarise_each(funs(list(seq(.[1], .[2], length.out = whichlen[i])))) %>% 
      # expand list columns with tidyr::unnest
      unnest()
    tempdf <- as.data.frame(tempdf)
    tempdf$transect <- tran
    tempdf$pts <- whichlen[i]
    tempdf$section <- seq(1, whichlen[i],  1)
    tempdf$date <- lendf$date[i]
    dflist[[i]] <- tempdf[,-which(names(tempdf)=='group')]}
  dflist <- do.call(rbind, dflist)
  return(dflist)
}

bcoords <- Map(getPts,bdat, bsum)
bcoords <- do.call(rbind, bcoords)
bcoords$location <- 'boat'

scoords <- lapply(sdat,getPts, lendf= spts)
scoords <- do.call(rbind, scoords)
names(scoords)[which(names(scoords)=='section')] <- 'basesection'

tomerge <- expand.grid(1:5, letters[1:4],KEEP.OUT.ATTRS=F, stringsAsFactors = F)
tomerge$Var2 <- paste(tomerge$Var1, tomerge$Var2, sep='')
names(tomerge) <- c('basesection','section')
scoords <- merge(scoords,tomerge)
scoords$location <- 'shore'
scoords <- scoords[,-which(names(scoords) =='basesection')]

allcoords <- rbind(scoords, bcoords)
names(allcoords)[1:2] <- tolower(names(allcoords)[1:2]) # to align with 2019 names

## merge with original macrophyte survey
mac <- merge(mac, allcoords)

## make dates into dates and create year 
mac$date <- as.POSIXct(mac$date, tz='GMT',format='%d.%m.%Y')
mac$year <- format(mac$date, "%Y")

## save
saveRDS(mac, '../dat-mod/mal-plantsurveys-wiikphd.rds')
saveRDS(dat, '../dat-mod/mal-plantsurveys-wiikphd-transects.rds')
