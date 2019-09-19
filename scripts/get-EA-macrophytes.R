## EA macrophyte surveys

## packages
library(rgdal)
library(sp)
library(sf)

library(Hmisc)
library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')

mdb <- mdb.get("../dat-orig/EA/sca_database_v2.2_2007_2019_Malham_data.mdb")
fv <- read.csv("../dat-orig/EA/2004-2004surveys-longformat.csv")
fvcoords <- read.csv("../dat-orig/EA/2004-2005-coordinates.csv")

## ====================================================================================
## database format data
## =================================================================================
head(mdb$LHS_BoatSurvey) # depht, fil alg and biomass; ID, sectionID
head(mdb$LHS_BoatSurveyData) # species in sample point; ID, BoatSurveyID
#head(mdb$LHS_Survey) # dates and IDs of surveys with who dunnit; ID does not map to above; ID should be
#   SurveyID! that maps to next.
#head(mdb$LHS_Section) # ID, SurveyID, GPS coordinates of sections

## start by sorting out mismatch in ID columns; ID surveys by date
dates <- mdb$LHS_Survey
dates$SurveyDate <- as.POSIXct(as.character(dates$SurveyDate), format='%m/%d/%y %H:%M:%S')
names(dates)[1] <- 'SurveyID'

sections <- mdb$LHS_Section
sections <- merge(sections, dates[,c('SurveyID','SurveyDate')], sort = F)
names(sections)[2] <- 'SectionID'

## boat surveys
## =================================================================================================
boats <-mdb$LHS_BoatSurvey # yay labeled integers
boats[,1:2] <- lapply(boats[,1:2], function(x) { attributes(x) <- NULL; x })

boats$ID %in% sections$SectionID # what is id in boats? fucks sake it's actually boatsurveyid in...
names(boats)[1] <- 'BoatSurveyID'

boatsp <- mdb$LHS_BoatSurveyData
boatsp[,1:2] <- lapply(boatsp[,1:2], function(x) { attributes(x) <- NULL; x }) # wgat is id in boatsp?

## melt bost surveys
strings <- c('Substrate', 'AquaticBiomass','FilAlg','Depth')
idvars = c('BoatSurveyID', 'SectionID')

makelong <- function(string, df, idvars) {
  repl <- paste('.', string,sep='')
  submelt <- melt(df, measure.vars = grep(string, names(df)), 
                  id.vars = idvars, value.name = string)
  submelt$point <- gsub(repl, "", submelt$variable)
  submelt <- submelt[,-which(names(submelt)=='variable')]
  return(submelt)
    }

bmelt <- lapply(strings, makelong, idvars=idvars, df=boats)
test <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all.x = TRUE),
       bmelt)

bmeltsp <- melt(boatsp, measure.vars = grep('X', names(boatsp)), variable.name = 'point',
                value.name = 'abundance')

alldat <- merge(test, bmeltsp, sort=F)


## shoreline surveys
## =====================================================================================================
shores <-mdb$LHS_WaderSurvey # yay labeled integers
shores[,1:2] <- lapply(shores[,1:2], function(x) { attributes(x) <- NULL; x })

shores$ID %in% sections$SectionID # what is id in shores? fucks sake it's actually shoresurveyid in...
names(shores)[1] <- 'WaderSurveyID'

shoresp <- mdb$LHS_WaderSurveyData
shoresp[,1:2] <- lapply(shoresp[,1:2], function(x) { attributes(x) <- NULL; x }) # wgat is id in shoresp?

strings <- c('Substrate', 'AquaticBiomass','FilAlg')
idvars = c('WaderSurveyID', 'SectionID')

smelt <- lapply(strings, makelong, idvars=idvars, df=shores)
smelt <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all.x = TRUE),
               smelt)

smeltsp <- melt(shoresp, measure.vars = grep('X', names(shoresp)), variable.name = 'point',
                value.name = 'abundance')

alldats <- merge(smelt, smeltsp, sort=F)

## sort out coordinates
## ========================================================================================================
## these coordinates are in bng with letters, which needs changing
##    4 needs to go before easting, and 3 before northing
where <- reshape2::melt(sections, id.vars = c('SurveyID', 'SectionID', 'SectionNumber','SurveyDate'),
              measure.vars=c('BoatShoreGPS',  'BoatLakeGPS'))
where <- rbind(where, reshape2::melt(sections, id.vars = c('SurveyID', 'SectionID', 'SectionNumber','SurveyDate'),
                        measure.vars=c('ShoreStartGPS',  'ShoreEndGPS')))
where$Section <- 'Shore'
where$Section[grep('Boat', where$variable)] <- 'Boat'

where$value[where$SurveyID==222 & where$SectionNumber ==3] <-
  c('SD8954466408','SD8942266532','SD8957666417','SD8954066368')
where$value[where$SurveyID==222 & where$SectionNumber ==4] <-
         c('SD8977766985','SD8952466729','SD8979966956','SD8976467018')

where$Easting[where$value!=''] <- paste0(3,substr(where$value[where$value!=''], 3, 7))
where$Northing[where$value!=''] <- paste0(4,substr(where$value[where$value!=''], 8, 12))

### Create coordinates variable and correct 2009
coords <- cbind(Easting = as.numeric(where$Easting),
                Northing = as.numeric(where$Northing))
coords <- coords[complete.cases(coords),]

### Create the SpatialPointsDataFrame
where <- SpatialPointsDataFrame(coords,
                                data = where[complete.cases(where),],
                                proj4string = CRS("+init=epsg:27700")) # code for bng

### Convert
where <- spTransform(where, CRS("+init=epsg:32629")) #utm suitable for uk

## create Lat, Long
where@data$Long <- coordinates(where)[, 1]
where@data$Lat <- coordinates(where)[, 2]

## check where things map to:
dat <- data.frame(where)
dat$SectionNumber <- factor(dat$SectionNumber)

ggplot(dat, aes(x=Long, y=Lat, color=SectionNumber, shape=Section)) +
  papertheme+
  geom_point(size=2) +
facet_wrap(~SurveyDate)

## generate 20 coordinates for X1:X20 for each boat and shore survey
sdat <- with(dat[dat$Section=='Boat',], split(dat[dat$Section=='Boat',], list(SectionNumber,SurveyID)))

sdats <- with(dat[dat$Section=='Shore',], split(dat[dat$Section=='Shore',], list(SectionNumber,SurveyID)))

getPts <- function(df,lendf) {
  tran <- df$SectionNumber[1]
  whichlen <- lendf
  
  tempdf <- df[,c('Long','Lat')] %>% group_by(group = Long %in% range(Long)) %>% 
      # summarize lat and lon for each group into a list of a sequence from the first to the second
      summarise_each(funs(list(seq(.[1], .[2], length.out = whichlen)))) %>% 
      # expand list columns with tidyr::unnest
      unnest()
    tempdf <- as.data.frame(tempdf)
    tempdf$SectionNumber <- tran
    tempdf$SectionID <- df$SectionID[1]
    tempdf$point <- seq(1, whichlen,  1)
    tempdf$date <- df$SurveyDate[1]
    tempdf <- tempdf[,-which(names(tempdf)=='group')]
 
  return(tempdf)
}

bcoords <- lapply(sdat, getPts,lendf=20)
bcoords <- do.call(rbind, bcoords)
bcoords$location <- 'boat'
bcoords$point <- paste('X', bcoords$point, sep='')

sdats <- sdats[sapply(sdats, function(x) dim(x)[1]) > 0] # recall one shore transect missed

scoords <- lapply(sdats, getPts,lendf=5)
scoords <- do.call(rbind, scoords)
scoords$location <- 'shore'

names(scoords)[which(names(scoords)=='point')] <- 'basesection'

tomerge <- expand.grid(1:5, letters[1:4],KEEP.OUT.ATTRS=F, stringsAsFactors = F)
tomerge$Var2 <- paste('X',tomerge$Var1, tomerge$Var2, sep='')
names(tomerge) <- c('basesection','point')

scoords <- merge(scoords,tomerge)
scoords$location <- 'shore'
scoords <- scoords[,-which(names(scoords) =='basesection')]

alldat <- merge(alldat, bcoords)
alldats <- merge(alldats, scoords)
alldats$Depth <- 25
alldats$Depth[grep('b', alldats$Depth)] <- 50
alldats$Depth[grep('c', alldats$Depth)] <- 75
alldats$Depth[grep('d', alldats$Depth)] <- 80

alldat <- rbind.fill(alldat, alldats)

## remove cases where abundance is zero for a particular species but there is some biomass
alldat <- alldat[-which(alldat$abundance==0 & alldat$AquaticBiomass>0),]
alldat$PlantSpecies <- as.character(alldat$PlantSpecies)
alldat$PlantSpecies[which(alldat$AquaticBiomass==0 & alldat$abundance==0)] <- 'No plants'
alldat <- alldat[,-which(names(alldat)=='ID')] # each row had its own ID...
# now can remove duplicated no plants entires
alldat <- alldat[-which(duplicated(alldat)),]


#W ==============================================================================================
## individual csv data
##============================================================================================
## number of open water points in all of these are 10
fvcoords$SurveyDate <- as.POSIXct(as.character(fvcoords$date), format='%d/%m/%y', tz='UK')

fv$SurveyDate <- as.POSIXct(as.character(fv$date), format='%d.%m.%Y', tz='UK')

fvcoords$Easting <- paste0(3,substr(fvcoords$bng[fvcoords$bng!=''], 3, 7))
fvcoords$Northing <- paste0(4,substr(fvcoords$bng[fvcoords$bng!=''], 8, 12))

### Create coordinates variable
coords <- cbind(Easting = as.numeric(fvcoords$Easting),
                Northing = as.numeric(fvcoords$Northing))

### Create the SpatialPointsDataFrame
fvcoords <- SpatialPointsDataFrame(coords,
                                data = fvcoords[complete.cases(fvcoords),],
                                proj4string = CRS("+init=epsg:27700")) # code for bng

### Convert
fvcoords <- spTransform(fvcoords, CRS("+init=epsg:32629")) #utm suitable for uk

## create Lat, Long
fvcoords@data$Long <- coordinates(fvcoords)[, 1]
fvcoords@data$Lat <- coordinates(fvcoords)[, 2]

##  make things compatible
fv$date <- fv$SurveyDate
fvcoords$date <- fvcoords$SurveyDate

## check where things map to:
dat <- data.frame(fvcoords)

## generate 10 coordinates for transects 1:10 for each boat and shore survey
sdat <- with(dat[dat$location=='boat',], split(dat[dat$location=='boat',], list(transect,date)))

sdats <- with(dat[dat$location=='shore',], split(dat[dat$location=='shore',], list(transect,date)))

getPts <- function(df,lendf) {
  transect <- df$transect[1]
  whichlen <- lendf
  
  tempdf <- df[,c('Long','Lat')] %>% group_by(group = Long %in% range(Long)) %>% 
    # summarize lat and lon for each group into a list of a sequence from the first to the second
    summarise_each(funs(list(seq(.[1], .[2], length.out = whichlen)))) %>% 
    # expand list columns with tidyr::unnest
    unnest()
  tempdf <- as.data.frame(tempdf)
  tempdf$transect <- transect
  #tempdf$SectionID <- df$SectionID[1]
  tempdf$point <- seq(1, whichlen,  1)
  tempdf$date <- df$date[1]
  tempdf <- tempdf[,-which(names(tempdf)=='group')]
  
  return(tempdf)
}

bcoords <- lapply(sdat, getPts,lendf=10)
bcoords <- do.call(rbind, bcoords)
bcoords$location <- 'boat'
rownames(bcoords) <- NULL

scoords <- lapply(sdats, getPts,lendf=5)
scoords <- do.call(rbind, scoords)
scoords$location <- 'shore'

names(scoords)[which(names(scoords)=='point')] <- 'basesection'

tomerge <- expand.grid(1:5, letters[1:4],KEEP.OUT.ATTRS=F, stringsAsFactors = F)
tomerge$Var2 <- paste(tomerge$Var1, tomerge$Var2, sep='')
names(tomerge) <- c('basesection', 'point')

scoords <- merge(scoords,tomerge)
scoords <- scoords[,-which(names(scoords) =='basesection')]

fv <- fv[,-which(names(fv)=='SurveyDate')]
fvcoords <- fvcoords[,-which(names(fvcoords)=='SurveyDate')]
names(fv)[names(fv)=='section'] <- 'point'

alldatb <- merge(fv[fv$location=='boat',], bcoords)
alldatsb <- merge(fv[fv$location=='shore',], scoords)

alldatsb$depth <- as.character(alldatsb$depth)
alldatsb$depth[grep('>', alldatsb$depth)] <- 80
alldatb$depth <- as.character(alldatb$depth)

alldatb <- rbind.fill(alldatb, alldatsb)
alldatb$speciestype <- 'binary'
alldat$speciestype <- 'numeric'

names(alldat) <- tolower(names(alldat))
names(alldatb) <- tolower(names(alldatb))

names(alldat)[!names(alldat) %in% names(alldatb)] <- c("sectionid","boatsurveyid",'biomass','species',
                                                        'transect','wadersurveyid')

## put them together
alldat <- rbind.fill(alldatb, alldat)

alldat$species[which(alldat$species=='')] <- 'No plants'

## create aggregate codes for plants to identify key information
oaggs <- data.frame(species=unique(alldat$species))
oaggs$Plant <- c('Chara','Chara','Elodea','Moss','Nitella','PotEut','PotEut','Chara','No plants',
                 'PotLuc','PotEut','Tolypella','Chara','NA','Carex','Menyanthes','Callitriche',
                 'Nitella','Chara','Fontinalis','PotLuc','Elodea','Nitella','Chara','Utricularia',
                 'Chara','Callitriche','Chara','PotEut','Callitriche','Carex','Equisetum','Menyanthes',
                 'Nitella','Hippuris','Moss')
oaggs$fullname <- oaggs$species                 
more <- as.numeric(rownames(oaggs)[apply(oaggs,2,nchar)[,'fullname']<10 & !is.na(oaggs$fullname)]  ) 
oaggs$fullname <- as.character(oaggs$fullname)
oaggs$fullname[more] <- c('Chara aspera','Chara globularis','Elodea canadensis','Fontinalis antipyretica',
                          'Nitella flexilis agg.','Potamogeton pectinatus','Potamogeton berchtoldii',
                          'No plants','Potamogeton lucens',
                          'Potamogeton crispus','Tolypella glomerata','Chara virgata','Carex rostrata',
                          'Menyanthes trifoliata','Callitriche hamulata cf')

alldat <- merge(alldat, oaggs, sort=F )
alldat$date <- format(alldat$date, '%Y-%m-%d')

allcoords <- fvcoords@data
somecoords <- where@data
somecoords <- subset(somecoords, select = c('SurveyDate','Section','SectionNumber','Long','Lat','variable'))
names(somecoords) <- c('date','location','transect','Long','Lat','end')
somecoords$location <- tolower(somecoords$location)
somecoords$end <- 'start'
somecoords$end <- ifelse(length(grep('Shore', somecoords$end))==0, 'end','start')

allcoords <- rbind.fill(allcoords[,c('date','location','end','transect','Long','Lat')],somecoords)
allcoords$date <- format(allcoords$date, '%Y-%m-%d')

## some plotting
want <- 'Chara' # Chara Potamogeton Hippuris Fontinalis Nitella No plants Utricularia

ggplot(alldat[grep(want,alldat$Plant),], aes(x=long,y=lat)) +
  papertheme +
  #geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(size=factor(biomass), fill=fullname), shape=21, color='black',alpha=0.7) +
  facet_wrap(~factor(date) ) +
  geom_line(data=allcoords, aes(x=Long, y=Lat,group=interaction(date, transect, location))) 

sumdat <- ddply(alldat, .(date, transect, location, point), summarise, biomass=max(biomass, na.rm = T),
                long=long[1], lat=lat[1])  
sumdat$biomass[sumdat$biomass==-Inf] <- NA

ggplot(sumdat, aes(x=long,y=lat, colour=factor(biomass))) +
  papertheme +
  #geom_path(data=peri, aes(group=group),color="black") +
  geom_point(alpha=0.7, size=3) +
  facet_wrap(~factor(date) ) +
  scale_colour_manual(values=c('#a6611a','#dfc27d','#80cdc1','#018571')) +
  geom_line(inherit.aes=F, data=allcoords, aes(x=Long, y=Lat,group=interaction(date, transect, location))) 

## ==============================================================================================
## save data
saveRDS(alldat, '../dat-mod/mal-plantsurveys-ea-db-xlsx.rds')
saveRDS(allcoords, "../dat-mod/mal-plantsurveys-ea-coords.rds")
