## script that combines all plant data
library(reshape2)
library(plyr)

## read in data
peri <- readRDS("../dat-mod/mal-plantsurvey-2019-perimeter.rds")
bath <- readRDS("../dat-mod/mal-plantsurvey-2019-bathy.rds")

plants <- readRDS("../dat-mod/mal-plantsurvey-2019-plants.rds")
oplants <- readRDS("../dat-mod/mal-plantsurveys-wiikphd.rds")
trans <- readRDS('../dat-mod/mal-plantsurveys-wiikphd-transects.rds')
names(trans) <- tolower(names(trans))

ea <- readRDS("../dat-mod/mal-plantsurveys-ea-db-xlsx.rds")
eatrans <- readRDS("../dat-mod/mal-plantsurveys-ea-coords.rds")

## remove june 2009 ea survey as it's in wiik phd surveys with cleaning done
ea <- ea[-which(ea$date=='2009-06-14'),]
eatrans <- eatrans[-which(eatrans$date=='2009-06-14'),]

## all transects in one
alltrans <- data.frame(date=rep(unique(oplants$date), each=nrow(trans))) # trans does not have date
alltrans <- cbind(alltrans, trans)
names(alltrans)[names(alltrans)=='position'] <- 'end'
names(alltrans) <- tolower(names(alltrans))
names(eatrans) <-tolower(names(eatrans))
alltrans<- subset(alltrans, select = c('date', 'location','end','transect','lat','long'))
alltrans <- rbind.fill(alltrans,eatrans)
alltrans$year <- format(alltrans$date, '%Y')

### rename '' as no plants and separate extra points from rest - for now
## FIXME 0 still a few replaced by new point places
extras <- plants[which(plants$point>201),]
plants <- plants[-which(plants$point>201 | is.na(plants$species)),]

plants$species[plants$species=='' & plants$plantcover==0 ] <- 'No plants'
plants$species[plants$species=='' & plants$plantcover==39 ] <- 'pluc' # one missing entry
## FIXME: correct missing entry in raw data
plants <- droplevels(plants)

## sort out extra space in oplants and make substrate cases comparable
unique(oplants$substrate)
oplants$substrate <- toupper(oplants$substrate)
oplants$substrate <- gsub(' ', '', oplants$substrate)
oplants$substrate[oplants$substrate==''] <- NA

## consistent names
names(oplants)[names(oplants)=='section'] <- 'point'

ea$substrate[ea$substrate==''] <- NA
ea$substrate[ea$substrate=='na'] <- NA

## change x prefixed things away
ea$point <- gsub('X', '', ea$point)

## make biomass 0 in cases where fil alg were given biomass score... and create no plants
oplants$biomass[grep('fil alg', oplants$notes)] <- oplants$abundance[grep('fil alg', oplants$notes)]
oplants$species <- as.character(oplants$species)
oplants$species[oplants$species=='' & is.na(oplants$abundance) ] <- 'No plants'

## create aggregate codes for plants to identify key information
oaggs <- data.frame(species=unique(oplants$species))
oaggs$Plant <- c('Chara','Chara','Nitella','Chara','Moss','No plants','Moss',#the rows with no species
                 'Carex','Chara','PotEut','Elodea','Chara','Chara','PotEut','Callitriche','Chara',
                 'Zannichellia','PotLuc','Balls','Epiphyte','FilAlg','Characeae','Nitella',
                 'Menyanthes','Equisetum','Chara','Chara','Epiphyte')
oaggs$fullname <- c('Chara aspera','Chara globularis','Nitella flexilis agg.','Chara contraria',
                    'moss','No plants','Fontinalis antipyretica','Carex rostrata','Chara aspera-contraria agg.',
                    'Potamogeton crispus','Elodea canadensis','Chara globularis-virgata agg.',
                    'Chara virgata','Potamogeton pusillus agg.','Callitriche cf hamulata',
                    'Chara contraria-virgata agg.','Zannichellia palustris','Potamogeton lucens',
                    'Aegagrophila linnaei','Palmella sp.','Bristly alg','Characeae','Nitella opaca',
                    'Menyanthes trifoliata','Equisetum fluviatile','Chara vulgaris-virgata agg.',
                    'Chara vulgaris var papillata','Epiphytes')

aggs <- data.frame(species=unique(plants$species))
aggs$Plant <- c('No plants','Elodea','Chara','Chara','FilAlg','Moss','Nitella','Chara',
                'Utricularia','Balls','Chara','PotEut','Chara','PotLuc','Moss','Fontinalis',
                'Chara')
aggs$fullname <- c('No plants','Elodea canadensis','Chara1','Chara2','FilAlg','Moss','Nitella flexilis agg.',
                   'Chara globularis','Utricularia vulgaris agg.','Aegagrophila linnaei',
                   'Chara4','Potamogeton crispus','Chara aspera','Potamogeton lucens','Moss',
                   'Fontinalis antipyretica','Chara3')

eaggs <- data.frame(species=unique(ea$species))
eaggs$Plant <- c('Chara','Chara','Elodea','Moss','Nitella','PotEut','PotEut','Chara',
                 'No plants','PotLuc','PotEut','Tolypella','Chara',NA,'Carex','Menyanthes',
                 'Callitriche','Nitella','Chara','Moss','PotLuc','Elodea','Nitella','Chara',
                 'Utricularia','Chara','Callitriche','Chara','Callitriche','Carex', 'Equisetum',
                 'Menyanthes','Nitella','Hippuris','Moss') 
eaggs$fullname <- c('Chara aspera','Chara globularis','Elodea canadensis','Fontinalis antipyretica',
                    'Nitella flexilis agg.','Potamogeton pectinatus','Potamogeton berchtoldii',
                    'Chara vulgaris var papillata','No plants','Potamogeton lucens','Potamogeton crispus',
                    'Tolypella glomerata','Chara virgata',NA,'Carex rostrata','Menyanthes trifoliata',
                    'Callitriche hamulata',as.character(eaggs[18:35,'species']))

## merge agg names with plant survey data from all years
plants <- merge(plants, aggs)
oplants <- merge(oplants, oaggs)
ea <- merge(ea, eaggs)

## change abundance here to 1 as a token guess
oplants$abundance[oplants$transect==1 & oplants$section=='4d' & 
                    oplants$depth==75 & oplants$biomass==0] <- 1

plants$plantheight[is.na(plants$plantheight)] <- 0 # these are fil alg and one 0 biomass site

## put all macro data together
oplants$speciestype <- 'numeric'
plants$speciestype <- 'numeric'
#ea$speciestype already created before

oplants$survey <- 'csm'
plants$survey <- '200pt'
ea$survey <-'csm'

allplants <- rbind.fill(oplants, plants)
allplants <- rbind.fill(allplants, ea)
allplants$year <- format(allplants$date, '%Y')

allplants$abundance[allplants$species=='No plants'] <- 0

## take away csm with na rows
allplants <- allplants[-which(is.na(allplants$species)),]

## remove surveys with awful transects
alltrans$datefac <- as.factor(alltrans$date)

alltrans$poor <- 'no'
alltrans$poor[alltrans$year %in% c('2011','2014')] <- 'yes'

## due to bad weather some transects missing
alltrans$missing <- 'no'
alltrans$missing[alltrans$datefac == '2009-08-11' & alltrans$transect==3 & alltrans$location=='shore'] <- 'yes'
alltrans$missing[alltrans$datefac == '2009-08-11' & alltrans$transect==4 ] <- 'yes'

alltrans$missing[alltrans$datefac == '2010-08-12' & alltrans$transect==2 & alltrans$location== 'shore'] <- 'yes'
alltrans$missing[alltrans$datefac == '2010-08-12' & alltrans$transect==4 & alltrans$location== 'shore'] <- 'yes'

alltrans$missing[alltrans$datefac == '2018-08-21' & alltrans$transect==4 & alltrans$location== 'shore'] <- 'yes'

allplants$datefac <- as.factor(allplants$date)
allplants$poor <- 'no'
allplants$poor[allplants$year %in% c('2011','2014')] <- 'yes'

ggplot(alltrans, aes(x=long, y=lat, colour=missing)) +
  geom_line(aes(group=interaction(factor(transect), location,factor(date)))) +
  geom_path(data=peri, aes(group=group),color="black", linetype='dotted') +
  facet_wrap(~datefac) # 2011, 2014 unfortunately

speciesdf <- unique(allplants[,c('speciestype','date')])

alltrans <- merge(alltrans, speciesdf)


## 2009 is difficult: weather poor in August so some transects missing but June was bad with 
##    Chara getting mixed up... need to merge 2009...
## 2009 transect 4 August needs June's data; August 3 needs June (only 5 data points from the boat)
## 2010 shores 2 and 4 need June
junt <- alltrans[alltrans$datefac=='2009-06-15' | alltrans$datefac=='2010-06-16',]
augt <- alltrans[(alltrans$datefac=='2009-08-11' | alltrans$datefac=='2010-08-12') & alltrans$missing=='yes',]

junp <- allplants[allplants$datefac=='2009-06-15' | allplants$datefac=='2010-06-16',]
augp <- allplants[allplants$datefac=='2009-08-11' | allplants$datefac=='2010-08-12' & allplants$missing=='yes',]

augp <- junp[junp$transect==4 & junp$year=='2009',]
augp <- rbind(augp, junp[junp$transect==3 & junp$year=='2009',])
augp <- rbind(augp, junp[(junp$transect==2 | junp$transect == 4) & junp$year=='2010' & junp$location=='shore',])
augp$date[augp$year=='2009'] <- unique(augt$date[augt$year=='2009'])      
augp$date[augp$year=='2010'] <- unique(augt$date[augt$year=='2010'])      
augp$datefac[augp$year=='2009'] <- unique(augt$datefac[augt$year=='2009'])      
augp$datefac[augp$year=='2010'] <- unique(augt$datefac[augt$year=='2010'])      

augt <- junt[junt$transect==4 & junt$year=='2009',]
augt <- rbind(augt, junt[junt$transect==3 & junt$year=='2009' & junt$location=='shore',])
augt <- rbind(augt, junt[(junt$transect==2 | junt$transect == 4) & junt$year=='2010' & junt$location=='shore',])
augt$date[augt$year=='2009'] <- unique(augp$date[augp$year=='2009'])      
augt$date[augt$year=='2010'] <- unique(augp$date[augp$year=='2010'])      
augt$datefac[augt$year=='2009'] <- unique(augp$datefac[augp$year=='2009'])      
augt$datefac[augt$year=='2010'] <- unique(augp$datefac[augp$year=='2010'])      

alltrans[rownames(augt),] <- augt
alltrans <- alltrans[-which(alltrans$datefac == '2009-06-15'),]
alltrans <- alltrans[-which(alltrans$datefac == '2010-06-16'),]

allplants <- allplants[-which(allplants$datefac=='2009-06-15'),]
allplants <- allplants[-which(allplants$datefac=='2010-06-16'),]
allplants <- rbind(allplants, augp)

alltrans <- alltrans[-which(alltrans$datefac == '2005-06-29'),]
allplants <- allplants[-which(allplants$datefac=='2005-06-29'),]


## create unique id for csm surveys
idf <- data.frame(unique(allplants[allplants$survey=='csm',c('datefac','transect', 'location', 'point')]))
idf$id <- 1:nrow(idf)

allplants <- merge(allplants, idf, all.x = T)

allplants$depth <- as.numeric(allplants$depth)

alltrans <- droplevels(alltrans)
allplants <- droplevels(allplants)

## create a data frame that avoids repeated points (i.e. no species data)
subplants <- data.frame(unique(allplants[allplants$survey=='csm',c('datefac','transect', 'location', 'point',
                                            "date","depth" ,"substrate","biomass","filalg",
                                            'long','lat','year',"speciestype","survey",
                                            'poor','id')]))
sumplants <- ddply(allplants[allplants$survey=='200pt',], .(point), summarise, biomass=sum(plantcover, na.rm = T),
      filalg=algae[1])

sumplants <- merge(sumplants, 
                   data.frame(unique(allplants[allplants$survey=='200pt',
                                               c('datefac','point',
                                                 "date","depth" ,"substrate",
                                                 'long','lat','year',"speciestype","survey",
                                                 'poor')])))
subplants <- rbind.fill(subplants, sumplants)

## save data
saveRDS(alltrans, '../dat-mod/all-plant-transects.rds')
saveRDS(allplants, '../dat-mod/all-plant-surveys.rds')
saveRDS(subplants,'../dat-mod/all-plant-surveys-nospecies.rds')
