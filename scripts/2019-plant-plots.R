library(ggplot2)
library(reshape2)
library(viridis)
library(gridExtra)
library(extrafont)
loadfonts()
library(plyr)

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')

## FIXME: difference between biomass and abundance in phd surveys

## read in data
plants <- readRDS("../dat-mod/mal-plantsurvey-2019-plants.rds")
peri <- readRDS("../dat-mod/mal-plantsurvey-2019-perimeter.rds")
bath <- readRDS("../dat-mod/mal-plantsurvey-2019-bathy.rds")

oplants <- readRDS("../dat-mod/mal-plantsurveys-wiikphd.rds")
trans <- readRDS('../dat-mod/mal-plantsurveys-wiikphd-transects.rds')
names(trans) <- tolower(names(trans))

ea <- readRDS("../dat-mod/mal-plantsurveys-ea-db-xlsx.rds")
eatrans <- readRDS("../dat-mod/mal-plantsurveys-ea-coords.rds")

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')

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

ea$substrate[ea$substrate==''] <- NA
ea$substrate[ea$substrate=='na'] <- NA

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

oplants$survey <- 'csm'
plants$survey <- '200pt'

ea$survey <-'csm'

## consistent names
names(oplants)[names(oplants)=='section'] <- 'point'

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
## 2009 transect 4 August needs June's data; August 3 shore needs June 
## 2010 shores 2 and 4 need June
junt <- alltrans[alltrans$datefac=='2009-06-15' | alltrans$datefac=='2010-06-16',]
augt <- alltrans[(alltrans$datefac=='2009-08-11' | alltrans$datefac=='2010-08-12') & alltrans$missing=='yes',]

junp <- allplants[allplants$datefac=='2009-06-15' | allplants$datefac=='2010-06-16',]
augp <- allplants[allplants$datefac=='2009-08-11' | allplants$datefac=='2010-08-12' & allplants$missing=='yes',]

augp <- junp[junp$transect==4 & junp$year=='2009',]
augp <- rbind(augp, junp[junp$transect==3 & junp$year=='2009' & junp$location=='shore',])
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


## change x prefixed things away
allplants$point <- gsub('X', '', allplants$point)

## create unique id for csm surveys
idf <- data.frame(unique(allplants[allplants$survey=='csm',c('datefac','transect', 'location', 'point')]))
idf$id <- 1:nrow(idf)

allplants <- merge(allplants, idf, all.x = T)

allplants$depth <- as.numeric(allplants$depth)
alltrans <- droplevels(alltrans)
allplants <- droplevels(allplants)

## =============================================================================================
## plots of individual species and years
## ====================================================================================
#pplot <- 
  ggplot(allplants[allplants$year==2019,], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=plantcover), shape=21, color='black') +
  geom_contour(data=bath, aes(z=z)) + 
  facet_wrap(~species) +
  scale_fill_viridis(option='viridis')

want <- c('Balls','Callitriche','Chara','Elodea','Characeae','Fontinalis','Nitella','No plants' ,
          'PotEut','PotLuc','Tolypella','Utricularia','Zannichellia','Moss')
#opplot <- 
  ggplot(allplants[allplants$survey=='csm' & !allplants$year %in% c('2011','2004') & 
                     allplants$Plant %in% want,], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=factor(abundance)), shape=21, color='black',size=2, alpha=.7) +
  geom_contour(data=bath, aes(z=z)) + 
  geom_line(data=alltrans[alltrans$missing=='yes',],  
           aes(x=long, y=lat, group=interaction(factor(transect), location,factor(date)))) +
  facet_wrap(~year) + #labeller = function(labs) {label_value(labs, multi_line = FALSE)}
  scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f','black'))

  test <- merge(alltrans[alltrans$speciestype=='numeric' & alltrans$poor=='no',], 
                data.frame(Plant=want))
  test$Plant <- as.character(test$Plant)
  
pwant <- allplants[allplants$survey=='csm' & allplants$speciestype== 'numeric'& allplants$poor == 'no' &
                     allplants$Plant %in% want,]

pwant2 <- allplants[allplants$survey=='csm' & allplants$speciestype== 'binary'& allplants$poor == 'no',]
pwant2 <- unique(pwant2['date','transect','location','point'])

pwant3 <- allplants[allplants$survey=='csm' & allplants$poor == 'no' &
                      allplants$Plant %in% want,]
## sep and june very similar, can take sep with the same logic as aug from other years - more chance
##    of plant development and most surveys in august

ggplot(pwant, aes(x=long,y=lat)) +
    papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
    geom_point(aes(fill=factor(abundance)), shape=21, color='black',size=2, alpha=.7) +
     facet_wrap(Plant~datefac, labeller = function(labs) {label_value(labs, multi_line = FALSE)},
               ncol=8) + 
    scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f','black')) +
  scale_color_manual(values=c('transparent','black'))

ggplot(pwant2, aes(x=long,y=lat)) +
  papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=factor(biomass)), shape=21, color='black',size=2, alpha=.7) +
  facet_wrap(~datefac) + 
  scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f','black')) +
  scale_color_manual(values=c('transparent','black'))

ggplot(pwant3, aes(x=long,y=lat)) +
  papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=factor(abundance > 0)), shape=21, color='black',size=2, alpha=.7) +
  facet_wrap(Plant~datefac,labeller = function(labs) {label_value(labs, multi_line = FALSE)}) + 
  scale_fill_manual(values = c('transparent','#d7301f')) 

## =============================================================================================
## summarise aggregate groups to highest abundance possible per point; 2019 and numeric csm
## =============================================================================================
groups <- ddply(plants, .(point,Plant), summarise, totcover=sum(plantcover),
                lat=lat[1],long=long[1], date=as.factor(date[1]), depth=depth[1])
#ogroups <- ddply(oplants, .(date,transect,section,Plant), summarise, totcover=max(abundance),
#                 lat=lat[1],long=long[1], depth=depth[1])
ogroups <- ddply(allplants[allplants$speciestype=='numeric' & allplants$survey=='csm',], 
                 .(datefac,transect,location,point,Plant), summarise, totcover=max(abundance),
                 lat=lat[1],long=long[1], depth=depth[1])

groups <- rbind.fill(groups,ogroups)

## choose which to show
no <- c('Callitriche','Epiphyte','Zannichellia','Utricularia')
df <- groups[-which(groups$Plant %in% no),]

ggplot(df, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(data=df[is.na(df$transect),],aes(fill=totcover), 
             shape=21, color='black', alpha=0.7) +
  geom_point(data=df[!is.na(df$transect),],aes(col=factor(totcover)), 
             shape=2) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  geom_line(data=trans, aes(x=long, y=lat,group=interaction(transect, location))) +
  facet_wrap(~Plant, ncol=5) +
  scale_fill_distiller(palette = 'Greens', direction=1) #+
  #scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f'))

df <- ogroups
df$totcover[is.na(df$totcover)] <- 0
df <- df[-which(df$Plant %in% no),]

ggplot(df, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(size=factor(totcover), color=Plant, shape=totcover==0), alpha=0.5) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  #geom_line(data=alltrans, aes(x=long, y=lat,group=interaction(transect, location))) +
  facet_wrap(Plant~datefac,  labeller = function(labs) {label_value(labs, multi_line = FALSE)}) #+
  #scale_color_manual(values=c('#a6cee3','#33a02c','#b2df8a','#ff7f00','#fdbf6f',
                              '#1f78b4','#fb9a99','#e31a1c','#cab2d6','#6a3d9a'))

ggplot(df, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(size=factor(totcover), color=depth, shape=totcover==0), alpha=0.5) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  geom_line(data=trans, aes(x=Long, y=Lat,group=interaction(transect, location))) +
  facet_wrap(~date, ncol=2) +
  scale_color_viridis() #+
  #scale_color_manual(values=c('#a6cee3','#33a02c','#b2df8a','#ff7f00','#fdbf6f',
                            #  '#1f78b4','#fb9a99','#e31a1c','#cab2d6','#6a3d9a'))

## =============================================================================================
## higher-summary figures of plant cover 2019 and numeric abundance
## =============================================================================================
## summarise plantcover over all substrates and depths
malsum <- ddply(plants, .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], year=format(date, '%Y')[1],
                height=mean(plantheight, na.rm = T))

subcodes <- data.frame(substrate = unique(malsum$substrate))
subcodes$GrainSize <- c(9,8,4,7,2,10,5,3,1,6,10)
malsum <- merge(malsum, subcodes)

omalsum <- ddply(allplants[allplants$survey=='csm' & allplants$poor=='no',], 
                 .(datefac,transect, location, point), summarise, plantcover=biomass[1], 
                substrate = substrate[1], year=year[1],
                long=long[1], lat=lat[1], depth=depth[1])# not really a summarise operation but does the job

osubcodes <- data.frame(substrate = unique(omalsum$substrate))
osubcodes$GrainSize <- c(3,2,6,5,NA,7,4,1,-99)
omalsum <- merge(omalsum, osubcodes)

## plant cover vs grain size of sediment
ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, size=GrainSize, color=plantcover)) +
  scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

ggplot(omalsum[-which(omalsum$GrainSize<0),]) + #ignore roots
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, size=GrainSize, fill=factor(plantcover)), shape=21, col='black') +
  #scale_color_viridis() +
  scale_fill_manual(values=c('#ffffcc','#c2e699','#78c679','#238443')) +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal() +
  facet_wrap(~datefac)

## plant height vs sediment
ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, color=height > 20, size=GrainSize)) +
  #scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

## interpolate plant height for geom_contour which likes regular intervals
fld <- with(malsum[complete.cases(malsum[,c('long','lat','height')]),], 
            interp(x = long, y = lat, z = height))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "z")
df$z[df$z>100] <- 100

df$long <- fld$x[df$x]
df$lat <- fld$y[df$y]

ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_tile(data=df, aes(x=long, y=lat, fill=z)) +
  scale_fill_gradientn(colours = c("#7b3294","white","#008837"), 
                                   values = rescale(c(0,20,100)),
                                   guide = "colorbar", limits=c(0,100)) +
  #geom_point(aes(x=long, y=lat, fill=height), shape=21, color='black', size=2.5) +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

## plant cover vs depth on silt: recode and merge to put on same plot
distro <- malsum[grep('silt', malsum$substrate),c('plantcover','depth','year')]

distro <- rbind.fill(distro, omalsum[grep('SI', omalsum$substrate),c('plantcover','depth','year')])
distro$plantcover[!distro$year=='2019'] <- distro$plantcover[!distro$year=='2019'] * 30

ggplot(distro,aes(x=depth, y=plantcover/100, group=year, col=year, fill=year)) +
  papertheme +
  geom_point(alpha=0.5, shape=21, col='black') +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),formula = y ~ s(x, bs = "cs", k=4), fill='grey70') +
  ylab('Plant cover on silt (%)') + xlab('Depth (cm)') +
  scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e'))
  #scale_color_viridis()

## chara depth distribution (including nitella/chara uncertainty)
chara <- ddply(plants[plants$Plant=='Chara',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Chara',
                height=mean(plantheight), year=format(date, '%Y')[1])
chara2 <- ddply(plants[!plants$point %in% chara$point,], .(point), summarise, plantcover=0, 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Chara',
               height=0, year=format(date, '%Y')[1])

subplants <- allplants[allplants$survey=='csm' & allplants$poor=='no' &
                         allplants$speciestype=='numeric',]
subplants2 <- allplants[allplants$survey=='csm' & allplants$poor=='no',]

ochara <- ddply(subplants[subplants$Plant=='Chara',], .(id), summarise, 
                plantcover=max(abundance, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Chara', year=year[1])

ochara2 <- ddply(subplants[!subplants$id %in% ochara$id,], .(id), summarise, 
                plantcover=0, 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Chara', year=year[1])

nit<- ddply(plants[plants$Plant=='Nitella',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
               height=mean(plantheight), year=format(date, '%Y')[1])
nit2<- ddply(plants[!plants$point %in% nit$point,], .(point), summarise, plantcover=0, 
            substrate = substrate[1],
            long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
            height=0, year=format(date, '%Y')[1])

onit <- ddply(subplants[subplants$Plant=='Nitella',], .(id), summarise, 
                plantcover=max(abundance, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', year=year[1])
onit2 <- ddply(subplants[!subplants$id %in% onit$id,], .(id), summarise, 
              plantcover=0, 
              substrate = substrate[1],
              long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', year=year[1])


keys <- rbind.fill(chara, chara2,ochara,ochara2, nit,nit2, onit,onit2)
keys$plantcover[!keys$year=='2019'] <- keys$plantcover[!keys$year=='2019'] * 30

keys$substrate[keys$substrat=='SI'] <- 'silt' 
keys <- keys[grep('silt',keys$substrate),]

charadist <-  # !!! key figure for paper see also https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
  ggplot(keys,aes(x=depth, y=plantcover/100, group=year, col=year, fill=year)) +
  papertheme +
  geom_point(alpha=0.5, col='black', shape=21) +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),
              formula = y ~ s(x, bs = "cs",k=4), fullrange=F, se=F) +
  facet_wrap(~Plant) +
    scale_color_manual(values=c('#d8b365','#f6e8c3','#c7eae5','#5ab4ac')) +
    scale_fill_manual(values=c('#d8b365','#f6e8c3','#c7eae5','#5ab4ac')) +
  ylab('Charophyte cover (%)') + xlab('Depth (cm)') #+ ylim(c(0,100))

font<- ddply(plants[plants$Plant=='Moss',], .(point), summarise, plantcover=1, 
            substrate = substrate[1],
            long=long[1], lat=lat[1], depth=depth[1], Plant='Moss',
            height=mean(plantheight), year=format(date, '%Y')[1])
font2 <- ddply(plants[!plants$point %in% font$point,], .(point), summarise, plantcover=0, 
             substrate = substrate[1],
             long=long[1], lat=lat[1], depth=depth[1], Plant='Moss',
             height=0, year=format(date, '%Y')[1])

ofont <- ddply(subplants2[subplants2$Plant=='Moss',], .(id), summarise, 
              plantcover=1, 
              substrate = substrate[1],
              long=long[1], lat=lat[1], depth=depth[1], Plant='Moss', year=year[1])
ofont2 <- ddply(subplants2[!subplants2$id %in% ofont$id,], .(id), summarise, 
               plantcover=0, 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Moss', year=year[1])

fontinalis <- rbind.fill(font, font2, ofont, ofont2) 

fontdist <-  # also essential
  ggplot(fontinalis,aes(x=depth, y=plantcover, group=year, col=year)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),
              formula = y ~ s(x, bs = "cs", k=5), fullrange=F, se=F) +
  ylab('Fontinalis cover (probability)') + xlab('Depth (cm)')

elo <- ddply(plants[plants$Plant=='Elodea',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
             substrate = substrate[1],
             long=long[1], lat=lat[1], depth=depth[1], Plant='Elodea',
             height=mean(plantheight), year=format(date, '%Y')[1])
elo2 <- ddply(plants[!plants$point %in% elo$point,], .(point), summarise, plantcover=0, 
             substrate = substrate[1],
             long=long[1], lat=lat[1], depth=depth[1], Plant='Elodea',
             height=0, year=format(date, '%Y')[1])

oelo <- ddply(subplants[subplants$Plant=='Elodea',], .(id), summarise, 
               plantcover=max(abundance, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Elodea', year=year[1])
oelo2 <- ddply(subplants[!subplants$id %in% oelo$id,], .(id), summarise, 
              plantcover=0, 
              substrate = substrate[1],
              long=long[1], lat=lat[1], depth=depth[1], Plant='Elodea', year=year[1])

elodea <- rbind.fill(elo,elo2, oelo, oelo2) 
elodea$plantcover[elodea$year!='2019'] <- 
  elodea$plantcover[elodea$year!='2019'] * 30 

elodist <- 
  ggplot(elodea,aes(x=depth, y=plantcover/100, group=year, col=year)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),
              formula = y ~ s(x, bs = "cs", k=4), fullrange=F, se=F) +
  ylab('Elodea cover (%)') + xlab('Depth (cm)')

speciesplots <- grid.arrange(charadist, fontdist, elodist, layout_matrix=rbind(c(1,1),c(2,3)))
ggsave('../figs/species-depths.png', speciesplots, width=7, height=7)

## ======================================================================================
## increases in fil alg cover
## ======================================================================================
## limit to depth where fil alg systematically recorded: < 150cm
## in 2019 survey, we had whole area where we recorded 'gunk'. no filalg there so remove
##    these points. Also two points with NA at upper end of depth 138,143 so will remove those
## FIXME: how to deal with fact that point in old surveys not exactly comparable?

algo <- subplants2[subplants2$depth < 150,c('date','transect','location','depth','filalg',
                                     'long','lat','substrate', 'year', 'id')]
algo <- algo[-which(duplicated(algo$id)),]

## since different effort by sections by year probs need to mean years
#algo <- ddply(algo,.(transect,section), summarise, filalg=mean(filalg, na.rm = T))
#algo$survey <- '2009/2010'

alg <- plants[plants$depth < 150 & plants$gunk==0,
              c('date','point','depth','algae','species','plantcover',
                                      'long','lat')]
alg$filalg <- alg$algae
alg$filalg[alg$species == 'filalg'] <- 1
alg$year <- '2019'
 alg <- alg[-which(duplicated(alg$point)),]

alg <- rbind.fill(alg, algo)
subalg <- alg[-which(alg$transect==4 & !is.na(alg$transect)),]

#algae <- 
  ggplot(subalg, aes(x=year, fill=filalg, group=filalg)) +
  geom_bar() +
  papertheme 

#shallows <- 
  ggplot(peri, aes(x=long,y=lat)) +
  papertheme +
  geom_path(aes(group=group),color="black") +
  geom_contour(data=bath, aes(z=z), col='grey',size=0.1) + 
  geom_point(data=unique(plants[plants$depth<150,c('lat','long','point','algae','gunk')]),shape=21, 
             aes(fill=factor(algae), color=factor(gunk)), size=2.5) +
  geom_line(data=trans[trans$location=='shore',], aes(group=transect),
            size=2, alpha=0.7) +
  scale_fill_manual('Algal abundance', values=c('#74c476','#31a354','#006d2c')) +
  scale_color_manual('Gunk',values=c('transparent','black')) +
    coord_equal() + theme(legend.box='vertical')

algplot <- grid.arrange(algae, shallows, ncol=2)

ggsave('../figs/filalg-abundance.png',algplot, width=7, height=5)

## ==========================================================================
## overall decline of macrophyte cover
## ============================================================================
## occurrence of plants in areas where we expect them (ie limit to silt for simplicity)
## FIXME: considering old surveys have sand gravel pebbles where we can expect aspera, probs
##    best remove areas where boulders instead
silt <- malsum[grep('silt',malsum$substrate),]
silt$year <- '2019'
osilt <- omalsum[omalsum$substrate=='SI',]

silt <- rbind.fill(silt, osilt)

#siltplot <- 
  ggplot(peri, aes(x=long,y=lat)) +
  papertheme +
  geom_path(aes(group=group),color="black") +
  geom_contour(data=bath, aes(z=z), col='grey',size=0.1) + 
  geom_point(data=silt,aes(color=year)) +
  #geom_line(data=trans[trans$location=='shore',], aes(group=transect),
            #size=2, alpha=0.7) +
  # scale_color_manual('Survey',values=c('transparent','black')) +
  coord_equal() + theme(legend.box='vertical', axis.title = element_blank(),
                        axis.text = element_blank())

#occplot <- 
  ggplot(silt, aes(x=year, group=plantcover > 0, fill=plantcover > 0)) +
  papertheme +
  geom_bar() +
  scale_fill_manual('Plant cover',values=c('#a6611a','#018571')) +
ylab('Survey points on silt; including \n repeated points 2009/2010')

#depthplot <-
  ggplot(silt, aes(x=depth, group=year, col=year)) +
  geom_density() +
    papertheme + xlab('Depth (cm)') + ylab('Density distribution')

occupancy <- grid.arrange(siltplot, occplot, depthplot, layout_matrix=rbind(c(2,3),c(2,1)))
ggsave("../figs/plant-occupancy.png", occupancy, height=10, width=8)

# quick proportional test on whether significantly different
table(silt$survey, silt$plantcover>0)
prop.test(x = c(168, 96), n = c(168+21, 96+76))

## chara bed distribution across depth


## percentage occurrence of chara aspera in survey?
## ==============================================================================
asp <- allplants[which(allplants$depth < 150 ),]
asp <- asp[-which(asp$substrate=='BO'),]
asp <- asp[-which(asp$substrate=='boulder,stone'),]
asp <- asp[-which(asp$substrate=='stony,boulder'),]

asp <- ddply(asp,.(year), summarise,pasp200 = length(which(fullname=='Chara aspera'))/length(unique(point)),
             pasp=length(which(fullname=='Chara aspera'))/length(unique(id)))
asp$pasp[asp$year=='2019'] <- asp$pasp200[asp$year=='2019']
asp <- asp[-which(asp$year %in% c('2011','2014')),]

ggplot(asp, aes(x=year, y=pasp*100)) +
  papertheme +
  geom_bar(stat = 'identity') +
  ylab('% Chara aspera at depth < 150cm \n excluding boulder substrate')
