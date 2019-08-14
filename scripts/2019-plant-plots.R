library(ggplot2)
library(reshape2)
library(viridis)
library(gridExtra)
library(extrafont)
loadfonts()

## read in data
plants <- readRDS("../dat-mod/mal-plantsurvey-2019-plants.rds")
peri <- readRDS("../dat-mod/mal-plantsurvey-2019-perimeter.rds")
bath <- readRDS("../dat-mod/mal-plantsurvey-2019-bathy.rds")

oplants <- readRDS("../dat-mod/mal-plantsurveys-wiikphd.rds")
trans <- readRDS('../dat-mod/mal-plantsurveys-wiikphd-transects.rds')
names(trans) <- tolower(names(trans))
## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')


### rename '' as no plants and separate extra points from rest - for now
## FIXME 0 still a few replaced by new point places
extras <- plants[which(plants$point>201),]
plants <- plants[-which(plants$point>201 | is.na(plants$species)),]

plants$species[plants$species=='' & plants$plantcover==0 ] <- 'No plants'
plants$species[plants$species=='' & plants$plantcover==39 ] <- 'pluc' # one missing entry
## FIXME: correct missing entry in raw data
plants <- droplevels(plants)

## sort out extra space in oplants
unique(oplants$substrate)
oplants$substrate <- gsub(' ', '', oplants$substrate)
oplants$substrate <- gsub('SI', 'Si', oplants$substrate)

## create aggregate codes for plants to identify key information
oaggs <- data.frame(species=unique(oplants$species))
oaggs$Plant <- c('Chara','Chara','Nitella','Chara','Moss','No plants','Moss',#the rows with no species
                'Carex','Chara','PotEut','Elodea','Chara','Chara','PotEut','Callitriche','Chara',
                'Zannichellia','PotLuc','Balls','Epiphyte','FilAlg','Characeae','Nitella',
                'Menyanthes','Equisetum','Chara','Chara','Epiphyte')

aggs <- data.frame(species=unique(plants$species))
aggs$Plant <- c('No plants','Elodea','Chara','Chara','FilAlg','Moss','Nitella','Chara',
                'Utricularia','Balls','Chara','PotEut','Chara','PotLuc','Moss','Moss',
                'Chara')
## merge agg names with plant survey data from all years
plants <- merge(plants, aggs)
oplants <- merge(oplants, oaggs)

## =============================================================================================
## plots of individual species and years
## ====================================================================================
#pplot <- 
  ggplot(plants, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=plantcover), shape=21, color='black') +
  geom_contour(data=bath, aes(z=z)) + 
  facet_wrap(~species) +
  scale_fill_viridis(option='viridis')

#opplot <- 
  ggplot(oplants, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=factor(abundance)), shape=21, color='black') +
  geom_contour(data=bath, aes(z=z)) + 
  facet_wrap(~Plant) +
  scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f'))

## =============================================================================================
## summarise aggregate groups to highest abundance possible per point
## =============================================================================================
groups <- ddply(plants, .(point,Plant), summarise, totcover=sum(plantcover),
                lat=lat[1],long=long[1], date=date[1], depth=depth[1])
ogroups <- ddply(oplants, .(date,transect,section,Plant), summarise, totcover=max(abundance),
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
  geom_line(data=trans, aes(x=Long, y=Lat,group=interaction(transect, location))) +
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
  geom_line(data=trans, aes(x=Long, y=Lat,group=interaction(transect, location))) +
  facet_wrap(~date, ncol=2) +
  scale_color_manual(values=c('#a6cee3','#33a02c','#b2df8a','#ff7f00','#fdbf6f',
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
## higher-summary figures of plant cover
## =============================================================================================
malsum <- ddply(plants, .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], 
                height=mean(plantheight))

subcodes <- data.frame(substrate = unique(malsum$substrate))
subcodes$GrainSize <- c(9,8,4,7,2,10,5,3,1,6,10)
malsum <- merge(malsum, subcodes)

omalsum <- ddply(oplants, .(date,transect, section), summarise, plantcover=max(abundance, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1])

osubcodes <- data.frame(substrate = unique(omalsum$substrate))
osubcodes$GrainSize <- c(2,4,3,1,6,5)
omalsum <- merge(omalsum, osubcodes)

## plant cover vs grain size of sediment
ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, size=GrainSize, color=plantcover)) +
  scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

ggplot(omalsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, col=GrainSize, size=plantcover)) +
  scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal() +
  facet_wrap(~date)

## plant height vs sediment
ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=coords.x1, y=coords.x2, color=height > 20, size=GrainSize)) +
  #scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

## plant cover vs depth
ggplot(malsum[grep('silt', malsum$substrate),],aes(x=depth, y=plantcover)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',formula = y ~ s(x, bs = "cs")) +
  ylab('Plant cover (%)') + xlab('Depth (cm)')

ggplot(omalsum[grep('Si', omalsum$substrate),],aes(x=depth, y=plantcover)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',formula = y ~ s(x, bs = "cs")) +
  ylab('Plant cover score') + xlab('Depth (cm)')

max(plants$depth[grep('chara', plants$species)], na.rm = T)  

## ======================================================================================
## increases in fil alg cover
## ======================================================================================
## limit to depth where fil alg systematically recorded: < 150cm
## in 2019 survey, we had whole area where we recorded 'gunk'. no filalg there so remove
##    these points. Also two points with NA at upper end of depth 138,143 so will remove those

algo <- oplants[oplants$depth < 150,c('date','transect','section','depth','filalg',
                                     'long','lat','substrate')]
algo <- algo[-which(duplicated(algo)),]

## since different effort by sections by year probs need to mean years
algo <- ddply(algo,.(transect,section), summarise, filalg=mean(filalg, na.rm = T))
algo$survey <- '2009/2010'

alg <- plants[plants$depth < 150 & plants$gunk==0,
              c('date','point','depth','algae','species','plantcover',
                                      'long','lat')]
alg$abundance <- NA
alg$abundance[alg$species == 'filalg' & alg$plantcover <= 10] <- 1
alg <- alg[-which(is.na(alg$abundance) & is.na(alg$algae)),]
alg$survey <- '2019'
alg$algae[which(alg$abundance == 1)] <- 1

alg <- rbind.fill(alg, algo)
alg$abundance <- ifelse(is.na(alg$filalg), alg$algae, alg$filalg)

algae <- 
  ggplot(alg, aes(x=survey, fill=abundance, group=abundance)) +
  geom_bar() +
  papertheme 

shallows <- 
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

ggsave('../figs/filalg-abundance.pdf',algplot, width=7, height=5)
