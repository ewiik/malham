library(ggplot2)
library(reshape2)
library(viridis)
library(gridExtra)
library(extrafont)
loadfonts()

## FIXME: difference between biomass and abundance in phd surveys

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

## FIXME: probs wortj assigning NA to 0 for abundance in oplants
oplants$species[oplants$transect==1 & oplants$section=='4d' & 
                  oplants$depth==75 & oplants$biomass==0] <- ''

plants$plantheight[is.na(plants$plantheight)] <- 0 # these are fil alg and one 0 biomass site

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
## summarise plantcover over all substrates and depths
malsum <- ddply(plants, .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], 
                height=mean(plantheight, na.rm = T))

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
distro <- malsum[grep('silt', malsum$substrate),c('plantcover','depth')]
distro$Survey <- '2019'

distro <- rbind.fill(distro, omalsum[grep('Si', omalsum$substrate),c('plantcover','depth')])
distro$plantcover[is.na(distro$Survey)] <- distro$plantcover[is.na(distro$Survey)] * 25
distro$plantcover[distro$plantcover==-Inf] <- 0

distro$Survey[is.na(distro$Survey)] <- '2009/2010'

ggplot(distro,aes(x=depth, y=plantcover, group=Survey, col=Survey)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',formula = y ~ s(x, bs = "cs")) +
  ylab('Plant cover on silt (%)') + xlab('Depth (cm)')

## chara depth distribution (including nitella/chara uncertainty)
chara <- ddply(plants[plants$Plant=='Chara',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Chara',
                height=mean(plantheight), survey='2019')

ochara <- ddply(oplants[oplants$Plant=='Chara',], .(date,transect, section), summarise, 
                plantcover=max(abundance, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Chara', survey='2009/2010')

ocharo <- ddply(oplants[grep('Chara', oplants$Plant),], .(date,transect, section), summarise, 
                plantcover=max(abundance, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Charophytes', survey='2009/2010')

nit<- ddply(plants[plants$Plant=='Nitella',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
               height=mean(plantheight), survey='2019')

onit <- ddply(oplants[oplants$Plant=='Nitella',], .(date,transect, section), summarise, 
                plantcover=max(abundance, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', survey='2009/2010')

keys <- rbind.fill(chara, ochara, nit, onit) # checked ocharo and the regression is same
keys$plantcover[keys$survey=='2009/2010'] <- keys$plantcover[keys$survey=='2009/2010'] * 10+ 
  (keys$plantcover[keys$survey=='2009/2010'] - 1) * 20

charadist <- ggplot(keys,aes(x=depth, y=plantcover, group=survey, col=survey)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',method.args = list(family = "tw"),
              formula = y ~ s(x, bs = "cs"), fullrange=F) +
  facet_wrap(~Plant) +
  ylab('Charophyte cover (%)') + xlab('Depth (cm)')

font<- ddply(plants[plants$Plant=='Moss',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
            substrate = substrate[1],
            long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
            height=mean(plantheight), survey='2019')

ofont <- ddply(oplants[oplants$Plant=='Moss',], .(date,transect, section), summarise, 
              plantcover=max(abundance, na.rm=T), 
              substrate = substrate[1],
              long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', survey='2009/2010')

fontinalis <- rbind.fill(font, ofont) 
fontinalis$plantcover[fontinalis$survey=='2009/2010'] <- 
  fontinalis$plantcover[fontinalis$survey=='2009/2010'] * 10 + 
  (fontinalis$plantcover[fontinalis$survey=='2009/2010'] - 1) * 20

fontdist <- ggplot(fontinalis,aes(x=depth, y=plantcover, group=survey, col=survey)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',method.args = list(family = "tw"),
              formula = y ~ s(x, bs = "cs"), fullrange=F) +
  ylab('Fontinalis cover (%)') + xlab('Depth (cm)')

elo <- ddply(plants[plants$Plant=='Elodea',], .(point), summarise, plantcover=sum(plantcover, na.rm=T), 
             substrate = substrate[1],
             long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
             height=mean(plantheight), survey='2019')

oelo <- ddply(oplants[oplants$Plant=='Elodea',], .(date,transect, section), summarise, 
               plantcover=max(abundance, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', survey='2009/2010')

elodea <- rbind.fill(elo, oelo) 
elodea$plantcover[elodea$survey=='2009/2010'] <- 
  elodea$plantcover[elodea$survey=='2009/2010'] * 10 + 
  (elodea$plantcover[elodea$survey=='2009/2010'] - 1) * 20

elodist <- 
  ggplot(elodea,aes(x=depth, y=plantcover, group=survey, col=survey)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',method.args = list(family = "tw"),
              formula = y ~ s(x, bs = "cs"), fullrange=F) +
  ylab('Elodea cover (%)') + xlab('Depth (cm)')

speciesplots <- grid.arrange(charadist, fontdist, elodist, layout_matrix=rbind(c(1,1),c(2,3)))
ggsave('../figs/species-depths.png', speciesplots, width=7, height=7)

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

ggsave('../figs/filalg-abundance.png',algplot, width=7, height=5)

## ==========================================================================
## overall decline of macrophyte cover
## ============================================================================
## occurrence of plants in areas where we expect them (ie limit to silt for simplicity)
silt <- malsum[grep('silt',malsum$substrate),]
silt$survey <- '2019'
osilt <- omalsum[omalsum$substrate=='Si',]
osilt$survey <- '2009/2010'

silt <- rbind.fill(silt, osilt)

siltplot <- 
  ggplot(peri, aes(x=long,y=lat)) +
  papertheme +
  geom_path(aes(group=group),color="black") +
  geom_contour(data=bath, aes(z=z), col='grey',size=0.1) + 
  geom_point(data=silt,aes(color=survey)) +
  #geom_line(data=trans[trans$location=='shore',], aes(group=transect),
            #size=2, alpha=0.7) +
  # scale_color_manual('Survey',values=c('transparent','black')) +
  coord_equal() + theme(legend.box='vertical', axis.title = element_blank(),
                        axis.text = element_blank())

occplot <- 
  ggplot(silt, aes(x=survey, group=plantcover > 0, fill=plantcover > 0)) +
  papertheme +
  geom_bar() +
  scale_fill_manual('Plant cover',values=c('#a6611a','#018571')) +
ylab('Survey points on silt; including \n repeated points 2009/2010')

depthplot <-
  ggplot(silt, aes(x=depth, group=survey, col=survey)) +
  geom_density() +
    papertheme + xlab('Depth (cm)') + ylab('Density distribution')

occupancy <- grid.arrange(siltplot, occplot, depthplot, layout_matrix=rbind(c(2,3),c(2,1)))
ggsave("../figs/plant-occupancy.png", occupancy, height=10, width=8)

# quick proportional test on whether significantly different
table(silt$survey, silt$plantcover>0)
prop.test(x = c(168, 96), n = c(168+21, 96+76))

## chara bed distribution across depth


## percentage occurrence of chara aspera and chara glob in survey? or just chara?
## ==============================================================================