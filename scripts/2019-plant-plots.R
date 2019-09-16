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

## read data
alltrans <- readRDS('../dat-mod/all-plant-transects.rds')
allplants <- readRDS('../dat-mod/all-plant-surveys.rds')
subplants <- readRDS('../dat-mod/all-plant-surveys-nospecies.rds')

peri <- readRDS("../dat-mod/mal-plantsurvey-2019-perimeter.rds")
bath <- readRDS("../dat-mod/mal-plantsurvey-2019-bathy.rds")

## correct Fontinalis to moss in 2019
allplants$Plant[allplants$Plant=='Fontinalis'] <- 'Moss'

## create subsets for when we might want to remove poor data 
poorplants <- allplants
poorsubplants <- subplants

allplants <- allplants[-which(allplants$poor=='yes'),]
subplants <- subplants[-which(subplants$poor=='yes'),]

## =============================================================================================
## plots with species information
## ====================================================================================
# declare list of most interesting groups
want <- c('Balls','Callitriche','Chara','Elodea','Characeae','Fontinalis','Nitella','No plants' ,
          'PotEut','PotLuc','Tolypella','Utricularia','Zannichellia','Moss')

# csm with and without species abundance
pwant <- allplants[allplants$survey=='csm' & allplants$speciestype== 'numeric'& 
                     allplants$Plant %in% want,]
pwant2 <- allplants[allplants$survey=='csm' & allplants$Plant %in% want,]

# plant cover by species in 2019
#pplot <- 
  ggplot(allplants[allplants$year==2019,], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=plantcover), shape=21, color='black') +
  geom_contour(data=bath, aes(z=z)) + 
  facet_wrap(~fullname) +
  scale_fill_viridis(option='viridis')


ggplot(pwant, aes(x=long,y=lat)) +
    papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
    geom_point(aes(fill=factor(abundance)), shape=21, color='black',size=2, alpha=.7) +
     facet_wrap(fullname~datefac, labeller = function(labs) {label_value(labs, multi_line = FALSE)},
               ncol=8) + 
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
## summarise aggregate species groups to highest abundance possible per point; 2019 and numeric csm
## =============================================================================================
groups <- ddply(allplants[which(allplants$survey=='200pt'),], .(point,Plant), summarise, 
                totcover=sum(plantcover, na.rm = T),
                lat=lat[1],long=long[1], year=year[1], depth=depth[1])

ogroups <- ddply(allplants[allplants$speciestype=='numeric' & allplants$survey=='csm',], 
                 .(id,Plant), summarise, totcover=max(abundance), year=year[1],
                 lat=lat[1],long=long[1], depth=depth[1])

bgroups <- ddply(allplants[allplants$survey=='csm',], 
                 .(id,Plant), summarise, totcover=max(abundance), year=year[1],
                 lat=lat[1],long=long[1], depth=depth[1])
bgroups <- rbind.fill(groups, bgroups)
bgroups$totcover[bgroups$totcover > 0] <- 1

groups <- rbind.fill(groups,ogroups)

## choose which to show
plotdf <- groups[groups$Plant %in% want,]

# plot of total cover of Plant groupings though numeric surveys
ggplot(plotdf, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black", size=0.5) +
  geom_point(data=plotdf[is.na(plotdf$id),],aes(fill=totcover), 
             shape=21, color='black', alpha=0.7) +
  geom_point(data=plotdf[!is.na(plotdf$id),],aes(col=factor(totcover))) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap(Plant ~ year, ncol=6,labeller = function(labs) {label_value(labs, multi_line = FALSE)} ) +
  scale_fill_distiller(palette = 'Greens', direction=1) +
  scale_color_manual(values = c('black','#fdcc8a','#fc8d59','#d7301f'))

df <- ogroups[ogroups$Plant %in% want,]
 # slightly different version of same thing with just csm surveys
ggplot(df, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(size=factor(totcover), color=Plant, shape=totcover==0), alpha=0.5) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  #geom_line(data=alltrans, aes(x=long, y=lat,group=interaction(transect, location))) +
  facet_wrap(Plant~year,  labeller = function(labs) {label_value(labs, multi_line = FALSE)}) #+
  #scale_color_manual(values=c('#a6cee3','#33a02c','#b2df8a','#ff7f00','#fdbf6f',
                       #       '#1f78b4','#fb9a99','#e31a1c','#cab2d6','#6a3d9a'))

# occurrence plot that can therefore show all surveys we have data for
df <- bgroups[bgroups$Plant %in% want,]
ggplot(df, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(color=factor(totcover)), alpha=0.5) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap(Plant~year,  labeller = function(labs) {label_value(labs, multi_line = FALSE)})

## =============================================================================================
## higher-summary maps of plant cover 2019 and numeric abundance
## =============================================================================================
# create continuous for substrate
subcodes <- data.frame(substrate = unique(allplants$substrate[allplants$survey=='200pt']))
subcodes$GrainSize <- c(9,2,8,1,10,5,7,10,3,6,4)
subcodes$survey <- '200pt'

subcodes <- rbind(subcodes, 
                  data.frame(substrate=unique(allplants$substrate[allplants$survey=='csm']),
                             GrainSize=c(3,2,6,1,5,NA,4,7,-99), # roots gets -99
                             survey='csm'))

subplants <- merge(subplants, subcodes)
subplants$biomass_scaled <- subplants$biomass
subplants$biomass_scaled[subplants$survey =='csm'] <- subplants$biomass_scaled[subplants$survey =='csm'] * 30

# create summary for 2019 plant height
malsum <- ddply(allplants[allplants$survey=='200pt',], .(point), summarise, 
                   height=mean(plantheight, na.rm = T))

malsum <- merge(malsum, 
                   data.frame(unique(allplants[allplants$survey=='200pt',
                                               c('datefac','point',
                                                 "date","depth" ,"substrate",
                                                 'long','lat','year')])))
subcodes$substrate <- as.character(subcodes$substrate)
malsum <- merge(malsum, subcodes[subcodes$survey=='200pt',])

## biomass over all substrates over time
## plant cover vs grain size of sediment
ggplot(subplants[-which(subplants$GrainSize<0),]) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, fill=biomass_scaled), alpha=0.5, shape=21, color='black', size=2) +
  scale_fill_viridis(direction = -1) +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal() +
  facet_wrap(~year)  #labeller = function(labs) {label_value(labs, multi_line = FALSE)}

## plant height vs sediment
ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, color=height > 20, size=GrainSize)) +
  #scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, fill=height, size=GrainSize), color='black', shape=21, alpha=0.7) +
  scale_fill_viridis(direction = -1) +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

## =============================================================================================
## higher-summary maps of plant cover 2019 and numeric abundance
## =============================================================================================
## plant cover vs depth excluding bouldery substrate
## FIXME: this isn't tidy now since renamed data frames etc. so check all data right
##    also make this a function so that it doesn't take so many lines
distro <- subplants[which(subplants$GrainSize<9 & subplants$survey=='200pt'),]

distro <- rbind(distro, data.frame(subplants[which(subplants$GrainSize<6 & subplants$GrainSize != -99 & 
                                           subplants$survey=='csm'),])) # NA are the > 75

## FIXME: what was up with June survey 2009, those transects migrated to August are spooking.
## long ice cover delayed macrophyte development?
##    totally change the depth distribution
ggplot(distro[-which(distro$year=='2009' & distro$transect >2),],aes(x=depth, y=biomass_scaled/100, group=year, col=year, fill=year)) +
  papertheme +
  geom_point(alpha=0.5, shape=21, col='black') +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),formula = y ~ s(x, bs = "cs", k=4), fill='grey70') +
  ylab('Plant cover ex cobbles, boulders (%)') + xlab('Depth (cm)') +
  scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e'))
  #scale_color_viridis()

## chara depth distribution (including nitella/chara uncertainty)
chara <- ddply(allplants[allplants$Plant=='Chara' & allplants$survey=='200pt',], .(point), 
               summarise, plantcover=sum(plantcover, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Chara',
                height=mean(plantheight), year=format(date, '%Y')[1])
chara2 <- ddply(allplants[!allplants$point %in% chara$point & allplants$survey=='200pt',], .(point), 
                summarise, plantcover=0, 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Chara',
               height=0, year=format(date, '%Y')[1])

numplants <- allplants[allplants$survey=='csm' & allplants$speciestype=='numeric',]
numbiplants2 <- allplants[allplants$survey=='csm',]

ochara <- ddply(numplants[numplants$Plant=='Chara',], .(id), summarise, 
                plantcover=max(abundance, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Chara', year=year[1])

ochara2 <- ddply(numplants[!numplants$id %in% ochara$id,], .(id), summarise, 
                plantcover=0, 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Chara', year=year[1])

nit<- ddply(allplants[allplants$Plant=='Nitella' & allplants$survey=='200pt',], .(point), 
            summarise, plantcover=sum(plantcover, na.rm=T), 
               substrate = substrate[1],
               long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
               height=mean(plantheight), year=format(date, '%Y')[1])
nit2<- ddply(allplants[!allplants$point %in% nit$point & allplants$survey=='200pt',], .(point), 
             summarise, plantcover=0, 
            substrate = substrate[1],
            long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella',
            height=0, year=format(date, '%Y')[1])

onit <- ddply(numplants[numplants$Plant=='Nitella',], .(id), summarise, 
                plantcover=max(abundance, na.rm=T), 
                substrate = substrate[1],
                long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', year=year[1])
onit2 <- ddply(numplants[!numplants$id %in% onit$id,], .(id), summarise, 
              plantcover=0, 
              substrate = substrate[1],
              long=long[1], lat=lat[1], depth=depth[1], Plant='Nitella', year=year[1])


keys <- rbind.fill(chara, chara2,ochara,ochara2, nit,nit2, onit,onit2)
keys$plantcover[!keys$year=='2019'] <- keys$plantcover[!keys$year=='2019'] * 30

#keys$substrate[keys$substrat=='SI'] <- 'silt' 
#keys <- keys[grep('silt',keys$substrate),]

#charadist <-  # !!! key figure for paper see also https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
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
