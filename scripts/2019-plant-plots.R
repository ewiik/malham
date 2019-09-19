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
bgroups$totcoverbin <- 0
bgroups$totcoverbin[bgroups$totcover > 0] <- 1

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
ndistro <- subplants[which(subplants$GrainSize<9 & subplants$survey=='200pt'),]
bdistro <- subplants[which(subplants$GrainSize<6 & subplants$GrainSize != -99 & 
                             subplants$survey=='csm'),]
distro <- rbind(ndistro, bdistro) 

## FIXME: what was up with June survey 2009, those transects migrated to August are spooking.
## long ice cover delayed macrophyte development?
##    totally change the depth distribution
ggplot(distro[-which(distro$year=='2009' & distro$transect >2),],
       aes(x=depth, y=biomass_scaled/100, group=year, col=year, fill=year)) +
  papertheme +
  geom_point(alpha=0.5, shape=21, col='black') +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),formula = y ~ s(x, bs = "cs", k=4), 
              fill='grey70') +
  ylab('Plant cover ex cobbles, boulders (%)') + xlab('Depth (cm)') +
  scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e'))
  #scale_color_viridis()

## biomass of plants in areas where we expect them (ie remove bouldery points)
#abplot <- 
ggplot(distro, aes(x=year, group=biomass_scaled, fill=biomass_scaled)) +
  papertheme +
  geom_bar() +
  #scale_fill_manual('Plant cover',values=c('#a6611a','#018571')) +
  ylab('Survey points on silt; including \n repeated points 2009/2010')

## occurrence of plants in areas where we expect them (ie remove bouldery points)
#abplot <- 
ggplot(distro, aes(x=year, group=biomass_scaled >0, fill=biomass_scaled>0)) +
  papertheme +
  geom_bar() +
  #scale_fill_manual('Plant cover',values=c('#a6611a','#018571')) +
  ylab('Survey points on silt; including \n repeated points 2009/2010')

## raw distribution of survey points across depth
#depthplot <-
ggplot(subplants, aes(x=depth, group=year, col=year)) +
  geom_density() +
  scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  #scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e'))
  papertheme + xlab('Depth (cm)') + ylab('Survey point density distribution')

#depthplot2 <-
ggplot(subplants, aes(x=depth, group=year, fill=year)) +
  geom_histogram(color='black', bins = 15) +
  #scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  papertheme + xlab('Depth (cm)') + ylab('Survey point density distribution')

occupancy <- grid.arrange(siltplot, occplot, depthplot, layout_matrix=rbind(c(2,3),c(2,1)))
ggsave("../figs/plant-occupancy.png", occupancy, height=10, width=8)

# quick proportional test on whether significantly different
ptable <- table(distro$year, distro$biomass>0)
summary(prop.test(x = ptable))

summary(lm(biomass >0 ~ factor(year), data=distro))

## plot distribution likelihood for key taxa
plist <- list('Chara','Nitella','Elodea','Fontinalis')

## make data frames that exclude boulders and are separate for each data type
cplants <- bgroups[!is.na(bgroups$id),]
nplants <- bgroups[is.na(bgroups$id),]

cplants <- cplants[cplants$id %in% cdistro$id,]
nplants <- nplants[nplants$point %in% ndistro$point,]

## function to start creating our data frames by plant
dfunc <- function(pname, dframe) {
  aggvar <- ifelse(is.na(dframe[1,'id']), 'point','id')
  df <- subset(dframe, dframe$Plant==pname)
  nodf <- subset(dframe, !dframe[,aggvar] %in% df[,aggvar]) #points without our plant
  nodf$Plant <- pname
  nodf$totcover <- 0
  nodf$totcover <- 0
  nodf <- nodf[-which(duplicated(nodf)),] # there may be many plants in points without our plant
  # need all points, and indicator if or not plant we want
  df <- rbind.fill(df, nodf)
}
chara <- dfunc('Chara', nplants)
ochara <- dfunc('Chara', cplants)  

nit<- dfunc('Nitella', nplants)
onit <- dfunc('Nitella', cplants)

elo <- dfunc('Elodea', nplants)
oelo <- dfunc('Elodea', cplants)

moss <- dfunc('Moss', nplants)
omoss <- dfunc('Moss', cplants)

keys <- rbind.fill(chara, ochara, nit, onit, elo, oelo, moss, omoss)

keys$totcover[keys$year %in% c('2009','2010','2018')] <- 
  keys$totcover[keys$year %in% c('2009','2010','2018')] * 30
keys$totcover[keys$year %in% c('2004','2005')] <- 
  keys$totcover[keys$year %in% c('2004','2005')] * 100

keys$totcoverbin[keys$totcover > 0] <- 1
keys$totcoverbin[keys$totcover == 0] <- 0

#charadist <-  # !!! key figure for paper see also https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
  ggplot(keys,aes(x=depth, y=totcover/100, group=year, col=year, fill=year)) +
  papertheme +
  geom_point(alpha=0.5, col='black', shape=21) +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),
              formula = y ~ s(x, bs = "cs",k=4), fullrange=F, se=F) +
  facet_wrap(~Plant) +
    scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  ylab('Charophyte cover (%)') + xlab('Depth (cm)') #+ ylim(c(0,100))

  ggplot(keys,aes(x=depth, y=totcoverbin, group=year, col=year, fill=year)) +
    papertheme +
    geom_point(alpha=0.5, col='black', shape=21) +
    stat_smooth(method = 'gam',method.args = list(family = "binomial"),
                formula = y ~ s(x, bs = "cs",k=4), fullrange=F, se=F) +
    facet_wrap(~Plant) +
    scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    ylab('Charophyte cover (%)') + xlab('Depth (cm)') #+ ylim(c(0,100))
  ## FIXME: make sure na depth 75 becomes 75 or 80
  
speciesplots <- grid.arrange(charadist, fontdist, elodist, layout_matrix=rbind(c(1,1),c(2,3)))
ggsave('../figs/species-depths.png', speciesplots, width=7, height=7)

## ======================================================================================
## 2019 fil alg cover
## ======================================================================================
## limit to depth where fil alg systematically recorded: < 150cm
## in 2019 survey, we had whole area where we recorded 'gunk'. no filalg there so remove
##    these points. Also two points with NA at upper end of depth 138,143 so will remove those

algo <- subplants[subplants$depth < 150 & subplants$survey=='200pt',
                  c('date','transect','location','depth','filalg',
                                     'long','lat','substrate', 'year', 'id')]
algo <- algo[-which(is.na(algo$filalg)),]
#algae <- 
  ggplot(algo, aes(x=year, fill=filalg, group=filalg)) +
  geom_bar() +
  papertheme 

#shallows <- 
  ggplot(peri, aes(x=long,y=lat)) +
  papertheme +
  geom_path(aes(group=group),color="black") +
  geom_contour(data=bath, aes(z=z), col='grey',size=0.1) + 
  geom_point(data=unique(allplants[allplants$depth<150 & allplants$survey=='200pt',
                                   c('lat','long','point','algae','gunk')]),shape=21, 
             aes(fill=factor(algae), color=factor(gunk)), size=2.5) +
  scale_fill_manual('Algal abundance', values=c('#74c476','#31a354','#006d2c')) +
  scale_color_manual('Gunk',values=c('transparent','black')) +
    coord_equal() + theme(legend.box='vertical')

algplot <- grid.arrange(algae, shallows, ncol=2)

ggsave('../figs/filalg-abundance.png',algplot, width=7, height=5)

## ============================================================================
## percentage occurrence of chara aspera in survey?
## ==============================================================================
asp <- allplants[which(allplants$depth < 150 ),]
asp <- asp[-which(asp$substrate %in% c('BO','CO')),]
asp <- asp[-which(asp$substrate=='boulder,stone'),]
asp <- asp[-which(asp$substrate=='stony,boulder'),]

aspalt <- asp[which(asp$location=='shore'),]

aspsum <- ddply(asp,.(year), summarise, pasp200 = length(which(fullname=='Chara aspera'))/length(unique(point)),
             pasp=length(which(fullname=='Chara aspera'))/length(unique(id)))
aspsum$paspsum[aspsum$year=='2019'] <- aspsum$paspsum200[aspsum$year=='2019']

aspsumalt <- ddply(aspalt,.(year), summarise,pasp=length(which(fullname=='Chara aspera'))/length(unique(id)))

ggplot(aspsum, aes(x=year, y=pasp*100)) +
  papertheme +
  geom_bar(stat = 'identity') +
  ylab('% Chara aspera at depth < 150cm \n excluding boulder substrate')

ggplot(aspsumalt, aes(x=year, y=pasp*100)) +
  papertheme +
  geom_bar(stat = 'identity') +
  ylab('% Chara aspera at depth < 150cm \n excluding boulder substrate')

ggplot(asp[which(asp$fullname=='Chara aspera'),], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black", size=0.5) +
  geom_point(aes(fill=abundance>0), 
             shape=21, color='black', alpha=0.7) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap( ~ year, ncol=3)# +
  #scale_fill_distiller(palette = 'Greens', direction=1) +
  #scale_color_manual(values = c('black','#fdcc8a','#fc8d59','#d7301f'))
