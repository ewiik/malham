## plots for plant survey data

## get libraries
library(ggplot2)
library(metR) # contour labels
library(ggsn)
library(reshape2)
library(viridis)
library(gridExtra)
library(extrafont)
library(plyr)
library(mgcv)

loadfonts()

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')

## read data
alltrans <- readRDS('../dat-mod/all-plant-transects.rds')
allplants <- readRDS('../dat-mod/all-plant-surveys.rds')
noplants <- readRDS('../dat-mod/all-plant-surveys-nospecies.rds') # the data just with depth biomass etc. ie no point duplication

peri <- readRDS("../dat-mod/mal-plantsurvey-2019-perimeter.rds")
bath <- readRDS("../dat-mod/mal-plantsurvey-2019-bathy.rds")

## correct Fontinalis to moss in 2019
allplants$Plant[allplants$Plant=='Fontinalis'] <- 'Moss'

## =============================================================================
## create subsets and other altered data  
## ===============================================================================
## for when we might want to look again at poor data
poorplants <- allplants
poornoplants <- noplants

## remove poor data from main data sets
allplants <- allplants[-which(allplants$poor=='yes'),]
noplants <- noplants[-which(noplants$poor=='yes'),]

# declare list of most interesting plant groups
want <- c('Balls','Callitriche','Chara','Elodea','Characeae','Fontinalis','Nitella','No plants' ,
          'PotEut','PotLuc','Tolypella','Utricularia','Zannichellia','Moss')

## csm with and without species abundance
csmnum <- allplants[allplants$survey=='csm' & allplants$speciestype== 'numeric'& 
                     allplants$Plant %in% want,]
csmall <- allplants[allplants$survey=='csm' & allplants$Plant %in% want,]

## summarise aggregate species groups to highest abundance possible per point; 2019 and numeric csm
ptgroups <- ddply(allplants[which(allplants$survey=='200pt'),], .(point,Plant), summarise, 
                totcover=sum(plantcover, na.rm = T),
                lat=lat[1],long=long[1], year=year[1], depth=depth[1]) # sum of cover for 2019 data

numgroups <- ddply(allplants[allplants$speciestype=='numeric' & allplants$survey=='csm',], 
                 .(id,Plant), summarise, totcover=max(abundance), year=year[1],
                 lat=lat[1],long=long[1], depth=depth[1]) # max of abundance here

bingroups <- ddply(allplants[allplants$survey=='csm',], 
                 .(id,Plant), summarise, totcover=max(abundance), year=year[1],
                 lat=lat[1],long=long[1], depth=depth[1]) # max here too where >0 to be converted to 1

bingroups <- rbind.fill(ptgroups, bingroups) # bring all surveys back together
bingroups$totcover[bingroups$totcover > 0] <- 1 # convert all abundance >0 to 1 for binary

numgroups <- rbind.fill(ptgroups,numgroups) # only numeric surveys

# create continuous value for substrate and insert into noplants
subcodes <- data.frame(substrate = unique(allplants$substrate[allplants$survey=='200pt']))
subcodes$GrainSize <- c(9,2,8,1,10,5,7,10,3,6,4)
subcodes$survey <- '200pt'

subcodes <- rbind(subcodes, 
                  data.frame(substrate=unique(allplants$substrate[allplants$survey=='csm']),
                             GrainSize=c(3,2,6,1,5,NA,4,7,-99), # roots gets -99
                             survey='csm'))


noplants <- merge(noplants, subcodes)

## create scaled biomass for semi-quantitative data
noplants$biomass_scaled <- noplants$biomass
noplants$biomass_scaled[noplants$survey =='csm'] <- noplants$biomass_scaled[noplants$survey =='csm'] * 30

## create mean for 2019 plant height
malheight <- ddply(allplants[allplants$survey=='200pt',], .(point), summarise, 
                height=mean(plantheight, na.rm = T))

malheight <- merge(malheight, 
                data.frame(unique(allplants[allplants$survey=='200pt',
                                            c('datefac','point',
                                              "date","depth" ,"substrate",
                                              'long','lat','year')])))
subcodes$substrate <- as.character(subcodes$substrate)
malheight <- merge(malheight, subcodes[subcodes$survey=='200pt',])

## exclude bouldery substrate for analyses that look at macrophyte cover over substrate fruitful for occupancy
## this excludes 'stony,boulder'; 'stony,boulder,gravel' from 2019, and boulders from csm (not cobbles)
subspt <- noplants[which(noplants$GrainSize<9 & noplants$survey=='200pt'),] # retain finer substrates from points survey
subscsm <- noplants[which(noplants$GrainSize<=6 & noplants$GrainSize != -99 & # retain finer substrates from CSM surveys
                             noplants$survey=='csm'),]
subs <- rbind(subspt, subscsm) 

## exclude boulders from binary and numeric plant group data, multiply CSM data by 30
bingroupsCSMNoBoulders <- bingroups[!is.na(bingroups$id),]
bingroupsPtsNoBoulders <- bingroups[is.na(bingroups$id),]

bingroupsCSMNoBoulders <- bingroupsCSMNoBoulders[bingroupsCSMNoBoulders$id %in% subscsm$id,]
bingroupsPtsNoBoulders <- bingroupsPtsNoBoulders[bingroupsPtsNoBoulders$point %in% subspt$point,]

numgroupsCSMNoBoulders <- numgroups[!is.na(numgroups$id),]
numgroupsPtsNoBoulders <- numgroups[is.na(numgroups$id),]

numgroupsCSMNoBoulders <- numgroupsCSMNoBoulders[numgroupsCSMNoBoulders$id %in% subscsm$id,]
numgroupsCSMNoBoulders$totcover <- numgroupsCSMNoBoulders$totcover * 30 # convert abundance to quant
numgroupsPtsNoBoulders <- numgroupsPtsNoBoulders[numgroupsPtsNoBoulders$point %in% subspt$point,]

## function to start creating our data frames by plant that preserve zero points
## recall that bingroups and numgroups contain the "plant" = "No plants" so they have the zeroes already.
dfunc <- function(pname, dframe) {
  aggvar <- ifelse(is.na(dframe[1,'id']), 'point','id')
  df <- subset(dframe, dframe$Plant==pname)
  nodf <- subset(dframe, !dframe[,aggvar] %in% df[,aggvar]) #points without our plant
  nodf$Plant <- pname
  nodf$totcover <- 0
  nodf <- nodf[-which(duplicated(nodf)),] # there may be many plants in points without our plant
  # need all points, and indicator if or not plant we want
  df <- rbind.fill(df, nodf)
}
chara <- dfunc('Chara', bingroupsPtsNoBoulders)
ochara <- dfunc('Chara', bingroupsCSMNoBoulders)  

nit<- dfunc('Nitella', bingroupsPtsNoBoulders)
onit <- dfunc('Nitella', bingroupsCSMNoBoulders)

elo <- dfunc('Elodea', bingroupsPtsNoBoulders)
oelo <- dfunc('Elodea', bingroupsCSMNoBoulders)

moss <- dfunc('Moss', bingroupsPtsNoBoulders)
omoss <- dfunc('Moss', bingroupsCSMNoBoulders)

charanum <- dfunc('Chara', numgroupsPtsNoBoulders)
ocharanum <- dfunc('Chara', numgroupsCSMNoBoulders)  

nitnum <- dfunc('Nitella', numgroupsPtsNoBoulders)
onitnum <- dfunc('Nitella', numgroupsCSMNoBoulders)

elonum <- dfunc('Elodea', numgroupsPtsNoBoulders)
oelonum <- dfunc('Elodea', numgroupsCSMNoBoulders)

mossnum <- dfunc('Moss', numgroupsPtsNoBoulders)
omossnum <- dfunc('Moss', numgroupsCSMNoBoulders)

keyplants <- rbind.fill(chara, ochara, nit, onit, elo, oelo, moss, omoss)
keyplants$year <- as.numeric(keyplants$year)
keyplants$Plant <- factor(keyplants$Plant, levels = c('Chara','Nitella','Elodea','Moss'))

keyplantsnum <- rbind.fill(charanum, ocharanum, nitnum, onitnum, elonum, oelonum, mossnum, omossnum)
keyplantsnum$totcover <- keyplantsnum$totcover/100
keyplantsnum$year <- as.numeric(keyplantsnum$year)
keyplantsnum$Plant <- factor(keyplantsnum$Plant)
keyplantsnum$Plant <- factor(keyplantsnum$Plant, levels = c('Chara','Nitella','Elodea','Moss'))

## =============================================================================================
## plot with survey points in 2019 and bathymetry; plus a 'mean transect' of CSM 
## ====================================================================================
samplingplot <- 
ggplot(alltrans[alltrans$year == 2009,], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  stat_contour(data=bath, aes(z=z/100), color='grey70')  + 
  geom_text_contour(data=bath, aes(z=z/100), stroke = 1, check_overlap = T) +
  geom_point(data=noplants[noplants$year==2019,], aes(color=year)) +
  geom_line(data=alltrans[alltrans$year==2009,],
            aes(group=interaction(factor(transect), location), color=year), size=1.5, alpha=0.7) +
  scale_color_manual('Survey', values=c('#a6611a','#018571'), labels=c('CSM','Points')) +
  coord_equal() + ylab('Latitude (utm)') + xlab('Longitude (utm)') +
  ggsn::scalebar(peri, location='bottomleft', transform=F, dist = 100, st.size=3, height=0.01,
                 dist_unit = 'm') +
  north(peri, location = 'bottomright', symbol = 17) +
  annotate(geom = 'text', label = c('Inflow','Outflow'), x = c(-Inf, Inf), y = c(Inf,-Inf), hjust = c(-0.3,3.5), 
           vjust = c(6,-1),
           family = papertheme$text$family)
  
ggsave('../figs/plantsurveys-transects.jpg', samplingplot)

## raw distribution of survey points across depth
depthplot <-
  ggplot(noplants, aes(x=depth, group=year, col=year)) +
  geom_density(size=2) +
  scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  #scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e'))
  papertheme + xlab('Depth (cm)') + ylab('Survey point density distribution')

ggsave('../figs/plant-surveypoints-depth.jpg', depthplot)

## =============================================================================================
## plots with species information
## ====================================================================================

# plant cover by species in 2019
#pplot <- 
  ggplot(allplants[allplants$year==2019,], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=plantcover), shape=21, color='black') +
  geom_contour(data=bath, aes(z=z)) + 
  facet_wrap(~fullname) +
  scale_fill_viridis(option='viridis')


ggplot(csmnum, aes(x=long,y=lat)) +
    papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
    geom_point(aes(fill=factor(abundance)), shape=21, color='black',size=2, alpha=.7) +
     facet_wrap(fullname~datefac, labeller = function(labs) {label_value(labs, multi_line = FALSE)},
               ncol=8) + 
    scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f','black')) +
  scale_color_manual(values=c('transparent','black'))

ggplot(csmall, aes(x=long,y=lat)) +
  papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=factor(abundance > 0)), shape=21, color='black',size=2, alpha=.7) +
  facet_wrap(Plant~datefac,labeller = function(labs) {label_value(labs, multi_line = FALSE)}) + 
  scale_fill_manual(values = c('transparent','#d7301f')) 

## =============================================================================================
## plots with aggregate plant information requiring species data
## =============================================================================================

## choose which to show
plotdf <- groups[which(groups$Plant %in% want),]
plotdf <- plotdf[-which(plotdf$Plant == 'Zannichellia'),]
plotdf$plantfac <- factor(plotdf$Plant, levels=c("No plants", "Chara","Nitella","Elodea",   "Balls",
                                                 "Moss", "PotLuc","Callitriche","PotEut","Utricularia"))
plotdf$altcover <- ifelse(is.na(plotdf$id), plotdf$totcover, plotdf$totcover*30)

# plot of total cover of Plant groupings though numeric surveys
#plantmaps <-
  ggplot(plotdf, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black", size=0.5) +
  geom_point(data=plotdf[is.na(plotdf$id),],aes(fill=totcover), 
             shape=21, color='black', alpha=0.7) +
  geom_point(data=plotdf[!is.na(plotdf$id),],aes(col=factor(totcover))) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap(plantfac ~ year, ncol=5,labeller = function(labs) {label_value(labs, multi_line = FALSE)} ) +
  scale_fill_distiller('Cover (%)',palette = 'Greens', direction=1) +
  scale_color_manual('Cover (1:3)',values = c('black','#fdcc8a','#fc8d59','#d7301f')) +
  coord_equal() +
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())

plantmaps <-
  ggplot(plotdf[-which(plotdf$plantfac %in% c("Balls","Callitriche")),], aes(x=long,y=lat)) +
  papertheme +
  geom_contour(data=bath, aes(z=z), color='grey70', linetype='solid', size=0.2) + 
  geom_path(data=peri, aes(group=group),color="grey70", size=0.5, linetype='solid') +
  geom_point(aes(fill=altcover), 
             shape=21, color='black', alpha=0.7, size=1) +
  facet_grid(year ~ plantfac,labeller = function(labs) {label_value(labs, multi_line = FALSE)} ) +
  scale_fill_distiller('Cover (%)',palette = 'Greens', direction=1) +
  coord_equal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())

ggsave('../figs/plant-maps-time.jpg', plantmaps, width=18, units='cm')
ggsave('../figs/plant-maps-time.pdf', plantmaps, width=28, units='cm')

df <- numgroups[numgroups$Plant %in% want,]
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
df <- bingroups[bingroups$Plant %in% want,]
ggplot(df, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(color=factor(totcover)), alpha=0.5) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap(Plant~year,  labeller = function(labs) {label_value(labs, multi_line = FALSE)})

## =============================================================================================
## higher-summary maps of plant cover 2019 and numeric abundance
## =============================================================================================
## biomass over all substrates over time
## plant cover vs grain size of sediment
ggplot(noplants[-which(noplants$GrainSize<0),]) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, fill=biomass_scaled), alpha=0.5, shape=21, color='black', size=2) +
  scale_fill_viridis(direction = -1) +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal() +
  facet_wrap(~year)  #labeller = function(labs) {label_value(labs, multi_line = FALSE)}

## plant height vs sediment; 2019
ggplot(malheight) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, color=height > 20, size=GrainSize)) +
  #scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

heightplot <- ggplot(malheight) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, fill=height, size=GrainSize), color='black', shape=21, alpha=0.9) +
  #scale_fill_viridis(direction = -1) +
  scale_fill_gradientn(name='Plant height (cm)',
    colors=c("#7b3294","white","#4dac26"),
    #colors=c("purple","white","green"),
    values=rescale(c(0,20,220)),
    limits=c(0,220),
    na.value = 'black') +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z), col='grey70') +
  coord_equal() + ylab('Latitude (utm)') + xlab('Longitude (utm)') +
  ggsn::scalebar(peri, location='bottomleft', transform=F, dist = 100, st.size=3, height=0.01,
                 dist_unit = 'm') +
  north(peri, location = 'bottomright', symbol = 17)

ggsave('../figs/plant-height-2019.jpg', heightplot)

## =============================================================================================
## higher-summary maps of plant cover and numeric abundance
## =============================================================================================
## FIXME: what was up with June survey 2009, those transects migrated to August are spooking.
## long ice cover delayed macrophyte development?
##    totally change the depth distribution
ggplot(subs[-which(subs$year=='2009' & subs$transect >2),],
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
abplot <- 
ggplot(subs, aes(x=year, group=biomass_scaled, fill=biomass_scaled)) +
  papertheme +
  geom_bar(position = 'fill') +
  scale_fill_distiller('Scaled plant cover',palette = 'Greens', direction = 1) +
  ylab('Survey points ex cobbles and boulders') + xlab('Year')

## occurrence of plants in areas where we expect them (ie remove bouldery points)
ocplot <- 
ggplot(subs, aes(x=year, group=biomass_scaled >0, fill=biomass_scaled>0)) +
  papertheme +
  geom_bar(position = 'fill') +
  scale_fill_manual('Plant occurrence',values=c('#a6611a','#018571'), labels=c(0,1)) +
  ylab('Survey points ex cobbles and boulders') + xlab('Year')

plantpts <- grid.arrange(abplot, ocplot, ncol=1)

ggsave('../figs/plant-biomass.jpg',plantpts, height=7, width=7)

# quick proportional test on whether significantly different
ptable <- table(subs$year, subs$biomass>0)
summary(prop.test(x = ptable))

summary(lm(biomass >0 ~ factor(year), data=subs))

## =========================================================================================
## plot the occurrences and abundances of key aggregate groups for the paper
## =========================================================================================
## see also https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
numplot <- 
  ggplot(keyplantsnum,aes(x=depth, y=totcover, group=interaction(Plant, factor(year)), col=factor(year), fill=factor(year))) +
    papertheme +
    geom_point(alpha=0.5, col='black', shape=21) +
    stat_smooth(method = 'gam',method.args = list(family = "binomial"),
                formula = y ~ s(x, bs = "cs"), medthod.args=list(weights=100), fullrange=F, se=F) +
    facet_grid(Plant~.) +
    scale_color_manual("Year", values=c('#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    scale_fill_manual("Year", values=c('#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    ylab('Macrophyte cover proportion') + xlab('Depth (cm)')  +
  guides(fill=guide_legend(nrow=2,byrow=F), color=guide_legend(nrow=2,byrow=F)) +
  ggtitle("b") + theme(plot.title = element_text(hjust = -0.05, vjust=-8))

binplot <-
ggplot(keyplants,aes(x=depth, y=totcover, group=interaction(Plant, factor(year)), col=factor(year), fill=factor(year))) +
  papertheme +
  geom_point(alpha=0.5, col='black', shape=21) +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),
              formula = y ~ s(x, bs = "cs"), fullrange=F, se=F) +
  facet_grid(Plant~.) +
  scale_color_manual("Year",values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  scale_fill_manual("Year",values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  #scale_fill_viridis()+
  #scale_color_viridis() +
  ylab('Macrophyte occurrence probability') + xlab('Depth (cm)') +
  guides(fill=guide_legend(nrow=2,byrow=F), color=guide_legend(nrow=2,byrow=F)) +
  ggtitle("a") + theme(plot.title = element_text(hjust = -0.05, vjust=-8))
# https://stackoverflow.com/questions/25401111/left-adjust-title-in-ggplot2-or-absolute-position-for-ggtitle

keyplot <- grid.arrange(binplot, numplot, ncol=2)

ggsave("../figs/plantdepths-allplots.jpg", keyplot, height=24,width=20, units='cm')  
  

## ======================================================================================
## 2019 fil alg cover
## ======================================================================================
## limit to depth where fil alg systematically recorded: < 150cm
## in 2019 survey, we had whole area where we recorded 'gunk'. no filalg there so highlight
##    these points. Also two points with NA at upper end of depth 138,143 so will remove those

algo <- noplants[noplants$depth < 150 & noplants$survey=='200pt',
                  c('date','transect','location','depth','filalg',
                                     'long','lat','substrate', 'year', 'id','gunk')]
algo <- algo[-which(is.na(algo$filalg)),]
#algae <- 
  ggplot(algo, aes(x=year, fill=filalg, group=filalg)) +
  geom_bar() +
  papertheme 

shallows <- 
  ggplot(peri, aes(x=long,y=lat)) +
  papertheme +
  geom_path(aes(group=group),color="black") +
  geom_contour(data=bath, aes(z=z), col='grey',size=0.1) + 
  geom_point(data=unique(allplants[allplants$depth<150 & allplants$survey=='200pt',
                                   c('lat','long','point','algae','gunk')]),shape=21, 
             aes(fill=factor(algae)), size=3) +
  scale_fill_manual('Algal abundance \n < 1.5m', values=c('#74c476','#31a354','#006d2c')) +
  coord_equal() + theme(legend.box='vertical', axis.title = element_blank(),
                        axis.text = element_blank(), axis.ticks = element_blank())

algplot <- grid.arrange(algae, shallows, ncol=2)

ggsave('../figs/filalg-abundance.jpg',shallows)

## ============================================================================
## percentage occurrence of chara aspera in survey?
## ==============================================================================
asp <- allplants[which(allplants$depth < 150 ),]
asp <- asp[-which(asp$substrate %in% c('BO')),]
asp <- asp[-which(asp$substrate=='boulder,stone'),]
asp <- asp[-which(asp$substrate=='stony,boulder'),]

aspsum <- ddply(asp,.(year), summarise, pasp200 = length(which(fullname=='Chara aspera'))/length(unique(point)),
             pasp=length(which(fullname=='Chara aspera'))/length(unique(id)))
aspsum$pasp[aspsum$year=='2019'] <- aspsum$pasp200[aspsum$year=='2019']

asplot <- 
ggplot(aspsum, aes(x=year, y=pasp*100)) +
  papertheme +
  geom_bar(stat = 'identity', fill='grey70') +
  ylab('% survey points with Chara aspera at < 150cm \n excluding boulder substrate') + xlab('Year') +
  scale_y_continuous(breaks = seq(0,70, by=10))

ggsave('../figs/aspera-occurrence.jpg', asplot, height = 5, width=7)

ggplot(asp[which(asp$fullname=='Chara aspera'),], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black", size=0.5) +
  geom_point(aes(fill=abundance>0), 
             shape=21, color='black', alpha=0.7) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap( ~ year, ncol=3)# +
  #scale_fill_distiller(palette = 'Greens', direction=1) +
  #scale_color_manual(values = c('black','#fdcc8a','#fc8d59','#d7301f'))
