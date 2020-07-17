library(ggplot2)
library(metR) # contour labels
library(ggsn)
library(reshape2)
library(viridis)
library(gridExtra)
library(extrafont)
loadfonts()
library(plyr)
library(mgcv)

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

# declare list of most interesting plant groups
want <- c('Balls','Callitriche','Chara','Elodea','Characeae','Fontinalis','Nitella','No plants' ,
          'PotEut','PotLuc','Tolypella','Utricularia','Zannichellia','Moss')

## create subsets  
## ===============================================================================
## for when we might want to remove poor data
poorplants <- allplants
poorsubplants <- subplants

allplants <- allplants[-which(allplants$poor=='yes'),]
subplants <- subplants[-which(subplants$poor=='yes'),]


## csm with and without species abundance
pwant <- allplants[allplants$survey=='csm' & allplants$speciestype== 'numeric'& 
                     allplants$Plant %in% want,]
pwant2 <- allplants[allplants$survey=='csm' & allplants$Plant %in% want,]

## summarise aggregate species groups to highest abundance possible per point; 2019 and numeric csm
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
bgroups$totcoverbin[bgroups$totcover > 0] <- 1 #all surveys

groups <- rbind.fill(groups,ogroups) # only numeric surveys

# create continuous value for substrate and insert into subplants
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

## create summary for 2019 plant height
malsum <- ddply(allplants[allplants$survey=='200pt',], .(point), summarise, 
                height=mean(plantheight, na.rm = T))

malsum <- merge(malsum, 
                data.frame(unique(allplants[allplants$survey=='200pt',
                                            c('datefac','point',
                                              "date","depth" ,"substrate",
                                              'long','lat','year')])))
subcodes$substrate <- as.character(subcodes$substrate)
malsum <- merge(malsum, subcodes[subcodes$survey=='200pt',])

## plant cover vs depth excluding bouldery substrate
ndistro <- subplants[which(subplants$GrainSize<9 & subplants$survey=='200pt'),]
bdistro <- subplants[which(subplants$GrainSize<6 & subplants$GrainSize != -99 & 
                             subplants$survey=='csm'),]
distro <- rbind(ndistro, bdistro) 


## =============================================================================================
## plot with survey points in 2019 and bathymetry; plus a 'mean transect' of CSM 
## ====================================================================================
samplingplot <- 
ggplot(alltrans[alltrans$year == 2009,], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  stat_contour(data=bath, aes(z=z/100), color='grey70')  + 
  geom_text_contour(data=bath, aes(z=z/100), stroke = 1, check_overlap = T) +
  geom_point(data=subplants[subplants$year==2019,], aes(color=year)) +
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
  ggplot(subplants, aes(x=depth, group=year, col=year)) +
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


ggplot(pwant, aes(x=long,y=lat)) +
    papertheme +
  geom_contour(data=bath, aes(z=z)) + 
  geom_path(data=peri, aes(group=group),color="black") +
    geom_point(aes(fill=factor(abundance)), shape=21, color='black',size=2, alpha=.7) +
     facet_wrap(fullname~datefac, labeller = function(labs) {label_value(labs, multi_line = FALSE)},
               ncol=8) + 
    scale_fill_manual(values = c('#fdcc8a','#fc8d59','#d7301f','black')) +
  scale_color_manual(values=c('transparent','black'))

ggplot(pwant2, aes(x=long,y=lat)) +
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

## plant height vs sediment; 2019
ggplot(malsum) +
  papertheme +
  geom_path(data=peri, aes(long,lat,group=group) ,color="black") +
  geom_point(aes(x=long, y=lat, color=height > 20, size=GrainSize)) +
  #scale_color_viridis() +
  geom_contour(data=bath, inherit.aes = F, aes(x=long, y=lat, z=z)) +
  coord_equal()

heightplot <- ggplot(malsum) +
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
abplot <- 
ggplot(distro, aes(x=year, group=biomass_scaled, fill=biomass_scaled)) +
  papertheme +
  geom_bar(position = 'fill') +
  scale_fill_distiller('Scaled plant cover',palette = 'Greens', direction = 1) +
  ylab('Survey points ex cobbles and boulders') + xlab('Year')

## occurrence of plants in areas where we expect them (ie remove bouldery points)
ocplot <- 
ggplot(distro, aes(x=year, group=biomass_scaled >0, fill=biomass_scaled>0)) +
  papertheme +
  geom_bar(position = 'fill') +
  scale_fill_manual('Plant occurrence',values=c('#a6611a','#018571'), labels=c(0,1)) +
  ylab('Survey points ex cobbles and boulders') + xlab('Year')

plantpts <- grid.arrange(abplot, ocplot, ncol=1)

ggsave('../figs/plant-biomass.jpg',plantpts, height=7, width=7)

# quick proportional test on whether significantly different
ptable <- table(distro$year, distro$biomass>0)
summary(prop.test(x = ptable))

summary(lm(biomass >0 ~ factor(year), data=distro))

## plot distribution likelihood for key taxa
plist <- list('Chara','Nitella','Elodea','Fontinalis')

## make data frames that exclude boulders and are separate for each data type
cplants <- bgroups[!is.na(bgroups$id),]
nplants <- bgroups[is.na(bgroups$id),]

cplants <- cplants[cplants$id %in% bdistro$id,]
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

#abdepth <-  # !!! key figure for paper see also https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
  ggplot(keys[!keys$year %in% c(2004,2005),],aes(x=depth, y=totcover/100, group=year, col=year, fill=year)) +
  papertheme +
  geom_point(alpha=0.5, col='black', shape=21) +
  stat_smooth(method = 'gam',method.args = list(family = "binomial"),
              formula = y ~ s(x, bs = "cs",k=4), fullrange=F, se=F) +
  facet_wrap(~Plant) +
    scale_color_manual(values=c('#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    scale_fill_manual(values=c('#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
  ylab('Macrophyte cover proportion') + xlab('Depth (cm)') #+ ylim(c(0,100))

  ## FIXME: check why gams slightly different now, something in code didn't run. there was somthing cdistro wasn't found and had to replace with bdistro
keymelt <- melt(keys, id.vars = c('point','Plant','year','depth'), measure.vars = c('totcover','totcoverbin'))  
keymelt$value[keymelt$variable=='totcover'] <- keymelt$value[keymelt$variable=='totcover']/100 
keymelt$value[keymelt$variable=='totcover' & keymelt$year %in% c(2004, 2005)] <- NA
keymelt$variable <- factor(keymelt$variable, labels=c('Abundance','Occurrence'))  
keymelt$Plant <- factor(keymelt$Plant, levels = c('Chara','Nitella','Elodea','Moss'))

kgam <- gam(value ~ s(depth, by=c(Plant, year, variable)), family='binomial', data=keymelt )  
ggplot(keymelt,aes(x=depth, y=value, group=interaction(Plant, year, variable), col=Plant, fill=Plant)) +
    papertheme +
    geom_point(alpha=0.5, col='black', shape=21) +
    stat_smooth(method = 'gam',method.args = list(family = "binomial"),
                formula = y ~ s(x, bs = "cs",k=4), fullrange=F, se=F) +
    facet_grid(year~variable) +
    scale_color_manual(values=c('#1f78b4','#a6cee3','#b2df8a','#33a02c')) +
    scale_fill_manual(values=c('#1f78b4','#a6cee3','#b2df8a','#33a02c')) +
    ylab('Macrophyte cover proportion') + xlab('Depth (cm)') #+ ylim(c(0,100))
  
  occdepth <-
  ggplot(keys,aes(x=depth, y=totcoverbin, group=year, col=year, fill=year)) +
    papertheme +
    geom_point(alpha=0.5, col='black', shape=21) +
    stat_smooth(method = 'gam',method.args = list(family = "binomial"),
                formula = y ~ s(x, bs = "cs",k=4), fullrange=F, se=F) +
    facet_wrap(~Plant) +
    scale_color_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    scale_fill_manual(values=c('#8c510a','#d8b365','#f6e8c3','#c7eae5','#5ab4ac','#01665e')) +
    ylab('Macrophyte occurrence') + xlab('Depth (cm)') #+ ylim(c(0,100))
  ## FIXME: make sure na depth 75 becomes 75 or 80

depths <- grid.arrange(abdepth, occdepth, ncol=1)    
ggsave('../figs/species-depths.jpg', depths, width=8, height=11)

## ======================================================================================
## 2019 fil alg cover
## ======================================================================================
## limit to depth where fil alg systematically recorded: < 150cm
## in 2019 survey, we had whole area where we recorded 'gunk'. no filalg there so highlight
##    these points. Also two points with NA at upper end of depth 138,143 so will remove those

algo <- subplants[subplants$depth < 150 & subplants$survey=='200pt',
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
asp <- asp[-which(asp$substrate %in% c('BO','CO')),]
asp <- asp[-which(asp$substrate=='boulder,stone'),]
asp <- asp[-which(asp$substrate=='stony,boulder'),]

aspsum <- ddply(asp,.(year), summarise, pasp200 = length(which(fullname=='Chara aspera'))/length(unique(point)),
             pasp=length(which(fullname=='Chara aspera'))/length(unique(id)))
aspsum$pasp[aspsum$year=='2019'] <- aspsum$pasp200[aspsum$year=='2019']

asplot <- 
ggplot(aspsum, aes(x=year, y=pasp*100)) +
  papertheme +
  geom_bar(stat = 'identity') +
  ylab('% survey points with Chara aspera at < 150cm \n excluding boulder substrate') + xlab('Year')

ggsave('../figs/aspera-occurrence.jpg', asplot)

ggplot(asp[which(asp$fullname=='Chara aspera'),], aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black", size=0.5) +
  geom_point(aes(fill=abundance>0), 
             shape=21, color='black', alpha=0.7) +
  geom_contour(data=bath, aes(z=z), color='black') + 
  facet_wrap( ~ year, ncol=3)# +
  #scale_fill_distiller(palette = 'Greens', direction=1) +
  #scale_color_manual(values = c('black','#fdcc8a','#fc8d59','#d7301f'))
