library(ggplot2)
library(reshape2)
library(viridis)

## read in data
plants <- readRDS("../dat-mod/mal-plantsurvey-2019-plants.rds")
peri <- readRDS("../dat-mod/mal-plantsurvey-2019-perimeter.rds")
bath <- readRDS("../dat-mod/mal-plantsurvey-2019-bathy.rds")

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')


### rename '' as no plants and separate extra points from rest - for now
## FIXME 0 still a few replaced by new point places
extras <- plants[which(plants$point>201),]
plants <- plants[-which(plants$point>201 | is.na(plants$species)),]

plants$species[plants$species=='' & plants$plantcover==0 ] <- 'No plants'
plants$species[plants$species=='' & plants$plantcover==39 ] <- 'pluc' # one missing entry
## FIXME: correct missing entry in raw data
plants <- droplevels(plants)

pplot <- 
  ggplot(plants, aes(x=long,y=lat)) +
  papertheme +
  geom_path(data=peri, aes(group=group),color="black") +
  geom_point(aes(fill=plantcover), shape=21, color='black') +
  geom_contour(data=bath, aes(z=z)) + 
  facet_wrap(~species) +
  scale_fill_viridis(option='viridis')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='nflex',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_stat_density2d(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  coord_equal() + ggtitle('Nitella flexilis')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='chara1',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Chara 1')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='caspera',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Chara aspera')


ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='ecan',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Elodea canadensis')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='cglob',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Chara globularis')

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malpndf[malpndf$species=='uvul',], inherit.aes = F, aes(x=coords.x1, y=coords.x2, size=plantcover)) +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth)) +
  ggtitle('Utricularia vulgaris')

malsum <- ddply(malpndf, .(point), summarise, plantcover=sum(plantcover, na.rm=T), substrate = substrate[1],
                coords.x1=coords.x1[1], coords.x2=coords.x2[1], depth=depth[1], height=mean(plantheight))
subcodes <- data.frame(substrate = unique(malsum$substrate))
subcodes$GrainSize <- c(9,8,4,7,2,10,NA,5,3,1,6,10)
malsum <- merge(malsum, subcodes)

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malsum, inherit.aes = F, aes(x=coords.x1, y=coords.x2, col=GrainSize, size=plantcover)) +
  scale_color_viridis() +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth))
coord_equal()

ggplot(testdf) +
  papertheme +
  aes(long,lat,group=group) + 
  #geom_polygon(data=testdf, aes(long,lat,group=group)) +
  geom_path(color="black") +
  geom_point(data = malsum, inherit.aes = F, aes(x=coords.x1, y=coords.x2, col=height<20, size=GrainSize)) +
  #scale_color_viridis() +
  geom_contour(data=malpndf[-which(duplicated(malpndf$point)),], inherit.aes = F, aes(x=coords.x1, y=coords.x2, z=depth))
coord_equal()

ggplot(malsum[grep('silt', malsum$substrate),],aes(x=depth, y=plantcover)) +
  papertheme +
  geom_point(alpha=0.5) +
  stat_smooth(method = 'gam',formula = y ~ s(x, bs = "cs")) +
  ylab('Plant cover (%)') + xlab('Depth (cm)')


max(plants$depth[grep('chara', plants$species)], na.rm = T)  
