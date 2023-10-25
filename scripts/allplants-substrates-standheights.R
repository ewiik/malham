## malham sediment inspections

## load libraries
library(ggplot2)
library("viridis")
library("gridExtra")
library("extrafont")
library('plyr')

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

## read files
if(!file.exists("../dat-mod/all-plant-surveys.rds")) {
  source("combine-plant-surveys.R")
}
dat <- readRDS("../dat-mod/all-plant-surveys.rds")

## check dates are those we want
unique(dat$date) # yes

unique(dat$substrate)

## take away 2019 because it's got different substrate records
dat19 <- dat[dat$year==2019,]

dat <- dat[dat$year!=2019,]
dat <- droplevels(dat)

## reduce to one data point per sample point
datred <- dat[-which(duplicated(dat[,c('transect','location','point','datefac')])),]

dat19red <- dat19[-which(duplicated(dat19[,c('point')])),]

## plot proportions of sediment across sampling points
ggplot(datred, aes(fill=substrate, x=datefac))+
  geom_bar(stat='count', position='stack')

## plot plant height of 2019
ptplot <-
  ggplot(dat19red, aes(x=plantheight)) +
  stat_bin(breaks=c(-5,0.00001, 10, 50, 100,150,200,230), closed='left', fill='grey70', color='black') +
  papertheme +
  xlab("Averaged stand height per sample point (cm). Bin of no plants, i.e. height 0, \n is expanded below zero for visibility") +
  ylab("Count of survey points in bin")

ggsave("../figs/2019-height-hist.jpg", height = 13, width=17, units='cm')
