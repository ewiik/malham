##  sample point generation for malham tarn 2019 macrophyte survey
## NOTE! This script currently saves nothing; discovered that setting seed did not seem to
##    guarantee reproduction of sample points.

## get libraries
library('ggplot2')
library('reshape2')
library('viridis')
library("grid")
library('gridExtra')
library("extrafont")
library('plyr')
library("dplyr")
library('gtable')
library(cowplot)

require(sf)
require(rgdal)
loadfonts()

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

## read in data
mal <- readOGR(dsn = "../dat-orig/shapes", layer = "Malham Tarn polygon_region")

## make into gpx file
malwgs <- spTransform(mal, CRS("+proj=longlat +datum=WGS84"))
set.seed(200)
malpn <- spsample(malwgs, 200, type = 'regular', nsig=8)
malpndf <- as.data.frame(malpn)
malpndf2 <- data.frame(cbind(data.frame(id=1:200), malpndf))
#pgirmess::writeGPX(malpndf2, 'test.gpx', 'w')

## previous trials to look at how the different N would look like on Tarn
set.seed(42)
malp <- spsample(mal, 1000, type = 'regular')
set.seed(300)
malp2 <- spsample(mal, 500, type = 'regular')
set.seed(200)
malp3 <- spsample(mal, 200, type = 'regular')
set.seed(20)
malp4 <- spsample(mal, 100, type = 'regular')

