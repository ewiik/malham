## summarise macrophyte assemblage by 2014 LMNI for all years of data

library(tidyverse)
library(ggplot2)
library(reshape2)

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')


dat <- read.csv("../dat-orig/macrophytes-table.csv")
dat[is.na(dat)] <- 0
names(dat)[1] <- 'Date'

lmni <- read.csv("../dat-orig/macrophytes-lmni.csv")
names(lmni)[1] <- 'Species'
lmni$Species <- gsub(" ", ".", lmni$Species)
lmni$Species <- gsub("/", ".", lmni$Species, fixed = T)
lmni$Species <- gsub("?", ".", lmni$Species, fixed=T)

dlong <- melt(dat, id.vars = "Date", variable.name = "Species")

dlong <- merge(dlong, lmni)

abbnames <- data.frame(Species=unique(dlong$Species))
abbnames$SpeciesAbb <- c("A-linn","Ca-cf-ham","Ca-cf-pl","Ca-stag",'Ch-asp',"Ch-cont","Ch-glob",
                         "Ch-spp.","Ch-vir","Ch-vul","E-can","F-ant","Hi-vul","Hy-scor","L-unif",
                         "M-spic", "N-flex|op", "P-alp","P-ber","P-cri","P-luc","P-nat","P-pec",
                         "P-perf","P-poly","P-prae","P-x-nit","P-x-ziz","Sph-spp.","T-glom","U-vul-agg.",
                         "Z-pal")

dlong <- merge(dlong, abbnames)

dlong$Species <- fct_reorder(dlong$Species, dlong$LMNI, min)
dlong$SpeciesAbb <- fct_reorder(as.factor(dlong$SpeciesAbb), dlong$LMNI, min)

sptable <- 
  ggplot(dlong, aes(x=SpeciesAbb, y=as.factor(Date), fill=LMNI, alpha=value)) +
  papertheme +
  geom_tile(color='black') +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_alpha(guide="none") + ylab("Year") + xlab("Species (abbreviated)") +
  scale_fill_continuous(guide = guide_colorbar(barwidth=20))

ggsave("../figs/species-lmni.jpg",sptable, width=13, height=8)
ggsave("../figs/species-lmni.pdf",sptable, width=17, height=10, units = "cm")

