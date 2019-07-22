## this is a starting script for JP and EW to start plotting long term trends

## read in data
if (!file.exists("../data/malham.rds")) {
  source("getmonitoringdata.R")
}
maldf <- readRDS("../data/malham.rds")

if (!file.exists("../data/dat-mod/allPhosphorus.rds")) {
  source("getsondedata.R")
}
alltp <- readRDS("../data/dat-mod/allPhosphorus.rds")
allsonde <- readRDS("../data/dat-mod/allSondes.rds")

## load packages
library("ggplot2")
library("viridis")
library("gridExtra")
library("extrafont")
library('plyr')

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

## ===================================================================================================
## Auto-downloaded data: 
## subset to interest vars and/or those with satisfactory data availability
datamuch <- ddply(maldf, "var", summarise, length=length(value))
datamuch <- datamuch[datamuch$length > 30,]
datamuch[order(datamuch$length, decreasing=TRUE),]

want <- datamuch$var[datamuch$length > 350]
malsub <- maldf[maldf$var %in% want,]

## change Site to something better
levels(malsub$Site)
levels(malsub$Site) <- c("Goredale Beck", "Malham Beck BH", "Malham Beck MC", "Malham Final",
                         "River Aire", "Tarn Beck", "Malham Tarn FSC", "Malham Tarn", 
                         "Malham Beck STW")
## plit
ggplot(malsub, aes(y=value, x=DateTime, col=Site)) +
  papertheme +
  geom_point(size=0.5, alpha=0.5) +
  facet_wrap(~var, scales = "free_y")

##select most interesting ones
vars <- as.character(unique(malsub$var)) # easier to copy paste what is wanted for next step
varwant <- c("pH","Temp Water", "Cond @ 25C", "BOD ATU", "Nitrate-N", "Sld Sus@105C", "Alky pH 4.5",
             "Orthophospht", "O Diss %sat" ,"Calcium - Ca", "Cu Filtered",  "Zinc - as Zn" )

## plit
## my lazy labeler fix
mylabel_parsed <- function (labels, multi_line = FALSE) 
{
  labels <- label_value(labels, multi_line = multi_line)
  if (multi_line) {
    lapply(unname(labels), lapply, function(values) {
      c(parse(text = as.character(values)))
    })
  }
  else {
    lapply(labels, function(values) {
      #values <- paste0("list(", values, ")")
      values <- paste0(values)
      #lapply(values, function(expr) c(parse(text = expr)))
      lapply(values, function(expr) c(as.character(expr)))
    })
  }
}
malsub$SiteCode <- malsub$Site
levels(malsub$SiteCode) <- abbreviate(levels(malsub$SiteCode))

malsub$varCode <- malsub$var
levels(malsub$varCode) <- abbreviate(levels(malsub$varCode))
fills <- data.frame(Site = unique(malsub$Site))

timeplot <- ggplot(malsub[malsub$var %in% varwant,], aes(y=value, x=DateTime, col=Site)) +
  papertheme +
  geom_point(size=0.5, alpha=0.8) +
  facet_wrap(SiteCode~varCode, scales = "free_y",labeller = mylabel_parsed, dir="h") + 
  theme(strip.background = element_rect(fill="white", colour = "white")) +
  guides(col=guide_legend(nrow=2, override.aes = list(alpha=1, size=2))) 
  
ggplot(malsub[malsub$var %in% varwant,], aes(y=value, x=DateTime, col=Site)) +
  papertheme +
  geom_point(size=0.5, alpha=0.5) +
  facet_wrap(~var, scales = "free_y")

ggsave("../figs/malsites-time.pdf", timeplot, width = 18, height = 9)

## where are the locations
loca <- as.data.frame(unique(malsub[,c('Site', 'Easting', 'Northing')]))
ggplot(malsub, aes(y=Northing, x=Easting)) +
  geom_point(aes(col=Site)) #+
 # geom_text(data=loca, aes(label=Site))
## FIXME: these don't seem to make sense..

## ===================================================================================================
## Liam's data etc. including TP
TPplot <- ggplot(alltp[!is.na(alltp$DateTime),], aes(x=factor(format(DateTime, format = '%y-%m-%d')), 
                                                     y=P, col=Measure)) +
  papertheme +
  geom_point(aes(pch=Site)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 12) +
  ylab("P, ug/L") + xlab("Time")

chlplot <- ggplot(allsonde, aes(y=value, x=DateTime, col=Sonde)) +
  papertheme +
  geom_point(size=0.5, alpha=0.3) +
  facet_wrap(~variable, scales='free_y') +
  guides(size=guide_legend(override.aes = list(size=3)), colour=guide_legend(override.aes = list(alpha=1, size=3)))+
  theme(axis.text.x = element_text(angle=45, hjust=1))

phdat <- allsonde[allsonde$variable=="pH",]
phdat$Site <- "Inflow"
phdat$Site[phdat$Sonde=="Outflow"] <- "Outflow"
ggplot(phdat, aes(y=value, x=DateTime, col=Site)) +
  papertheme +
  geom_point(size=0.5, alpha=0.3) 
  #facet_wrap(~Site,ncol=1) #+
  #guides(size=guide_legend(override.aes = list(size=3)), colour=guide_legend(override.aes = list(alpha=1, size=3)))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(malsub[malsub$Site=="RIVER AIRE AT MALHAM TARN" & !is.na(malsub$datetime),], aes(y=pH, x=datetime, col=Site)) +
  papertheme +
  geom_point(size=0.8, alpha=0.8, aes(col=format(datetime,"%m"))) +
  facet_wrap(~format(datetime,"%m"))

chldat <- allsonde[allsonde$variable=="Chl",]
ggplot(chldat, aes(y=value, x=DateTime)) +
  facet_wrap(~format(chldat$DateTime, "%H")) +
  geom_point()
ggplot(chldat, aes(y=value, x=DateTime)) +
  geom_point()+
  ylim(c(0,100))

oxdat <- allsonde[allsonde$variable=="DOrel",]
ggplot(oxdat, aes(y=value, x=DateTime)) +
  facet_wrap(~format(oxdat$DateTime, "%H")) +
  geom_point()
oxsum <- ddply(oxdat,.(Month, Sonde, Year), summarise, meanOx=mean(value, na.rm = TRUE))
ggplot(oxsum, aes(y=meanOx, x=Month, col=Sonde)) +
  geom_point() +
  papertheme +
  ylab("Mean monthly % oxygen")#+
  #facet_wrap(~Sonde, ncol=1)

ggsave(plot=chlplot,filename= "../figs/time-sondes.png", width=15, height=10)
ggsave(plot=TPplot, filename="../figs/time-tp.png")
