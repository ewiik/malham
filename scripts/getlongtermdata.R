## long-term monitoring data received probably from Environment Agency but cannot remember anymore
## (received as part of PhD work)

##load necessary packages
library('reshape2')
library('ggplot2')
library('extrafont')

loadfonts()

## nice ggplot theme
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

## create function that converts mg L PO4 to ug L P.
po4top <- function(x) {Pmass <- 30.974
Omass <- 15.999
pfrac <- Pmass/(Pmass+Omass*4)
x <- pfrac*x*1000}



## read in files and make date interpretable
mal <- read.csv("../data/Malham_data150909.csv")
mal$datetime <- paste(mal$DATE, mal$TIME)
mal$datetime <- as.POSIXct(mal$datetime, format="%d-%b-%y %H%M", tz="GMT")

if(!file.exists("../data/emmatp.csv")) {
  download.file(url="https://ndownloader.figshare.com/files/5666586", destfile="../data/emmatp.csv")
 }
malemma <- read.csv("../data/emmatp.csv", sep = "\t", row.names = 1)
malemma <- as.data.frame.matrix(t(malemma))
malemma$Month <- sprintf("%02d", match(substr(rownames(malemma), 1,3), tolower(month.abb)))
malemma$Year <- 2000 + as.numeric(substr(rownames(malemma),4,5))
malemma$DateTime <- paste0(malemma$Month, malemma$Year)
malemma$DateTime <- as.POSIXct(paste0(malemma$datetime,"01"), format="%m%Y%d") ## FIXME; need days of
##    my samplings... not tidy in thesis meta..=(
malemma <- data.frame(Site="Malham Tarn", melt(malemma, id.vars = "DateTime", measure.vars = "Mal"))
malemma$lessthan <- FALSE
malemma$Measure <- "TP"

## change column names to sensible options;
## FIXME: what is BOD ATU? what is PV.N.80.4Hrs..mg.l, how is oxidised N produced and how is orgC produced
##    SiO2.Rv.Filt..mg.l, DtrgtAncSyn..mg.l, Dtrgt.NncSyn..mg.l, 
##    WethPresTemp..UNITLESS" "X1183..WethPresPrec..UNITLESS, Liqcolour.st..UNITLESS" "X6517..
##    Liqcolour.mn..UNITLESS" "X6518..Liqcolour.se..UNITLESS" etc etc
oldnames <- names(mal)
names(mal) <- c('URN','Site', 'DATE','TIME','MAT','LAB.NO','PURP','pH','Cond20uS','Colour','TempWater',
                'Cond25uS','Orel','Oabs', 'BODmgL','BOD5daymgL','CODO2mgL','PVN','OrgCmgL','AmmNmgL','KjelNmgL',
                'OxNmgL','NitrateNmgL','NitriteNmgL','UnionNH3mgL','SuspSol105mgL','HardnessmgL','Alk4.5',
                'ChloridemgL','OrthoPmgL', 'SiO2mgL','SO4mgL','FiltOrthoPmgL','NamgL','KmgL',
                'MgmL','CamgL','OrgCFiltmgL','PmgL','AncSyn','NncSyn','Oil','ChlAmgL','GlycolsmgL',
                'OscillatoriaNOml','WPTemp','WPPresc','AphanizomenonNOml','GomphosphaerNOml','LyngbyaNOml',
                'PhormidiumNOml','ColeosphaerNOml','TurbidityNTU','AnabaenaBNOml','MicrocystisNOml',
                'CuFiltugL','ZnugL','LiqColSt','LiqColMn','LiqColse','SolCol','SolQuan','SolText','OdourNat',
                'OdourSt','WethVisib','Weth7Prec','Weth7Temp','Weth7Vsy','LASmgL','NDigestedugL','PhenolYesNo',
                'PdigestedugL','ChlorophyllugL','ChlorAugL','Flow','NonylPhenolugL','FoamYesNo','NitrogenNmgL',
                'FiltOrthoPmgL2','Orel2','Oabs2','FiltOxNmgL','FiltNH3NmgL','datetime')
malsub <- mal[,c('Site', 'DATE','TIME','pH','Cond20uS','Cond25uS','Orel','Oabs', 'BODmgL','TempWater','Colour',
                 'OrgCmgL','AmmNmgL','KjelNmgL',
                 'OxNmgL','NitrateNmgL','NitriteNmgL','UnionNH3mgL','SuspSol105mgL','HardnessmgL','Alk4.5',
                 'OrthoPmgL', 'SiO2mgL','SO4mgL','FiltOrthoPmgL','TurbidityNTU','NDigestedugL','ChlAmgL',
                 'ChlorophyllugL','PdigestedugL','NitrogenNmgL','PmgL',
                 'FiltOrthoPmgL2','Orel2','Oabs2','FiltOxNmgL','FiltNH3NmgL','datetime')]
malsub$Alk4.5 <- as.character(malsub$Alk4.5)
malsub$Alk4.5 <- gsub(x=malsub$Alk4.5, pattern= "<",replacement= "")
malsub$Alk4.5 <- as.numeric(malsub$Alk4.5)


mallessthan <- malsub
mallessthan <- as.data.frame(apply(mallessthan, 2, gsub, pattern = "<", replacement=""))

## what are the duplicated columns all about?
## FIXME: are the complicated Ns reported as mg/L N or not?
malN <- melt(malsub, id.vars = c('Site','datetime'), 
             measure.vars =c('AmmNmgL','KjelNmgL','OxNmgL','NitrateNmgL','UnionNH3mgL',
                  'NDigestedugL','NitrogenNmgL','FiltNH3NmgL','FiltOxNmgL'))
malN$lessthan <- FALSE
malN$lessthan[grep('<', malN$value)] <- TRUE
malN$value <- gsub(x=malN$value, pattern= "<",replacement= "")
malN$value <- as.numeric(malN$value)

malN$value[grep('ugL', malN$variable)] <- malN$value[grep('ugL', malN$variable)]/1000

allNplot <- ggplot(malN, aes(y=value, x=datetime, col=Site)) +
  papertheme +
  geom_point(aes(shape=lessthan), alpha=0.6) +
  facet_wrap(~variable, scales='free_y')+
  theme(legend.direction = 'vertical') + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y")
ggsave(plot=allNplot, file="../figs/allNdata.pdf", width=13, height=8)

malP <- melt(malsub, id.vars = c('Site','datetime'), 
             measure.vars =c('OrthoPmgL','FiltOrthoPmgL','PdigestedugL','FiltOrthoPmgL2','PmgL'))
malP$lessthan <- FALSE
malP$lessthan[grep('<', malP$value)] <- TRUE
malP$value <- gsub(x=malP$value, pattern= "<",replacement= "")
malP$value <- as.numeric(malP$value)

malP$value[grep("Ortho", malP$variable)] <- po4top(malP$value[grep("Ortho", malP$variable)])
malP$value[malP$variable=="PmgL"] <- malP$value[malP$variable=="PmgL"]*1000
allPplot <- ggplot(malP, aes(y=value, x=datetime, col=Site)) +
  papertheme + 
  geom_point(aes(shape=lessthan), alpha=0.6) +
  facet_wrap(~variable, scales='free_y', ncol=2) +
  ylab("P (ug/L) (converted from all cases)") +
  geom_hline(yintercept = 15) + # CSM limit for Malham Tarn-depth lakes in TP 
  theme(legend.direction = 'vertical') + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y")
ggsave(plot=allPplot, file="../figs/allPdata.pdf",height=8, width=13)

## select most sensible nutrient variables for plotting:
## N: NitrateNmgL, NdigestedugLOXNmgL, AmmNmgL
## P: Pdigested, pmgl (assume comparable for now FIXME)
malP$Measure <- "TP"
malP$Measure[grep("Ortho", malP$variable)]<- "OrthoP"
malP <- rbind(malP, malemma)

subPplot <-
  ggplot(na.omit(malP[malP$Measure=="TP",]), aes(y=value, x=datetime, col=Site)) +
  papertheme + 
  geom_point(aes(shape=lessthan), alpha=0.6) +
  ylab("P (ug/L) (converted from all cases)") +
  geom_hline(yintercept = 15) + # CSM limit for Malham Tarn-depth lakes in TP 
  theme(legend.direction = 'vertical', axis.text.x = element_text(angle=90)) + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y", date_breaks = "3 months")
ggsave(plot=subPplot, file="../figs/subPdata.pdf",height=8, width=13)

malNsub <- malN[which(malN$variable %in% c("NitrateNmgL", "NdigestedugL", "OxNmgL", "AmmNmgL")),]
malNsub$Measure <- "TN"
malNsub$Measure[grep("Amm", malNsub$variable)] <- "Ammonium-N"
malNsub$Measure[grep("Nitrat", malNsub$variable)] <- "Nitrate-N"
malNsub$Measure[grep("Ox", malNsub$variable)] <- "Oxidised N"

subNplot <-
  ggplot(na.omit(malNsub), aes(y=value, x=datetime, col=Site)) +
  papertheme + 
  geom_point(aes(shape=lessthan), alpha=0.6) +
  ylab("N (mg/L) (converted from all cases)") +
  theme(legend.direction = 'vertical', axis.text.x = element_text(angle=90)) + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y", date_breaks = "3 months") +
  facet_wrap(~Measure, ncol=1)
ggsave(plot=subNplot, file="../figs/subNdata.pdf",height=8, width=13)

## oxygen etc.
malOx <- melt(malsub, id.vars = c("Site","datetime"), 
              measure.vars = c("Orel","Oabs","BODmgL","Colour","OrgCmgL","Orel2" ,"Oabs2"))
malOx$lessthan <- FALSE
malOx$lessthan[grep('<', malOx$value)] <- TRUE
malOx$value <- gsub(x=malOx$value, pattern= "<",replacement= "")
malOx$value <- as.numeric(malOx$value)
malOx$variable[grep("Orel2", malOx$variable)] <- "Orel" # seem to be a continuation saved as diff name
malOx$variable[grep("Oabs2", malOx$variable)] <- "Oabs"

ggplot(malOx, aes(y=value, x=datetime, col=Site)) +
  papertheme + 
  geom_point(aes(shape=lessthan), alpha=0.6) +
  ylab("N (mg/L) (converted from all cases)") +
  theme(legend.direction = 'vertical', axis.text.x = element_text(angle=90)) + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y", date_breaks = "3 months") +
  facet_wrap(~variable, ncol=1, scales="free_y")

## other remaining vars
malchl <- melt(malsub, id.vars = c("Site",'datetime'), measure.vars = c("ChlAmgL" ,"ChlorophyllugL"))

ggplot(na.omit(malchl), aes(x=datetime, y=value)) +
  papertheme +
  geom_point(aes(col=Site), alpha=0.6)+
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17"))

pHplot <- ggplot(malsub[-which(is.na(malsub$datetime)),], aes(y=pH, x=datetime)) +
  papertheme +
  geom_point(aes(col=Site),alpha=0.6) +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  facet_wrap(~format(datetime, "%m")) +
  theme(legend.direction = 'vertical', axis.text.x = element_text(angle=90)) +
  scale_x_datetime(date_labels = "%y", date_breaks = "1 years") +
  xlab("")
ggsave(plot=pHplot, file="../figs/pHdata.pdf", width=8, height=8)  

inflowdat <- subset(malsub, Site=="TARN BECK AT ENTRANCE TO MALHAM TARN")
ggplot(inflowdat[-which(is.na(inflowdat$datetime)),], aes(y=pH, x=datetime)) +
  papertheme +
  geom_point() +
  #scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  #facet_wrap(~format(datetime, "%m")) +
  theme(legend.direction = 'vertical', axis.text.x = element_text(angle=90)) +
  scale_x_datetime(date_labels = "%y", date_breaks = "1 years") +
  xlab("Year")


ggplot(malsub[-which(is.na(malsub$datetime)),], aes(y=Alk4.5, x=datetime)) +
  papertheme +
  geom_point(aes(col=Site),alpha=0.6) +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  facet_wrap(~format(datetime, "%m"))

ggplot(malsub[-which(is.na(malsub$datetime)),], aes(y=pH, x=datetime)) +
  papertheme +
  geom_point(aes(size=Alk4.5),alpha=0.6)  +
  facet_wrap(~Site, ncol=1)

## save some processed data
saveRDS(malP, "../data/dat-mod/malP-decadal.rds")
saveRDS(malN, "../data/dat-mod/malN-decadal.rds")

saveRDS(malemma, "../data/dat-mod/malTP-wiik.rds")
