## long-term monitoring data received probably from Environment Agency but cannot remember anymore
## (received as part of PhD work); corresponds to site names used by EA-online so assuming EA

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
mal <- read.csv("../dat-orig/EA/Malham_data150909.csv")
mal$datetime <- paste(mal$DATE, mal$TIME)
mal$datetime <- as.POSIXct(mal$datetime, format="%d-%b-%y %H%M", tz="GMT")

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

## capture those cases where detection limit is likely to be active
mallessthan <- malsub
mallessthan <- as.data.frame(apply(mallessthan, 2, gsub, pattern = "<", replacement=""))

## =============================================================================================
## Nitrogen
## ====================================================================================
## what are the duplicated columns all about?
## FIXME: are the NH3s reported as mg/L N or not?
malN <- melt(malsub, id.vars = c('Site','datetime'), 
             measure.vars =c('AmmNmgL','KjelNmgL','OxNmgL','NitrateNmgL','UnionNH3mgL',
                  'NDigestedugL','NitrogenNmgL','FiltNH3NmgL','FiltOxNmgL'))
malN$lessthan <- FALSE
malN$lessthan[grep('<', malN$value)] <- TRUE
malN$value <- gsub(x=malN$value, pattern= "<",replacement= "")
malN$value <- as.numeric(malN$value)

malN$value[grep('ugL', malN$variable)] <- malN$value[grep('ugL', malN$variable)]/1000

realnames <- data.frame(variable=unique(malN$variable))
realnames$NType <- c('Ammonia/um','TN','TN','Nitrate','Ammonia','TN','TN','Nitrate','TN-filtered')
malN <- merge(malN, realnames)

#allNplot <- 
  ggplot(malN, aes(y=value, x=datetime, col=NType)) +
  papertheme +
  geom_point(aes(shape=lessthan), alpha=0.6) +
  facet_wrap(~Site, scales='free_y', ncol=1)+
  theme(legend.direction = 'vertical') + labs(shape="Below detection limit") +
  scale_color_manual(values=c('#e66101','#fdb863','black','#b2abd2','#5e3c99')) +
  scale_x_datetime(date_labels = "%b %y", date_breaks = '1 year') +
    guides(color=guide_legend(ncol=3)) +
    ylab('Nitrogen, mg/L') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plot=allNplot, file="../figs/allNdata.pdf", width=13, height=8)

## ============================================================================================
## Phosphorus
## ===========================================================================================
malP <- melt(malsub, id.vars = c('Site','datetime'), 
             measure.vars =c('OrthoPmgL','FiltOrthoPmgL','PdigestedugL','FiltOrthoPmgL2','PmgL'))
malP$lessthan <- FALSE
malP$lessthan[grep('<', malP$value)] <- TRUE
malP$value <- gsub(x=malP$value, pattern= "<",replacement= "")
malP$value <- as.numeric(malP$value)

malP$value[grep("Ortho", malP$variable)] <- po4top(malP$value[grep("Ortho", malP$variable)])
malP$value[malP$variable=="PmgL"] <- malP$value[malP$variable=="PmgL"]*1000

realnamesP <- data.frame(variable = unique(malP$variable))
realnamesP$PType <- c('OrthoP','OrthoP','TP','OrthoP','TP')

malP <- merge(malP, realnamesP)

#allPplot <- 
  ggplot(malP, aes(y=value, x=datetime, col=PType)) +
  papertheme + 
  geom_point(aes(shape=lessthan), alpha=0.6) +
  facet_wrap(~Site, scales='free_y', ncol=1) +
  ylab("P (ug/L) (converted from all cases)") +
  geom_hline(yintercept = 12) + # CSM limit for Malham Tarn-depth lakes in TP 
  theme(legend.direction = 'vertical') + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y", date_breaks = '1 year') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plot=allPplot, file="../figs/allPdata.pdf",height=8, width=13)

## ====================================================================================================
## oxygen etc.
## =================================================================================================
malOx <- melt(malsub, id.vars = c("Site","datetime"), 
              measure.vars = c("Orel","Oabs","BODmgL","Colour","OrgCmgL","Orel2" ,"Oabs2"))
malOx$lessthan <- FALSE
malOx$lessthan[grep('<', malOx$value)] <- TRUE
malOx$value <- gsub(x=malOx$value, pattern= "<",replacement= "")
malOx$value <- as.numeric(malOx$value)
malOx$variable[grep("Orel2", malOx$variable)] <- "Orel" # seem to be a continuation saved as diff name
malOx$variable[grep("Oabs2", malOx$variable)] <- "Oabs"

ggplot(malOx[malOx$variable %in% c('Orel','BODmgL'),], aes(y=value, x=datetime, col=Site)) +
  papertheme + 
  geom_path() +
  geom_point(aes(shape=lessthan), alpha=0.6) +
  ylab("Oxygen (% or mg/L)") +
  theme(legend.direction = 'vertical', axis.text.x = element_text(angle=40, hjust=1)) + labs(shape="Below detection limit") +
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17")) +
  scale_x_datetime(date_labels = "%b %y", date_breaks = "1 year") +
  facet_wrap(~variable, ncol=1, scales="free_y")

## other remaining vars
malchl <- melt(malsub, id.vars = c("Site",'datetime'), measure.vars = c("ChlAmgL" ,"ChlorophyllugL"))

ggplot(na.omit(malchl), aes(x=datetime, y=value)) +
  papertheme +
  geom_point(aes(col=Site), alpha=0.6)+
  scale_color_manual(values=c("#386cb0", "#f0027f", "#bf5b17"))

#pHplot <- 
  ggplot(malsub[-which(is.na(malsub$datetime)),], aes(y=pH, x=datetime)) +
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
saveRDS(malsub, "../dat-mod/malham-EA-decadal.rds")
saveRDS(malP, "../dat-mod/malham-EA-P-decadal.rds")
saveRDS(malN, "../dat-mod/malham-EA-N-decadal.rds")

