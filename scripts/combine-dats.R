## join most interesting data for malham presentation

## online grab data: take only malham tarn, tarn beck, river aire and malham tarn fsc, 
##    select nitrate-N, Total N oxidised, orthophosphate-P (all in elemental mg/L)
ongrab <- readRDS("../dat-mod/malham-EA.rds") # 2000 to 2019
levels(ongrab$Site)
levels(ongrab$Site) <- c("Goredale Beck", "Malham Beck BH", "Malham Beck MC", "Malham Final",
                         "River Aire", "Tarn Beck", "Malham Tarn FSC", "Malham Tarn", 
                         "Malham Beck STW","Malham Beck NB")

#ongrab <- subset(ongrab, Site %in% c("Malham Tarn FSC","Tarn Beck","Malham Tarn","River Aire") &
 #                  var %in% c("N Oxidised","Nitrate-N","Orthophospht"))

ongrab <- subset(ongrab, Site %in% c("Malham Tarn FSC","Tarn Beck","Malham Tarn","River Aire") &
                   var %in% c("N Oxidised","Nitrate-N","Orthophospht",
                              'P Digested'  ,'Phosphorus-P' , 'OrthophsFilt'))
# why not P Digested  ,Phosphorus-P OrthophsFilt

alldat <- subset(ongrab, select=c("Site","DateTime","var","value"))
alldat$Measure <- "Nitrate-N"
alldat$Measure[alldat$var == "N Oxidised"] <- "TN"
alldat$Measure[alldat$var =="Orthophospht"] <- "Phosphate-P"
alldat$Measure[alldat$var =="P Digested"] <- "TP-PDigested"
alldat$Measure[alldat$var =="Phosphorus-P"] <- "TP-PhosphorusP"
alldat$Measure[alldat$var =="OrthophsFilt"] <- "Phosphate-P"


alldat <- alldat[,-which(names(alldat) == "var")]
alldat$Site <- as.character(alldat$Site)
alldat$value[alldat$Measure == "Phosphate-P"] <- alldat$value[alldat$Measure == "Phosphate-P"]*1000

## data from Liam: recall allTP is now as ug/L P and is based on autosamplers, Hach, and unknown
##    NE methods (the detection limit stuff); no TP from Malham Tarn itself.
if (!file.exists("../dat-mod/NE-Notts-TP.rds")) {
  source("get-NE-Notts-TPdata.R")
}
alltp <- readRDS("../dat-mod/NE-Notts-TP.rds")
#allsonde <- readRDS("../data/dat-mod/allSondes.rds")

names(alltp)[names(alltp)=="P"] <- "value"
alltp <- alltp[,c("Site", "DateTime","value","Measure")]
alltp[,c(1,4)] <- apply(alltp[,c(1,4)],2, as.character)

alldat <- rbind(alldat, alltp)
alldat$lessthan <- NA

## get decadal data
malP <- readRDS("../dat-mod/malham-EA-P-decadal.rds") # 1970s to 2009
malN <- readRDS("../dat-mod/malham-EA-N-decadal.rds")
 
malN <- subset(malN, variable=='NitrateNmgL' | variable=='NDigestedugL') 
malN$measure <- ifelse(malN$variable=="NDigestedugL","TN",'Nitrate-N')
malN$Site <-as.character(malN$Site)
malN$Site[grep("BECK", malN$Site)] <- "Tarn Beck"
malN$Site[grep("AIRE", malN$Site)] <- "River Aire"
malN$Site[grep("TARN", malN$Site)] <- "Malham Tarn"
names(malN)[names(malN)=="datetime"] <- "DateTime"
names(malN)[names(malN)=="measure"] <- "Measure"
malN <- malN[,c("Site", "DateTime", "value", "Measure", "lessthan")]

alldat <- rbind(alldat, malN)

malP <- subset(malP, variable=="PdigestedugL" | variable=="PmgL") # only ones that can reflect
#   TP
malP$Site <-as.character(malP$Site)
malP$Site[grep("BECK", malP$Site)] <- "Tarn Beck"
malP$Site[grep("AIRE", malP$Site)] <- "River Aire"
malP$Site[grep("TARN", malP$Site)] <- "Malham Tarn"
names(malP)[names(malP)=="datetime"] <- "DateTime"
names(malP)[names(malP)=="variable"] <- "Measure"

malP <- malP[,c("Site", "DateTime", "value", "Measure", "lessthan")]

alldat <- rbind(alldat, malP)

## my PhD data
malemma <- readRDS("../dat-mod/malham-wiik-TP.rds")
malemma <- malemma[,c("Site", "DateTime", "value", "Measure", "lessthan")]

alldat <- rbind(alldat, malemma)

## make all nice for plot
alldat$Measure <- as.factor(alldat$Measure)
alldat$Site <- as.factor(alldat$Site)
alldat$lessthan[is.na(alldat$lessthan)] <- FALSE
alldat$lessthan[alldat$Measure=="TP-DT20"] <- TRUE
alldat$Measure[alldat$Measure=="TP-DT20"] <- "TP"
alldat$Site[alldat$Site=="Outflow"] <- "River Aire"
alldat$Site[alldat$Site == "Inflow"] <- "Tarn Beck"
alldat <- droplevels(alldat)

alldat$value[alldat$Measure=="Phosphate-P" & alldat$value <1 & !is.na(alldat$value)] <- 
  alldat$value[alldat$Measure=="Phosphate-P" & alldat$value <1& !is.na(alldat$value)]*1000

alldat <- alldat[complete.cases(alldat),]
alldat <- alldat[-which(alldat$Site=="Malham Tarn FSC"),]

## plot
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

Ndat <- alldat[grep("N", alldat$Measure),]
Ndat$Site <- relevel(Ndat$Site, ref = 4)
Nplot <- ggplot(Ndat[Ndat$Site!="Malham Tarn",], aes(x=DateTime, y=value)) +
  papertheme +
  facet_wrap(Site~Measure, ncol=1, labeller = mylabel_parsed) +
  geom_point(aes(colour=lessthan), alpha=0.6) +
  geom_hline(yintercept = 0.5) + ylab("N (mg/L)") +
  scale_color_manual(name="Below detection limit", values=c("#4575b4","#d73027")) +
  scale_x_datetime(date_breaks = '1 year', date_labels = "%y")

Pdat <- alldat[grep("P", alldat$Measure),]
Pdat$Site <- relevel(Pdat$Site, ref = 4)
Pdat$value[Pdat$Measure=='TP-PhosphorusP'] <- Pdat$value[Pdat$Measure=='TP-PhosphorusP']*1000

#Pplot <- 
  ggplot(Pdat[Pdat$Site=='Malham Tarn',], aes(x=DateTime, y=value, group=Site)) +
  papertheme +
  facet_wrap(~Measure, ncol=1,  scales='free_y',labeller = mylabel_parsed) +
  geom_point(aes(colour=lessthan), alpha=0.6) +
  geom_hline(yintercept = 12) + ylab("P (ug/L)") +
  #ylim(c(0,100)) +
  scale_color_manual(name="Below detection limit", values=c("#4575b4","#d73027")) +
  scale_x_datetime(date_breaks = '1 year', date_labels = "%y")
 
oneplot <-   ggplot(Pdat[Pdat$Site=='Malham Tarn' & Pdat$Measure!='Phosphate-P',], 
          aes(x=DateTime, y=value, group=Site, col=Measure)) +
    papertheme +
    #facet_wrap(~Measure, ncol=1,  scales='free_y',labeller = mylabel_parsed) +
    geom_point(alpha=0.6) +
    geom_hline(yintercept = 12) + ylab("P (ug/L)") + xlab('Year') +
    #ylim(c(0,100)) +
    #scale_color_manual(name="Below detection limit", values=c("#4575b4","#d73027")) +
    scale_x_datetime(date_breaks = '1 year', date_labels = "%y")

TPsum <- Pdat[Pdat$Site=='Malham Tarn' & Pdat$Measure!='Phosphate-P',]   
TPsum$Year <- format(TPsum$DateTime,'%Y')
TPsum <- ddply(TPsum, .(Year), summarise, meanTP = mean(value, na.rm = T),
               medTP=median(value, na.rm = T))
TPsum <- melt(TPsum, measure.vars = c('meanTP','medTP'))

twoplot <- ggplot(TPsum, aes(x=Year, y=value, col=variable)) +
  papertheme +
  geom_point(alpha=0.6) +
  geom_hline(yintercept = 12) + ylab("Annually aggregated \n P (ug/L)") #+
  #scale_color_manual(name="Below detection limit", values=c("#4575b4","#d73027")) +
  #scale_x_datetime(date_breaks = '1 year', date_labels = "%y")

ggsave(filename = "../figs/summaryN.png",plot = Nplot)
ggsave(filename = "../figs/summaryP.png", plot=Pplot)

quickplot <- grid.arrange(oneplot, twoplot, ncol=1)
ggsave(filename = "../figs/TP-time-Malham.png", plot=quickplot, width=12, height=8)
