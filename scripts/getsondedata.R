## sort sonde data created by Liam O'Reilly during 2014-2015 as well as other
##    monitoring data provided by Natural England/Nottingham Trent
## FIXME: check when sondes were swapped between the years. Would that be with the data sheet change?
##    in which case need to create column for sonde location rather than split it by year only
## read in data set
## FIXME: get units for everything
## FIXME: get side inflow data clean too!

mal1 <- read.csv("../data/MalSonde2014-2015.csv", stringsAsFactors = FALSE)
mal2 <- read.csv("../data/MalSonde2015-2016.csv", stringsAsFactors = FALSE)
tp1 <- read.csv("../data/Mal-Sampling-NottsTrent-2014.csv", skip=1, stringsAsFactors = FALSE)
tp2 <- read.csv("../data/Mal-Stream-Sampling-NottsTrent-2014.csv", skip=2, stringsAsFactors = FALSE)
tp3 <- read.csv("../data/MalTP-NE-NottsTrent-2014.csv", stringsAsFactors = FALSE)
tp4 <- read.csv("../data/MalTP-NE-2015-2016.csv", stringsAsFactors = FALSE)

## create function that converts mg L PO4 to ug L P.
po4top <- function(x) {Pmass <- 30.974
Omass <- 15.999
pfrac <- Pmass/(Pmass+Omass*4)
x <- pfrac*x*1000}


## ==========================================================================================
## create flow data for both years but note that sonde locations different!
flow1 <- mal1[-1,c(1:4)]
colnames(flow1) <- c("Date", "Time","Level","Flow")
flow1$Sonde <- rep("Flow")

flow2 <- mal2[-1,c(1:4)]
colnames(flow2) <- c("Date", "Time","Level","Flow")
flow2$Sonde <- rep("Flow")

flow <- rbind(flow1, flow2)
numcols <- which(names(flow) %in% c('Level','Flow'))
flow[,numcols] <- apply(flow[,numcols], 2, as.numeric)
flow <- na.omit(flow)

## create inflow data for both years but note that donde locations different
inflowcols <- seq(from=which(names(mal1)=="Inflow.Sonde"), to=which(names(mal1)=="X.9"))
inflow1 <- mal1[-1,c(1:2, inflowcols)]
names(inflow1) <- c("Date","Time","Temp","Cond","pH","Ammon","Turb","DOrel","DOabs","Chl")
inflow1$Sonde <- "Inflow1"

inflowcols <- seq(from=which(names(mal2)=="Inflow.Sonde"), to=which(names(mal2)=="X.8"))
inflow2 <- mal2[-1,c(1:2, inflowcols)]
names(inflow2) <- c("Date","Time","Temp","Cond","pH","Ammon","Turb","DOrel","DOabs")
inflow2$Chl <- NA
inflow2$Sonde <- "Inflow2"

inflow <- rbind(inflow1, inflow2)
numcols <- c("Temp","Cond","pH","Ammon","Turb","DOrel","DOabs","Chl")
inflow[,numcols] <- apply(inflow[,numcols], 2, as.numeric)
inflow <- inflow[-which(is.na(inflow[,numcols])),]

## create outflow data for both years (these in same place)
outflowcols <- seq(from=which(names(mal1)=="Outflow.Sonde"), to=which(names(mal1)=="X.16")) 
outflow1 <- mal1[-1,c(1:2,outflowcols)]
names(outflow1) <- c("Date","Time","Temp","Cond","pH","Ammon","Turb","DOrel","DOabs","Chl")
outflow1$Sonde <- "Outflow"

outflowcols <- seq(from=which(names(mal2)=="Outflow.Sonde"), to=which(names(mal2)=="X.20")) 
outflow2 <- mal2[-1,c(1:2,outflowcols)]
names(outflow2) <- c("Date","Time","Temp","Cond","pH","Ammon","Turb","DOrel","DOabs")
outflow2$Chl <- NA
outflow2$Sonde <- "Outflow"

outflow <- rbind(outflow1, outflow2)
outflow[,numcols] <- apply(outflow[,numcols], 2, as.numeric)
outflow <- outflow[-which(is.na(outflow[,numcols])),]

## create outflow level data for both years (these in same place)
outlevels <- mal1[-1,c(1:2, which(names(mal1)=="OutFlow.Level"))]
names(outlevels) <- c('Date',"Time","OutLevel")

outlevels2 <- mal2[-1, c(1:2, which(names(mal2)=="OutFlow.Level"))]
names(outlevels2) <- c('Date',"Time","OutLevel")

outlevels <- rbind(outlevels, outlevels2)
outlevels$Sonde <- "OutflowLevel"

## ===========================================================================================
## make them all have sensible time and date
alldf <- list(flow, inflow, outflow, outlevels)

alldf <- lapply(alldf, function(x) {x$Date <- as.POSIXct(x$Date, tz="Europe/London", format="%d/%m/%Y")
return(x)})

alldf <- lapply(alldf, function(x) {x$DateTime <- with(x, paste(Date,Time, sep=" "))
x$DateTime <-as.POSIXct(x$DateTime, tz="Europe/London", format="%Y-%m-%d %H:%M:%S")
return(x)})

alldf <- lapply(alldf, function(x) {x$DateTime <- with(x, paste(Date,Time, sep=" "))
x$DateTime <-as.POSIXct(x$DateTime, tz="Europe/London", format="%Y-%m-%d %H:%M:%S")
return(x)})

alldf <- lapply(alldf, function(x) {x$Year <- as.numeric(format(x$DateTime, "%Y"))
x$Month <-as.factor(format(x$DateTime, "%b"))
return(x)})

names(alldf) <- c("flowdf","inflowdf","outflowdf","outlevelsdf")
attach(alldf)

sondedat <- rbind(inflowdf, outflowdf)
sondedat <- melt(sondedat, id.vars = c('Date','Time','Sonde','DateTime','Year','Month'))

flowmelt <- melt(flowdf, id.vars=c('Date','Time','Sonde','DateTime','Year','Month'))
levelmelt <- melt(outlevelsdf, id.vars = c('Date','Time','Sonde','DateTime','Year','Month'))

sondedat <- rbind(sondedat, flowmelt, levelmelt)
saveRDS(sondedat, '../data/dat-mod/allSondes.rds')

## =================================================================================================
## Fix TP stuff
tp1 <- tp1[,c("Date","Time..BST.","Site","Phosphate..mg.l.PO4....Hach.PhosVer")]
names(tp1) <- c("Date","Time","Site","PO4mgL")

tp1$DateTime <- with(tp1, paste(Date,Time, sep=" "))
tp1$DateTime <- gsub("approx ", "", tp1$DateTime)
tp1$DateTime <- gsub("14:30","2:30:00 PM", tp1$DateTime)
tp1$DateTime <-as.POSIXct(tp1$DateTime, tz="Europe/London", format="%d/%m/%Y %I:%M:%S %p")
  # some end up NA but these had no time available so whatever.
tp1$ugP <- po4top(tp1$PO4mgL)

## note that I don't care here about BST vs GMT for our purposes but could check these later FIXME
tp2 <- tp2[,c('Date','Time..BST.black..GMT.blue.','Site','Phosphate..mg.l.PO4.')]
names(tp2) <- c("Date","Time","Site","PO4mgL")

tp2$DateTime <- with(tp2, paste(Date,Time, sep=" "))
tp2$DateTime <- gsub("2:20:00 PM", "14:20", tp2$DateTime)
tp2$DateTime <- gsub("2:55:00 PM","14:55", tp2$DateTime)
tp2$DateTime <-as.POSIXct(tp2$DateTime, tz="Europe/London", format="%d/%m/%Y %H:%M")
# remove empty rows of data
tp2 <- tp2[complete.cases(tp2$DateTime),]
# some end up NA but these had no time available so whatever.
tp2$ugP <- po4top(tp2$PO4mgL)

tp3$Date <- as.POSIXct(tp3$Date, tz="Europe/London", format="%d/%m/%Y")

tp4$DateTime <- gsub("MAR","03", tp4$DateTime)
tp4$DateTime <- gsub("  ", " ", tp4$DateTime)
tp4$DateTime <-as.POSIXct(tp4$DateTime, tz="Europe/London", format="%d-%m-%y %H:%M")
tp4$TPugL <- as.numeric(gsub("<","",tp4$TpmgL))*1000

## FIXME: can we find time of day for tp3 and also where was tp taken from?
alltp <- data.frame(DateTime=tp1$DateTime, P=tp1$ugP, Site="Tarn",Measure="Phosphate")
alltp <- rbind(alltp, data.frame(DateTime=tp2$DateTime, P=tp2$ugP, Site="Streams",Measure="Phosphate"))
alltp <- rbind(alltp, data.frame(DateTime=tp3$Date, P=tp3$TpugL, Site="Inflow?", Measure="TP"))
alltp <- rbind(alltp, data.frame(DateTime=tp4$DateTime, P=tp4$TPugL, Site="Inflow?", Measure="TP-DT20"))

## ==========================================================================================================
## save relevant data frames
saveRDS(alltp, "../data/dat-mod/allPhosphorus.rds")

