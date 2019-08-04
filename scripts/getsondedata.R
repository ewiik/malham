## sort sonde data created by Liam O'Reilly during 2014-2015 as well as other
##    monitoring data provided by Natural England/Nottingham Trent
## FIXME: check when sondes were swapped between the years. Would that be with the data sheet change?
##    in which case need to create column for sonde location rather than split it by year only
## read in data set
## FIXME: get units for everything
## FIXME: get side inflow data clean too!
## note lots data based on email from Liam 20.07.2016

mal1 <- read.csv("../dat-orig/NE/MalSonde2014-2015.csv", stringsAsFactors = FALSE)
mal2 <- read.csv("../dat-orig/NE/MalSonde2015-2016.csv", stringsAsFactors = FALSE)

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
saveRDS(sondedat, '../dat-mod/NE-Sondes.rds')


