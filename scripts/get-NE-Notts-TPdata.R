## sort out NE and notts trent sampling for TP

tp1 <- read.csv("../dat-orig/NottsTrent/Mal-Sampling-NottsTrent-2014.csv", skip=1, stringsAsFactors = FALSE)
tp2 <- read.csv("../dat-orig/NottsTrent/Mal-Stream-Sampling-NottsTrent-2014.csv", skip=2, stringsAsFactors = FALSE)
tp3 <- read.csv("../dat-orig/NE/MalTP-NE-NottsTrent-2014.csv", stringsAsFactors = FALSE)
tp4 <- read.csv("../dat-orig/NE/MalTP-NE-2015-2016.csv", stringsAsFactors = FALSE)

## create function that converts mg L PO4 to ug L P.
po4top <- function(x) {Pmass <- 30.974
Omass <- 15.999
pfrac <- Pmass/(Pmass+Omass*4)
x <- pfrac*x*1000}


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

tp2tidy <- tp2
tp2tidy$Site[grep("Out", tp2tidy$Site)] <- "Outflow"
tp2tidy$Site[grep("In", tp2tidy$Site)] <- "Inflow"
tp2tidy$Site[grep("ridge", tp2tidy$Site)] <- "Inflow"
tp2tidy$Site[grep("inlet", tp2tidy$Site)] <- "Inflow"
tp2tidy <- tp2tidy[-which(tp2tidy$Site==" "),]

## note this is "autosampler" (notts not nott trent) plus the random NE file not within a zip file
## latter no site mentioned; both have flow data thoughso likely inflow.. FIXME
tp3$Date <- as.POSIXct(tp3$Date, tz="Europe/London", format="%d/%m/%Y")

tp4$DateTime <- gsub("MAR","03", tp4$DateTime)
tp4$DateTime <- gsub("  ", " ", tp4$DateTime)
tp4$DateTime <-as.POSIXct(tp4$DateTime, tz="Europe/London", format="%d-%m-%y %H:%M")
tp4$TPugL <- as.numeric(gsub("<","",tp4$TpmgL))*1000

## FIXME: can we find time of day for tp3 and also where was tp taken from?
alltp <- data.frame(DateTime=tp1$DateTime, P=tp1$ugP, Site="Malham Tarn",Measure="Phosphate-P")
alltp <- rbind(alltp, data.frame(DateTime=tp2tidy$DateTime, P=tp2tidy$ugP, Site=tp2tidy$Site,
                                 Measure="Phosphate-P"))
alltp <- rbind(alltp, data.frame(DateTime=tp3$Date, P=tp3$TpugL, Site="Inflow", Measure="TP"))
alltp <- rbind(alltp, data.frame(DateTime=tp4$DateTime, P=tp4$TPugL, Site="Inflow", Measure="TP-DT20"))

## ==========================================================================================================
## save relevant data frames
saveRDS(alltp, "../dat-mod/NE-Notts-TP.rds")
