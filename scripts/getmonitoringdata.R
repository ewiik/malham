## create protocol for downloading data from the yorkshire region
## ================================================================================
## data availability starts at 2000
years <- 2000:2018
namelist <- list(paste0("http://environment.data.gov.uk/water-quality/batch/measurement?area=3-34&isComplianceSample=false&year=", 
                        years))
destlist <- list(paste0("../data/yorkmon",years,".csv"))
mapply(download.file, namelist, destlist)

## extract malham and join the data sets with sensible column names
## ================================================================================
raws <- lapply(unlist(destlist), read.csv)

mals <- lapply(raws, function(x) { malhs <- grep("MALH", x$sample.samplingPoint.label)
                                  x[malhs,]})          
maldf <- do.call(rbind, mals)

names(maldf)
names(maldf) <- c("ID","pointID","pointIDshort","Site","DateTime","var","Variable","varID","wtf",
                  "value","interpretation","Unit","SampleMaterial","Compliance","Purpose","Easting",
                  "Northing")
maldf <- droplevels(maldf)

## take away columns/rows we don't want for our purposes
maldf$SampleMaterial[-grep("WATER", maldf$SampleMaterial)] # some uncoded and sewage effluent..!
maldf <- maldf[grep("WATER", maldf$SampleMaterial),] # some uncoded and sewage effluent..!

maldf <- maldf[,c("Site","DateTime","var","Variable","value", "Unit","Easting", "Northing")]


## make time sensible
maldf$DateTime <- gsub(x=maldf$DateTime, pattern = "T", replacement = " ") 
maldf$DateTime <- as.POSIXct(maldf$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London")

maldf$Year <- as.numeric(format(maldf$DateTime, "%Y"))
maldf$Month <- as.numeric(format(maldf$DateTime, "%m"))
maldf$DOY <- as.numeric(strftime(maldf$DateTime, format = "%j"))
maldf <- transform(maldf, Time = as.numeric(format(DateTime, "%H")) +
                        as.numeric(format(DateTime, "%M"))/60)
str(maldf)

## save the data for analysis
saveRDS(maldf, "../data/malham.rds")
