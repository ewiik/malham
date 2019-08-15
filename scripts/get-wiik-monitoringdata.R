## tidy wiik phd nutrient data

if(!file.exists("../dat-orig/wiiketal/emmatp.csv")) {
  download.file(url="https://ndownloader.figshare.com/files/5666586", destfile="../data/emmatp.csv")
}
malemma <- read.csv("../dat-orig/wiiketal/emmatp.csv", sep = "\t", row.names = 1)
malemma <- as.data.frame.matrix(t(malemma))
malemma$Month <- sprintf("%02d", match(substr(rownames(malemma), 1,3), tolower(month.abb)))
malemma$Year <- 2000 + as.numeric(substr(rownames(malemma),4,5))

## insert exact dates from field notebooks (do here for all sites, for the record)
## FIXME: where is feb 2009?
malemma <- melt(malemma, id.vars = c('Month','Year'), variable.name = 'Site')
malemma <- malemma[order(malemma$Year, malemma$Month, malemma$variable),]

malemma$DateTime <- c('11.03.09','10.03.09','12.03.09','01.04.09','01.04.09','02.04.09',
                      '07.05.09','08.05.09','09.05.09','17.06.09','16.06.09','15.06.09',
                      '18.07.09','18.07.09','19.07.09','12.08.09','13.08.09','11.08.09',
                      '15.09.09','14.09.09','16.09.09','21.10.09','20.10.09','19.10.09',
                      '08.12.09','08.12.09','09.12.09','13.02.10','13.02.10','14.02.10',
                      '20.03.10','20.03.10','21.03.10','20.04.10','19.04.10','20.04.10',
                      '18.05.10','17.05.10','19.05.10','15.06.10','15.06.10','16.06.10',
                      '17.07.10','16.07.10','16.07.10','10.08.10','10.08.10','12.08.10',
                      '20.09.10','19.09.10','18.09.10','25.01.11','25.01.11','26.01.11')


malemma$DateTime <- as.POSIXct(malemma$DateTime, format="%d.%m.%y", tz='Europe/London') 

malemma$lessthan <- FALSE
malemma$Measure <- "TP"

malemma <- subset(malemma, Site=='Mal')
malemma$Site <- 'Malham Tarn'

saveRDS(malemma, "../dat-mod/malham-wiik-TP.rds")


