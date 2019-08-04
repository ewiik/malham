## tidy wiik phd nutrient data

if(!file.exists("../dat-orig/wiiketal/emmatp.csv")) {
  download.file(url="https://ndownloader.figshare.com/files/5666586", destfile="../data/emmatp.csv")
}
malemma <- read.csv("../dat-orig/wiiketal/emmatp.csv", sep = "\t", row.names = 1)
malemma <- as.data.frame.matrix(t(malemma))
malemma$Month <- sprintf("%02d", match(substr(rownames(malemma), 1,3), tolower(month.abb)))
malemma$Year <- 2000 + as.numeric(substr(rownames(malemma),4,5))
malemma$DateTime <- paste0(malemma$Month, malemma$Year)
malemma$DateTime <- as.POSIXct(paste0(malemma$datetime,"01"), format="%m%Y%d") ## FIXME; need days of
##    my samplings... not tidy in thesis meta..=(
malemma <- data.frame(Site="Malham Tarn", melt(malemma, id.vars = "DateTime", measure.vars = "Mal"))
malemma$lessthan <- FALSE
malemma$Measure <- "TP"

saveRDS(malemma, "../dat-mod/malham-wiik-TP.rds")


