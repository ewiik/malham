library(zoo)

rain <- read.table("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/bradforddata.txt",
                   skip = 14, col.names=c("Year","Month",'Tmax',"Tmin",
                                          "AFdays","Rain","Sun","Comments"), fill=T,
                   stringsAsFactors = F, na.strings = "---")
# sun in hours, rain in mm. AFdays no idea
# Bradford
# Location 414900E 435200N, Lat 53.813 Lon -1.772, 134 metres amsl
# Estimated data is marked with a * after the value.
# Missing data (more than 2 days missing in month) is marked by  ---.
# Sunshine data taken from an automatic Kipp & Zonen sensor marked with a #, otherwise sunshine data taken from a Campbell Stokes recorder.

nao <- read.table("https://crudata.uea.ac.uk/cru/data/nao/nao.dat",
                   skip = 0, col.names=c("Year",c(month.name),"YearMean"), fill=T,
                   stringsAsFactors = F, na.strings = "-99.99")
saveRDS(nao, "../dat-orig/nao.rds")

aire <- read.csv("../dat-orig/ssi-aire.csv", stringsAsFactors = F) # from barker et al

saveRDS(rain, "../dat-orig/bradford-rain-metoffice.rds")
rain <- readRDS("../dat-orig/bradford-rain-metoffice.rds")

rain <- apply(rain, 2, gsub, pattern="\\*",  replacement='')
rain <- as.data.frame(rain)
rain[,1:7] <- apply(rain[,1:7], 2, as.numeric) 
rain$YearMonth <- paste(rain$Year, rain$Month, sep='-')

ggplot(rain[rain$Year>1980 & rain$Month<8 & rain$Month>3,], aes(x=Year, y=Rain, color=Month)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Month, ncol=1, scales = "free_y")

ggplot(rain[rain$Year>1980 & rain$Month<8,], aes(x=YearMonth, y=Rain, color=Year, group=1)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(hjust=1, angle=90))

## fix aire dates
new <- aire[which(duplicated(aire$DATE)),]
aire <- aire[-which(duplicated(aire$DATE)),]

aire$DATE[grep("^18", aire$DATE)] <- as.yearmon(aire$DATE[grep("^18", aire$DATE)], format = "%Y-%m") # ^ = beginning with
aire$DATE[-grep("^18", aire$DATE)] <- gsub("-","-19", aire$DATE[-grep("^18", aire$DATE)])
aire$DATE[-grep("^18", aire$DATE)] <- as.numeric(as.yearmon(aire$DATE[-grep("^18", aire$DATE)], format = "%b-%Y")) 
aire$DATE <- as.numeric(aire$DATE)

new$DATE <- gsub("-","-20", new$DATE)
new$DATE <- as.numeric(as.yearmon(new$DATE, format = "%b-%Y")) 

aire <- rbind(aire, new)

maire <- melt(aire, measure.vars = c('X3','X6'), id.vars = 'DATE')
maire$value[maire$value == -999] <- NA
maire$variable <- factor(maire$variable, labels=c("3-month index","6-month index"))

ticks <- seq(1890, 2015, 5)

aplot <- ggplot(maire[!is.na(maire$value),], aes(y=value, x=DATE, col=value<=-1.5)) +
  papertheme +
  geom_point(alpha=0.8) +
  facet_wrap(~variable, ncol=1) +
  ylab("Standardised Streamflow Index (SSI)") + xlab("Date") +
  scale_x_continuous(breaks=ticks) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = c(1997,1998,2007,2008)) +
  scale_color_manual("Extreme drought", values=c("#542788","#b35806"))

ggsave("../figs/aire-ssi.jpg",aplot, width=17, unit='cm')

naom <- melt(nao, id.vars = 'Year', measure.vars = 2:13)
naom$DATE <- paste(naom$variable, naom$Year, sep='-')
naom$DATE <- as.numeric(as.yearmon(naom$DATE, format = "%B-%Y")) 
naomeans <- ddply(naom, .(Year), summarise, NAOmean=mean(value, na.rm=T))

naowintermeans <- ddply(naom[naom$variable %in% c("November", "December","January"),], 
                        .(Year), summarise, NAOwinter = mean(value, na.rm = T))

naomeans <- merge(naomeans, naowintermeans)

anplot <- 
  ggplot(maire[!is.na(maire$value),], aes(y=value, x=DATE)) +
  papertheme +
  geom_point(alpha=0.8, aes(col=value<=-1.5)) +
  facet_wrap(~variable, ncol=1) +
  ylab("Standardised Streamflow Index (SSI)") + xlab("Date") +
  scale_x_continuous(breaks=ticks) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = c(1997,1998,2007,2008)) +
  scale_color_manual("Extreme drought", values=c("#542788","#b35806")) +
  #geom_line(data=naom[naom$Year>1889,-2]) +
  geom_line(data=naomeans[naomeans$Year>1889,], inherit.aes = F, aes(y=NAOwinter, x=Year))

ggsave("../figs/aire-ssi-winterNAO.jpg",anplot, width=17, unit='cm')

