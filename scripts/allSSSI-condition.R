## look at condition and condition assessment date for SSSI units
## note that condition or assessment date or type is not available for SACs and SSSIs in
##    the portal and I sent an EIR on 31st Oct 2023
## data downloaded 31st Oct 2023

## read in packages
library(dplyr)
library(plyr)
library(ggplot2)

## read in dat
dat <- read.csv("")

## make date into date etc
## NOTE that there are 102 NAs in the cond_date column! These are for 9 sites
dat$condat <- as.POSIXct(dat$COND_DATE, format="%m/%d/%Y %H:%M:%S", tz="GMT")
dat$conyear <- as.numeric(format(dat$condat, "%Y"))
dat$conmon <- as.numeric(format(dat$condat, "%m"))
# table(dat$SSSI_NAME[which(is.na(dat$conyear))])

## create summary for when sites were assessed, also by condition
assdates <- ddply(dat, .(conyear, CONDITION), summarise, N=n())
assdates <- na.omit(assdates)
assdates$CONDITION <- factor(assdates$CONDITION, 
                                levels=c("DESTROYED","PART DESTROYED",
                                         "UNFAVOURABLE DECLINING",
                                         "UNFAVOURABLE NO CHANGE",
                                         "UNFAVOURABLE RECOVERING",
                                         "NOT ASSESSED", "FAVOURABLE"),
                             ordered = T)

## create category for condition, e.g. destroyed would plausibly never warrant reassessment
megacat <- data.frame(CONDITION=levels(assdates$CONDITION), 
                      Class=c("Destroyed",rep("Under threat", times=4),
                              "Needs assessment","Favourable"))
megacat$Class <- factor(megacat$Class, levels=c("Destroyed","Under threat",
                                                "Needs assessment","Favourable"),
                        ordered = T)

## add this to dat too
dat <- merge(dat, megacat, sort=F)

## patch holes in data set in terms of years of 'no data'
assdates <- merge(expand.grid(CONDITION=levels(asscum$CONDITION),
                          conyear=seq(1998,2023,1)), 
              assdates, all.x=TRUE)
assdates <- merge(assdates, megacat, sort=F)
# make N of these years 0 to enable cumsum
assdates$N[which(is.na(assdates$N))] <- 0

asscum = assdates %>% group_by(CONDITION) %>% arrange(conyear) %>% mutate(Total = cumsum(N),
                                                                          proptot=round(cumsum(N)/sum(N)*100,digits=1))

## create summary data for just what the condition distribution is
sumcond <- ddply(dat, .(CONDITION), summarise, Total=n(), Percentage=round(n()/nrow(dat)*100, digits=1))
sumcond <- merge(sumcond, megacat, sort=F)
sumcond2 <- ddply(sumcond, .(Class), summarise, Percentage=sum(Percentage))

## create summary data for what proportion of sites were assessed within the last 6-year cycle
sumcond3 <- ddply(dat, .(CONDITION, conyear<2018), summarise, Total=n())
sumcond3 <- merge(sumcond3, megacat, sort=F)
sumcond4 <- ddply(sumcond3, .(Class,`conyear < 2018`), summarise, Total=sum(Total))
sumcond4 <- ddply(sumcond4, .(Class), summarise, pOld=Total[which(`conyear < 2018`==T)]/sum(Total))
# 90 percent of all destroyed discovered prior to 6 years ago
# 73% of under threat discovered prior to 6 yrs ago
# 74% of favourable discovered prior to 6yrs ago

## create summary data for ditribution of Class every year
yearsum <- ddply(dat, .(conyear, Class), summarise, N=n())
yeartot <- ddply(dat, .(conyear), summarise, Total=n())

yearsum <- merge(yearsum, yeartot)
yearsum$Percentage <- round(yearsum$N/yearsum$Total*100, digits = 1)

## plot the conditions and dates
## NOTE that according to the Common Standards Monitoring guidance, the condition of a unit should be assessed at
##    least every six years.
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
ggplot(assdates, aes(x=conyear, group=CONDITION, fill=CONDITION, y=N)) +
  theme_bw() +
  facet_wrap(~Class, nrow=2, scales='free_y') +
  geom_vline(xintercept = 2017) +
  geom_bar(stat='identity', position=position_stack(), color='black') +
  scale_fill_manual(values=c("grey70",'#e31a1c','#fd8d3c','#fecc5c','#ffffb2','#e31a1c',"#0570b0")) +
  theme(legend.position="top") 

ggplot(asscum, aes(x=conyear, group=CONDITION, fill=CONDITION, y=proptot)) +
  theme_bw() +
  facet_wrap(~Class, nrow=4, scales='free_y') +
  geom_vline(xintercept = 2017.5) +
  geom_bar(stat='identity', position=position_dodge(), color='black') +
  scale_fill_manual(values=c("grey70",'#e31a1c','#fd8d3c','#fecc5c','#ffffb2','#e31a1c',"#0570b0")) +
  theme(legend.position="top") 

ggplot(dat, aes(x=conyear, group=Class, fill=Class)) +
  geom_bar()

yearp1 <- ggplot(yearsum, aes(x=conyear, y=Percentage, group=Class, fill=Class)) +
  theme_bw() +
  geom_bar(stat='identity') +
  theme(legend.position="top", axis.title.x = element_blank()) +
  scale_y_continuous(labels=c(0,25,50,75,""))

yearp2 <- ggplot(yeartot, aes(x=conyear, y=Total))+
  theme_bw() +
  geom_line() + geom_point() + xlab("Year assessed") +
  scale_y_continuous(labels=c('0','1k','2k','3k','4k','5k'))

grid.arrange(yearp1, yearp2, nrow=2)

ggplot(yearsum[yearsum$Class=="Favourable",], aes(x=Total, y=Percentage, size=conyear,
                                                  color=conyear)) +
  geom_point() +
  scale_color_viridis_b()
