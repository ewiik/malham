## create plot for estimated loadings based on goldsmith draft report
## estimated annual loads in kg for TP and nitrate-N
## detection limits 
library("ggplot2")
library("extrafont")

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

loads <- data.frame(Point=rep(seq(1:8), times=2), Nutrient=rep(c("Nitrogen","Phosphorus"), each=8, times=2),
                    Load=c(645.86,422.97,1023.66,2897.28,NA, 22.9, NA, 33,
                           8.71,3.77, 22.11,58.4, NA, 1.2, NA, 3),
                    Site=c(rep("Northwest", times=5), rep("East", times=3)))

## plot the loads
loadplot <- ggplot(loads, aes(y=Load, x=Point, group=Nutrient)) +
  papertheme+
  geom_col(position="dodge", aes(fill=Site)) +
  facet_wrap(~Nutrient, scales='free_y') + ylab("Load (kg/yr)")

ggsave(plot=loadplot, file="../figs/loadplot.pdf")

## some observational data 2009-2010 from field notebook from PhD
obsdat <- data.frame(Date=c("15.09.09","12.08.10","18.09.10"), Notes=c("A lot of filamentous algae in inflow area"),
                     "Lots of small green algal balls in filter paper","Loads of the tiny orb algae")