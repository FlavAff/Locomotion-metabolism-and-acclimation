rm(list =ls())
dev.off()

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/Temperature and LW/")
library(plyr)
library(data.table)
library(ggplot2)

#recorded air temperatures for each site
ClimDat <- data.frame(Site = c(rep("Murcia",12),rep("Toledo",12),rep("Evora",12),rep("Porto",12),rep("Jaca",12),rep("Penalara",12)),
                      Month = c(rep(c(1:12),6)),
                      Precipitation = c(3,3,3,4,4,2,1,1,2,4,4,4,
                              6,5,4,7,7,3,2,2,3,6,6,6,
                              8,9,6,7,6,2,0,0,3,7,7,9,
                              14,13,11,10,9,6,2,3,6,10,12,12,
                              6,5,4,6,8,5,3,4,4,6,6,6,
                              12,11,10,13,13,8,4,3,7,11,12,13))

plot(ClimDat$Site,ClimDat$Precipitation)
ANOVA <- aov(ClimDat$Precipitation ~ ClimDat$Site)
summary(ANOVA)
TUKEY <- TukeyHSD(x=ANOVA, 'ClimDat$Site', conf.level=0.95)
TUKEY
t.test(ClimDat[ClimDat$Site == "Porto",]$Precipitation, ClimDat[ClimDat$Site == "Evora",]$Precipitation)
t.test(ClimDat[ClimDat$Site == "Porto",]$Precipitation, ClimDat[ClimDat$Site == "Jaca",]$Precipitation)
