rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(gridExtra)

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/")
dat <- read.csv("../Results/SchoolField/Revised/UsedValuesMean.csv")
dat$site <- factor(dat$site, levels = c("Jaca","Penalara","Porto","Toledo","Evora","Murcia"))
dat$Site.April.mean.temp <- round(dat$Site.April.mean.temp, digits = 1)
dat[12,5]<-"11.0"
dat$Site.April.mean.temp <- as.factor(dat$Site.April.mean.temp)
colours <- c("darkgreen","lightgreen","orange")


p <- ggplot(data = dat, aes(x = Site.April.mean.temp, y = B0_end)) + geom_point(aes(color=Genus, shape=Genus),size=I(4)) + 
  #geom_line(aes(group=Genus, color=Genus)) + 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab(expression(paste("B0 (",mu,"mol/mgL)"))) +
  xlab("Mean Site Temperature (°C)") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = B0_end - 1.96*B0_se, ymax = B0_end + 1.96*B0_se, color = Genus, width=.2))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + scale_colour_manual(name="Genus", values=colours)
p1 <- p + facet_grid( .~ Genus, scales = "free", space = "free")
p1


p <- ggplot(data = dat, aes(x = Site.April.mean.temp, y = Ea_end)) + geom_point(aes(color=Genus, shape=Genus),size=I(4)) + 
  #geom_line(aes(group=Genus, color=Genus)) + 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("Ea (eV)") +
  xlab("Mean Site Temperature (°C)") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = Ea_end - 1.96*Ea_se, ymax = Ea_end + 1.96*Ea_se, color = Genus, width=.2))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + scale_colour_manual(name="Genus", values=colours)
p2 <- p + facet_grid( .~ Genus, scales = "free", space = "free")
p2


p <- ggplot(data = dat, aes(x = Site.April.mean.temp, y = (Tpk_end-273.15))) + geom_point(aes(color=Genus, shape=Genus),size=I(4)) + 
  #geom_line(aes(group=Genus, color=Genus)) + 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("Tpk (°C)") +
  xlab("Mean Site Temperature (°C)") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = (Tpk_end - 1.96*Tpk_se)-273.15, ymax = (Tpk_end + 1.96*Tpk_se)-273.15, color = Genus, width=.2))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + scale_colour_manual(name="Genus", values=colours)
p3 <- p + facet_grid( .~ Genus, scales = "free", space = "free")
p3




tiff("../Results/SchoolField/Revised/B0Points.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p1)
dev.off()
tiff("../Results/SchoolField/Revised/EaPoints.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p2)
dev.off()
tiff("../Results/SchoolField/Revised/TpkPoints.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p3)
dev.off()



############################################################
source("multiplot.R")
p1 <- p1 + theme(axis.title.x=element_blank())
p2 <- p2 + theme(axis.title.x=element_blank())
p3 <- p3 + theme(axis.title.x=element_blank())

tiff("../Results/SchoolField/Revised/combinedPoints.tiff", width = 25, height = 40, units = 'cm', res = 300, compression = 'lzw')
#multiplot(p1,p2,p3, labs=list("Genus",""), cols = 3)
multiplot(p1,p3,p2, cols = 1)
dev.off()
