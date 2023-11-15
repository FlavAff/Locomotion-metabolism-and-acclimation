rm(list = ls())
dev.off()

library(plyr)
library(ggplot2)
library(gridExtra)

setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
dat <- read.csv("../Results/SchoolField/EaB0.csv")
#dat$Site.April.mean.temp <- round(dat$Site.April.mean.temp, digits = 1)
#dat[12,5]<-"11.0"
#dat$Site.April.mean.temp <- as.factor(dat$Site.April.mean.temp)
#dat$Site.April.mean.temp <- factor(dat$Site.April.mean.temp, levels= c("7.3", "11.0", "12.7", "13.5", "14.9", "16.2")) 
dat$site <- factor(dat$site, levels = c("Toledo","Porto","Evora"))
colours <- c("blue","purple","red")


p <- ggplot(data = dat, aes(x = site, y = B0_end)) + geom_point(aes(color=site, shape=Genus),size=I(4)) + 
  #geom_line(aes(group=Genus, color=Genus)) + 
  theme_classic() + theme(axis.title=element_text(size=35),axis.title.x = element_blank()) + ylab(expression(B[0](mu~mol/mgL))) +
  xlab("Site") + theme(axis.text=element_text(size=32))
p <- p + geom_errorbar(aes(ymin = B0_end - 1*B0_se, ymax = B0_end + 1*B0_se, color = site, width=.2))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + scale_colour_manual(name="Genus", values=colours)
p1 <- p + facet_grid( .~ Genus, scales = "free", space = "free") + theme(strip.text.x = element_text(size = 32))
p1


p <- ggplot(data = dat, aes(x = site, y = Ea_end)) + geom_point(aes(color=site, shape=Genus),size=I(4)) + 
  #geom_line(aes(group=Genus, color=Genus)) + 
  theme_classic() + theme(axis.title=element_text(size=35), axis.title.x = element_blank()) + ylab("E (eV)") +
  theme(axis.text=element_text(size=32))
p <- p + geom_errorbar(aes(ymin = Ea_end - 1.645*Ea_se, ymax = Ea_end + 1.645*Ea_se, color = site, width=.2))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + scale_colour_manual(name="Genus", values=colours)
p2 <- p + facet_grid( .~ Genus, scales = "free", space = "free") + theme(strip.text.x = element_text(size = 32))
p2


p <- ggplot(data = dat, aes(x = site, y = (Tpk_end-273.15))) + geom_point(aes(color=Genus, shape=Genus),size=I(4)) + 
  #geom_line(aes(group=Genus, color=Genus)) + 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("Tpk (?C)") +
  xlab("Site") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = (Tpk_end - 1.96*Tpk_se)-273.15, ymax = (Tpk_end + 1.96*Tpk_se)-273.15, color = Genus, width=.2))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + scale_colour_manual(name="Genus", values=colours)
p3 <- p + facet_grid( .~ Genus, scales = "free", space = "free")
p3




tiff("../Results/SchoolField/B0Points.tiff", width = 40, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p1)
dev.off()
tiff("../Results/SchoolField/EaPoints.tiff", width = 40, height = 20, units = 'cm', res = 300, compression = 'lzw')
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

tiff("../Results/SchoolField/New/combinedPoints.tiff", width = 30, height = 40, units = 'cm', res = 250, compression = 'lzw')
#multiplot(p1,p2,p3, labs=list("Genus",""), cols = 3)
multiplot(p1,p2, cols = 1)
dev.off()
