rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(gridExtra)
library(grid)

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/")
source("multiplot.R")
dat <- read.csv("../Results/FurtherMods/a_Ea_b0.csv")
dat <- dat[,2:8]
dat$Ea <- -dat$Ea
datCl <- subset(dat, dat$Genus == "Cloeon")
datCh <- subset(dat, dat$Genus == "Chironomus")

p <- ggplot(data = datCl, aes(x = Site, y = Ea)) + geom_point(aes(color=Site),size=I(3))+ 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("Ea") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = Ea - 1.96*Ea_se, ymax = Ea + 1.96*Ea_se, color = Site, width=.2))
p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20))
p <- p + scale_colour_manual(name="Site of Origin", values=c(rgb(0,0.5,1),rgb(1,0.25,0)))
p <- p + theme(strip.text.x = element_text(size = 17)) + theme(legend.position = "none")
p1 <- p + facet_grid( .~ Strategy, scales = "free", space = "free")
p1

p <- ggplot(data = datCh, aes(x = Site, y = Ea)) + geom_point(aes(color=Site),size=I(3))+ 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("Ea") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = Ea - 1.96*Ea_se, ymax = Ea + 1.96*Ea_se, color = Site, width=.2))
p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20))
p <- p + scale_colour_manual(name="Site of Origin", values=c(rgb(0,0.5,1),rgb(1,0.25,0)))
p <- p + theme(strip.text.x = element_text(size = 17)) + theme(legend.position = "none")
p2 <- p + facet_grid( .~ Strategy, scales = "free", space = "free")
p2

p <- ggplot(data = datCl, aes(x = Site, y = ln.b0)) + geom_point(aes(color=Site),size=I(3))+ 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("ln(B0)") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = ln.b0 - 1.96*ln.b0_se, ymax = ln.b0 + 1.96*ln.b0_se, color = Site, width=.2))
p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20))
p <- p + scale_colour_manual(name="Site of Origin", values=c(rgb(0,0.5,1),rgb(1,0.25,0)))
p <- p + theme(strip.text.x = element_text(size = 17)) + theme(legend.position = "none")
p3 <- p + facet_grid( .~ Strategy, scales = "free", space = "free")
p3

p <- ggplot(data = datCh, aes(x = Site, y = ln.b0)) + geom_point(aes(color=Site),size=I(3))+ 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab("ln(B0)") + theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = ln.b0 - 1.96*ln.b0_se, ymax = ln.b0 + 1.96*ln.b0_se, color = Site, width=.2))
p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20))
p <- p + scale_colour_manual(name="Site of Origin", values=c(rgb(0,0.5,1),rgb(1,0.25,0)))
p <- p + theme(strip.text.x = element_text(size = 17)) + theme(legend.position = "none")
p4 <- p + facet_grid( .~ Strategy, scales = "free", space = "free")
p4


pdf("../Results/FurtherMods/Corrected2/Ea_point_Cl.pdf")
print(p1)
dev.off()
pdf("../Results/FurtherMods/Corrected2/Ea_point_Ch.pdf")
print(p2)
dev.off()
pdf("../Results/FurtherMods/Corrected2/lnB0_point_Cl.pdf")
print(p3)
dev.off()
pdf("../Results/FurtherMods/Corrected2/lnB0_point_Ch.pdf")
print(p4)
dev.off()

p1 <- p1 + theme(axis.title.x=element_blank())
p2 <- p2 + theme(axis.title.x=element_blank())
tiff("../Results/FurtherMods/Corrected2/total_point.tiff", width = 30, height = 40, units = 'cm', res = 300, compression = 'lzw')
multiplot(p1, p3, p2, p4, cols=2)
dev.off()


########################################################################################
deltasE <- c()
deltasB <- c()
deltasEse <- c()
deltasBse <- c()
for (i in c(seq(5,8,1),seq(13,16,1))){
  deltaE <- dat[i,4] - dat[i-4,4]
  deltaB <- dat[i,6] - dat[i-4,6]
  deltaEse <- dat[i,5] - dat[i-4,5]
  deltaBse <- dat[i,7] - dat[i-4,7]
  
  deltasE <- c(deltaE,deltasE)
  deltasB <- c(deltaB,deltasB)
  deltasEse <- c(deltaEse,deltasEse)
  deltasBse <- c(deltaBse,deltasBse)
} 
Genus <- c(rep("Cloeon",4),rep("Chironomus",4))
Strategy <- rev(dat[c(1:4),2])
newdat <- as.data.frame(cbind.data.frame(Genus=as.character(Genus),
                                         Strategy=as.character(Strategy),
                                         deltasE=as.numeric(deltasE),
                                         deltasEse=as.numeric(deltasEse),
                                         deltasB=as.numeric(deltasB),
                                         deltasBse=as.numeric(deltasBse)))

p <- ggplot(data=newdat, aes(x = Strategy, y = deltasE)) + geom_point(aes(),size=I(3))+ 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab(expression(paste(Delta,"Ea(eV)"))) +
  theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = deltasE - 1.96*deltasEse, ymax = deltasE + 1.96*deltasEse, width=.2))
p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20))
p <- p + scale_colour_manual(name="Site of Origin", values=c(rgb(0,0.5,1),rgb(1,0.25,0)))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + geom_hline(yintercept=0, linetype="dotted", color = ("red"))
p5 <- p + facet_grid( .~ Genus, scales = "free", space = "free")
tiff("../Results/FurtherMods/deltaE_point.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p5)
dev.off()

p <- ggplot(data=newdat, aes(x = Strategy, y = deltasB)) + geom_point(aes(),size=I(3))+ 
  theme_classic() + theme(axis.title=element_text(size=22)) + ylab(expression(paste(Delta,"B0"))) +
  theme(axis.text=element_text(size=15))
p <- p + geom_errorbar(aes(ymin = deltasB - 1.96*deltasBse, ymax = deltasB + 1.96*deltasBse, width=.2))
p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20))
p <- p + scale_colour_manual(name="Site of Origin", values=c(rgb(0,0.5,1),rgb(1,0.25,0)))
p <- p + theme(strip.text.x = element_text(size = 17, face = 'italic')) + theme(legend.position = "none")
p <- p + geom_hline(yintercept=0, linetype="dotted", color = ("red"))
p6 <- p + facet_grid( .~ Genus, scales = "free", space = "free")
tiff("../Results/FurtherMods/deltaB0_point.tiff", width = 15, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p6)
dev.off()

tiff("../Results/FurtherMods/delta_point.tiff", width = 30, height = 10, units = 'cm', res = 300, compression = 'lzw')
multiplot(p5, p6, cols=2)
dev.off()
