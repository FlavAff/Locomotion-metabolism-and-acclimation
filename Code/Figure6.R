rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(truncnorm)

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("further_models_fun.R")
source("a_plot_fun2.R")
source("multiplot.R")
values <- read.csv("../Results/SchoolField/Revised/UsedValuesMean.csv")

Pd2 <- 0.68
Pd3 <- 1.05
d0 <- 0.3
i <- 1
c <- 1
visc <- 0.000899

SiteSearchRate.Vopt4.version1("Porto",strio,dipt,"Sympetrum","Cloeon", maxi = 3)
#SiteSearchRate.Vopt4.version1("Toledo",strio,dipt,"Sympetrum","Cloeon", maxi = 40)
SiteSearchRate.Vopt4.version1("Evora",strio,dipt,"Sympetrum","Cloeon", maxi = 3)
#Remember to set right curve thickness in function script (yes this could be done better)
p1 <- SiteSearchRate.Vopt4.version1("Porto",strio,dipt,"Sympetrum","Cloeon", maxi = 3)
p2 <- SiteSearchRate.Vopt4.version1("Evora",strio,dipt,"Sympetrum","Cloeon", maxi = 3)
SiteSearchRate.Vopt4.version1("Porto",strio,dipt,"Sympetrum","Chironomus", maxi = 1.5)
#SiteSearchRate.Vopt4.version1("Toledo",strio,dipt,"Sympetrum","Chironomus", maxi = 0.65)
SiteSearchRate.Vopt4.version1("Evora",strio,dipt,"Sympetrum","Chironomus", maxi = 1.5)
p4 <- SiteSearchRate.Vopt4.version1("Evora",strio,chir,"Sympetrum","Chironomus", maxi = 1.25)
p3 <- SiteSearchRate.Vopt4.version1("Porto",strio,chir,"Sympetrum","Chironomus", maxi = 1.25)

#Figure 6 will be made in seashore
p1 <- p1 + theme(axis.title = element_blank(),legend.position = "none")
p2 <- p2 + theme(axis.title = element_blank(),legend.position = "none")
p3 <- p3 + theme(axis.title = element_blank(),legend.position = "none") + ggtitle("Cool site (Porto)") + theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5))
p4 <- p4 + theme(axis.title = element_blank(),legend.position = "none") + ggtitle("Warm site (Evora)") + theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5))
tiff("../Results/FurtherMods/aChirP.tiff", width = 10, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p3)
dev.off()
tiff("../Results/FurtherMods/aChirE.tiff", width = 10, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p4)
dev.off()
tiff("../Results/FurtherMods/aCloeP.tiff", width = 10, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p1)
dev.off()
tiff("../Results/FurtherMods/aCloeE.tiff", width = 10, height = 10, units = 'cm', res = 300, compression = 'lzw')
print(p2)
dev.off()
