rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(truncnorm)
library(viridis)

setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("further_models_fun.R")
source("a_plot_fun2.R")
source("multiplot.R")
values <- read.csv("../Results/SchoolField/New/UsedValuesMeanNew.csv")

Pd2 <- 0.68
Pd3 <- 1.05
d0 <- 0.3
i <- 1
c <- 1
visc <- 0.000899

pCl2 <- SiteSearchRate.Vopt4.version1("Porto",strio,dipt,"Sympetrum","Cloeon", maxi = 10)
pCl1 <- SiteSearchRate.Vopt4.version1("Toledo",strio,dipt,"Sympetrum","Cloeon", maxi = 10)
pCl3 <- SiteSearchRate.Vopt4.version1("Evora",strio,dipt,"Sympetrum","Cloeon", maxi = 10)
#Remember to set right curve thickness in function script (yes this could be done better)
pCh1 <- SiteSearchRate.Vopt4.version1("Toledo",strio,chir,"Sympetrum","Chironomus", maxi = 10)
pCh3 <- SiteSearchRate.Vopt4.version1("Evora",strio,chir,"Sympetrum","Chironomus", maxi = 10)
pCh2 <- SiteSearchRate.Vopt4.version1("Porto",strio,chir,"Sympetrum","Chironomus", maxi = 10)

#Figure 6 will be made in seashore
pCl2 <- pCl2 + theme(axis.title = element_blank(),legend.position = "none") + 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=0, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pCl1 <- pCl1 + theme(axis.title = element_blank(),legend.position = "none") + 
  geom_rect(aes(xmin=10, xmax=35, ymin=0, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pCl3 <- pCl3 + theme(axis.title = element_blank(),legend.position = "none") + 
  geom_rect(aes(xmin=10, xmax=30.192, ymin=0, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pCh2 <- pCh2 + theme(axis.title = element_blank(),legend.position = "none") + ggtitle("Porto") + 
  theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5)) + 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=0, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pCh3 <- pCh3 + theme(axis.title = element_blank(),legend.position = "none") + ggtitle("Evora") + 
  theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5)) + 
  geom_rect(aes(xmin=10, xmax=30.192, ymin=0, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pCh1 <- pCh1 + theme(axis.title = element_blank(),legend.position = "none") + ggtitle("Toledo") + 
  theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5)) + 
  geom_rect(aes(xmin=10, xmax=35, ymin=0, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)


source("multiplot.R")
multiplot(pCh1, pCl1, pCh2, pCl2, pCh3, pCl3, cols = 3)




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
