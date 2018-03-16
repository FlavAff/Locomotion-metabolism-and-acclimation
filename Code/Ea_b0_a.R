rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(truncnorm)

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/")
k <- 8.617*(10^-5)
ChPo <- read.csv("../Results/FurtherMods/a_predictions_PortoChironomus_Vopt4_v1.csv")
ClPo <- read.csv("../Results/FurtherMods/a_predictions_PortoCloeon_Vopt4_v1.csv")
ChEv <- read.csv("../Results/FurtherMods/a_predictions_EvoraChironomus_Vopt4_v1.csv")
ClEv <- read.csv("../Results/FurtherMods/a_predictions_EvoraCloeon_Vopt4_v1.csv")

#Mean Evora=15, Porto=12.65
#values for b0 at mean temps
ChEv[67,c(3:6)]
ChPo[57,c(3:6)]
ClEv[67,c(3:6)]
ClPo[57,c(3:6)]

ChPo$Site <- rep("Porto")
ClPo$Site <- rep("Porto")
ChEv$Site <- rep("Evora")
ClEv$Site <- rep("Evora")
Ch <- rbind.data.frame(ChPo,ChEv)
Ch$One.kT <- 1/((Ch$Temperature+273.15)*k)
Cl <- rbind.data.frame(ClPo,ClEv)
Cl$One.kT <- 1/((Cl$Temperature+273.15)*k)

ChPo2a <- lm(Ch[Ch$Site=="Porto",]$active2D~Ch[Ch$Site=="Porto",]$One.kT)
ChPo3a <- lm(Ch[Ch$Site=="Porto",]$active3D~Ch[Ch$Site=="Porto",]$One.kT)
ChPo2s <- lm(Ch[Ch$Site=="Porto",]$sessile2D~Ch[Ch$Site=="Porto",]$One.kT)
ChPo3s <- lm(Ch[Ch$Site=="Porto",]$sessile3D~Ch[Ch$Site=="Porto",]$One.kT)
ClPo2a <- lm(Cl[Cl$Site=="Porto",]$active2D~Cl[Cl$Site=="Porto",]$One.kT)
ClPo3a <- lm(Cl[Cl$Site=="Porto",]$active3D~Cl[Cl$Site=="Porto",]$One.kT)
ClPo2s <- lm(Cl[Cl$Site=="Porto",]$sessile2D~Cl[Cl$Site=="Porto",]$One.kT)
ClPo3s <- lm(Cl[Cl$Site=="Porto",]$sessile3D~Cl[Cl$Site=="Porto",]$One.kT)
ChEv2a <- lm(Ch[Ch$Site=="Evora",]$active2D~Ch[Ch$Site=="Evora",]$One.kT)
ChEv3a <- lm(Ch[Ch$Site=="Evora",]$active3D~Ch[Ch$Site=="Evora",]$One.kT)
ChEv2s <- lm(Ch[Ch$Site=="Evora",]$sessile2D~Ch[Ch$Site=="Evora",]$One.kT)
ChEv3s <- lm(Ch[Ch$Site=="Evora",]$sessile3D~Ch[Ch$Site=="Evora",]$One.kT)
ClEv2a <- lm(Cl[Cl$Site=="Evora",]$active2D~Cl[Cl$Site=="Evora",]$One.kT)
ClEv3a <- lm(Cl[Cl$Site=="Evora",]$active3D~Cl[Cl$Site=="Evora",]$One.kT)
ClEv2s <- lm(Cl[Cl$Site=="Evora",]$sessile2D~Cl[Cl$Site=="Evora",]$One.kT)
ClEv3s <- lm(Cl[Cl$Site=="Evora",]$sessile3D~Cl[Cl$Site=="Evora",]$One.kT)

summary(ChPo2a)
summary(ChPo3a)
summary(ChPo2s)
summary(ChPo3s)
summary(ChEv2a)
summary(ChEv3a)
summary(ChEv2s)
summary(ChEv3s)

summary(ClPo2a)
summary(ClPo3a)
summary(ClPo2s)
summary(ClPo3s)
summary(ClEv2a)
summary(ClEv3a)
summary(ClEv2s)
summary(ClEv3s)
