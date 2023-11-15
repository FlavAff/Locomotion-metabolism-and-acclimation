rm(list = ls())
dev.off()

library(tidyverse)

setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("further_models_fun.R")

params <- read_csv("../Results/SchoolField/ModelParameters.csv")

Temp <- seq(from = 273.15, to = 318.15, length.out = 200)
Pd2 <- 0.68
Pd3 <- 1.05
d0 <- 0.3
i <- 1
c <- 1

searchrates <- function(preydat,preddat,preyname,loc) {
  if (loc == "Toledo") {
    Tref <- 14.6 + 273.15
  }
  if (loc == "Porto") {
    Tref <- 16.9 + 273.15
  }
  if (loc == "Evora") {
    Tref <- 17.3 + 273.15
  }
  PreyParams <- params %>% filter(site==loc) %>% filter(genus==preyname)
  PredParams <- params %>% filter(site==loc) %>% filter(genus=="Sympetrum")
  
  mr <- mean(preydat$Mass)
  mc <- mean(preddat$Mass)
  
  bc <- PredParams[PredParams$term=="b",]$estimate
  br <- PreyParams[PreyParams$term=="b",]$estimate
  
  Ec <- PredParams[PredParams$term=="E",]$estimate
  Er <- PreyParams[PreyParams$term=="E",]$estimate
  
  v0c <- Vopt2(power(PredParams[PredParams$term=="B0",]$estimate),mc)
  v0r <- Vopt2(power(PreyParams[PreyParams$term=="B0",]$estimate),mr)
  
  #print(Ec-Er)
  
  sessile2D <- sessile.search.rate.2D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c, Tref = Tref)
  sessile3D <- sessile.search.rate.3D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c, Tref = Tref)
  active2D <- active.search.rate.2D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, b0r = v0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c, Tref = Tref)
  active3D <- active.search.rate.3D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, b0r = v0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c, Tref = Tref)
  
  Temperature <- Temp - 273.15
  strats <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  strats$One.over.kT <- 1/(k*Temp)
  strats$TempK <- Temp
  return(strats)
}

aTolCh<- searchrates(chir,strio,"Chironomus","Toledo")
aPorCh <- searchrates(chir,strio,"Chironomus","Porto")
aEvoCh <- searchrates(chir,strio,"Chironomus","Evora")

aTolCl<- searchrates(chir,strio,"Cloeon","Toledo")
aPorCl <- searchrates(chir,strio,"Cloeon","Porto")
aEvoCl <- searchrates(chir,strio,"Cloeon","Evora")


getEanda0 <- function(mod,dim) {
  linearmod1 <- lm(log(mod$sessile2D)~mod$One.over.kT)
  linearmod2 <- lm(log(mod$sessile3D)~mod$One.over.kT)
  linearmod3 <- lm(log(mod$active2D)~mod$One.over.kT)
  linearmod4 <- lm(log(mod$active3D)~mod$One.over.kT)
  #print(paste("B0 for sessile 2D is",linearmod1$coefficients[1]))
  #print(paste("E for sessile 2D is",abs(linearmod1$coefficients[2])))
  
  #print(paste("B0 for sessile 3D is",linearmod2$coefficients[1]))
  #print(paste("E for sessile 3D is",abs(linearmod2$coefficients[2])))
  
  #print(paste("B0 for active 2D is",linearmod3$coefficients[1]))
  #print(paste("E for active 2D is",abs(linearmod3$coefficients[2])))
  
  #print(paste("B0 for active 3D is",linearmod4$coefficients[1]))
  #print(paste("E for active 3D is",abs(linearmod4$coefficients[2])))
  if (dim == 2) {
    print(summary(linearmod1))
  }
  if (dim == 3) {
    print(summary(linearmod4))
  }
}

getEanda0(aTolCh,2)
getEanda0(aPorCh,2)
getEanda0(aEvoCh,2)

getEanda0(aTolCl,3)
getEanda0(aPorCl,3)
getEanda0(aEvoCl,3)

