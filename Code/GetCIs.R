setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")

# load packages
library(rTPC)
library(broom)
library(tidyverse)
library(minpack.lm)
library(nls.multstart)

source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("Schoolfields2.R")

EvoraChir <- subset(chir, chir$site == "Evora")
EvoraDipt <- subset(dipt, dipt$site == "Evora")
EvoraStrio <- subset(strio, strio$site == "Evora")
ToledoChir <- subset(chir, chir$site == "Toledo")
ToledoDipt <- subset(dipt, dipt$site == "Toledo")
ToledoStrio <- subset(strio, strio$site == "Toledo")
PortoChir <- subset(chir, chir$site == "Porto")
PortoDipt <- subset(dipt, dipt$site == "Porto")
PortoStrio <- subset(strio, strio$site == "Porto")

#define merged file
BootRunsEChir <- read.csv("../Results/SchoolField/Boots/Chironomus/Evora/mergedChirEvo.csv")
BootRunsEDipt <- read.csv("../Results/SchoolField/Boots/Cloeon/Evora/mergedDiptEvo.csv")
BootRunsEStrio <- read.csv("../Results/SchoolField/Boots/Sympetrum/Evora/mergedStrioEvo.csv")
BootRunsPChir <- read.csv("../Results/SchoolField/Boots/Chironomus/Porto/mergedChirPor.csv")
BootRunsPDipt <- read.csv("../Results/SchoolField/Boots/Cloeon/Porto/mergedDiptPor.csv")
BootRunsPStrio <- read.csv("../Results/SchoolField/Boots/Sympetrum/Porto/mergedStrioPor.csv")
BootRunsTChir <- read.csv("../Results/SchoolField/Boots/Chironomus/Toledo/mergedChirTol.csv")
BootRunsTDipt <- read.csv("../Results/SchoolField/Boots/Cloeon/Toledo/mergedDiptTol.csv")
BootRunsTStrio <- read.csv("../Results/SchoolField/Boots/Sympetrum/Toledo/mergedStrioTol.csv")

CIs <- function(variables,variables2) {
  BootModels <- c()
  loc.spp <- variables2
  Tempe <- seq(from=283.15, to = 318.15, by = 0.5)
  Masss1 <- seq(from = as.numeric(min(loc.spp$Mass)), to = as.numeric(max(loc.spp$Mass)), length.out = length(Tempe))
  
  BootRuns <- variables
  for(i in unique(BootRuns$boot_num)){
    params <- subset(BootRuns, BootRuns$boot_num == i)
    B0 <- params[params$term == "B0",]$estimate
    b <- params[params$term == "b",]$estimate
    E <- params[params$term == "E",]$estimate
    Ed <- params[params$term == "Ed",]$estimate
    TempH <- params[params$term == "TempH",]$estimate
    if (variables2$site[1]=="Evora") {
      curve <- SchoolfieldMEvo(B0, b, m = Masss1, E, Ed, TempH, Temp = Tempe)  
    }
    if (variables2$site[1]=="Toledo") {
      curve <- SchoolfieldMTol(B0, b, m = Masss1, E, Ed, TempH, Temp = Tempe)  
    }
    if (variables2$site[1]=="Porto") {
      curve <- SchoolfieldMPor(B0, b, m = Masss1, E, Ed, TempH, Temp = Tempe)  
    }
    bootN <- rep(i,length(curve))
    Temperature <- seq(from = 10, to = 45, length.out = length(curve))
    output <- cbind(curve,bootN,Temperature)
    BootModels <- rbind(output,BootModels)
  }
  BootModels <- as.data.frame(BootModels)
  boot1_conf_preds <- group_by(BootModels, Temperature) %>%
    summarise(conf_lower = quantile(curve, 0.025),
              conf_upper = quantile(curve, 0.975)) %>%
    ungroup()
  return(boot1_conf_preds)
}

EvoraCIsChir <- CIs(BootRunsEChir,EvoraChir)
ToledoCIsChir <- CIs(BootRunsTChir,ToledoChir)
PortoCIsChir <- CIs(BootRunsPChir,PortoChir)

EvoraCIsDipt <- CIs(BootRunsEDipt,EvoraDipt)
ToledoCIsDipt <- CIs(BootRunsTDipt,ToledoDipt)
PortoCIsDipt <- CIs(BootRunsPDipt,PortoDipt)


EvoraCIsStrio <- CIs(BootRunsEStrio,EvoraStrio)
ToledoCIsStrio <- CIs(BootRunsTStrio,ToledoStrio)
PortoCIsStrio <- CIs(BootRunsPStrio,PortoStrio)

allCIs <- rbind(EvoraCIsChir,ToledoCIsChir,PortoCIsChir,
                EvoraCIsDipt,ToledoCIsDipt,PortoCIsDipt,
                EvoraCIsStrio,ToledoCIsStrio,PortoCIsStrio)

allCIs <- data.frame(allCIs)
allCIs$site <- c(rep(c(rep("Evora",71),rep("Toledo",71),rep("Porto",71)),3))
allCIs$genus <- c(rep("Chironomus",213),rep("Cloeon",213),rep("Sympetrum",213))

write.csv(allCIs,"../Results/SchoolField/ModelCIs.csv")
