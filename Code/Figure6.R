rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(truncnorm)

setwd("~/Documents/MSc/CMEECourseWork/Project/Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("further_models_fun.R")
source("a_plot_fun2.R")
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
p1 <- SiteSearchRate.Vopt4.version1("Porto",strio,dipt,"Sympetrum","Cloeon", maxi = 3)
p2 <- SiteSearchRate.Vopt4.version1("Evora",strio,dipt,"Sympetrum","Cloeon", maxi = 3)
SiteSearchRate.Vopt4.version1("Porto",strio,dipt,"Sympetrum","Chironomus", maxi = 1.5)
#SiteSearchRate.Vopt4.version1("Toledo",strio,dipt,"Sympetrum","Chironomus", maxi = 0.65)
SiteSearchRate.Vopt4.version1("Evora",strio,dipt,"Sympetrum","Chironomus", maxi = 1.5)
p4 <- SiteSearchRate.Vopt4.version1("Evora",strio,chir,"Sympetrum","Chironomus", maxi = 1.5)
p3 <- SiteSearchRate.Vopt4.version1("Porto",strio,chir,"Sympetrum","Chironomus", maxi = 1.5)
