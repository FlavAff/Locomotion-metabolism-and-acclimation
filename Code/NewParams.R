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

fitmodelplz <- function(dataset) {
  if (dataset$site[1] == "Evora") {
    fit<-nls_multstart(LogC ~ SchoolfieldMEvo(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                       data= dataset,  iter = 6000,
                       start_lower = c(0.01, 0.1, 0.2, 1, 0), #set lower bound of starting points in paramaters
                       start_upper = c(2, 2, 3, 40, 353.15), #set upper bound of starting points in paramaters
                       supp_errors = 'Y',
                       convergence_count = 1000,
                       na.action = na.omit,
                       lower=c(0.01, 0.5, 0.5, 2, 300)) #set lower bound of parameter values acceptable
  }
  if (dataset$site[1] == "Toledo") {
    fit<-nls_multstart(LogC ~ SchoolfieldMTol(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                       data= dataset,  iter = 6000,
                       start_lower = c(0.01, 0.1, 0.2, 1, 0), #set lower bound of starting points in paramaters
                       start_upper = c(2, 2, 3, 40, 353.15), #set upper bound of starting points in paramaters
                       supp_errors = 'Y',
                       convergence_count = 1000,
                       na.action = na.omit,
                       lower=c(0.01, 0.5, 0.5, 2, 300)) #set lower bound of parameter values acceptable

  }
  if (dataset$site[1] == "Porto") {
    fit<-nls_multstart(LogC ~ SchoolfieldMPor(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                       data= dataset,  iter = 6000,
                       start_lower = c(0.01, 0.1, 0.2, 1, 0), #set lower bound of starting points in paramaters
                       start_upper = c(2, 2, 3, 40, 353.15), #set upper bound of starting points in paramaters
                       supp_errors = 'Y',
                       convergence_count = 1000,
                       na.action = na.omit,
                       lower=c(0.01, 0.5, 0.5, 2, 300)) #set lower bound of parameter values acceptable
  }
  
  # get parameters ####                   
  params_fit<-tidy(fit) # tidy version of model outputs
  summary_fit<-glance(fit) # summary of model fit
  preds_fit<- augment(fit) #predicted values for plotting
  return(params_fit)
}

ChirEvora <- subset(chir, chir$site == "Evora")
DiptEvora <- subset(dipt, dipt$site == "Evora")
StrioEvora <- subset(strio, strio$site == "Evora")
ChirToledo <- subset(chir, chir$site == "Toledo")
DiptToledo <- subset(dipt, dipt$site == "Toledo")
StrioToledo <- subset(strio, strio$site == "Toledo")
ChirPorto <- subset(chir, chir$site == "Porto")
DiptPorto <- subset(dipt, dipt$site == "Porto")
StrioPorto <- subset(strio, strio$site == "Porto")

ChirEvoraFit <- fitmodelplz(ChirEvora)
ChirToledoFit <- fitmodelplz(ChirToledo)
ChirPortoFit <- fitmodelplz(ChirPorto)
DiptEvoraFit <- fitmodelplz(DiptEvora)
DiptToledoFit <- fitmodelplz(DiptToledo)
DiptPortoFit <- fitmodelplz(DiptPorto)
StrioEvoraFit <- fitmodelplz(StrioEvora)
StrioToledoFit <- fitmodelplz(StrioToledo)
StrioPortoFit <- fitmodelplz(StrioPorto)

allparams <- rbind(ChirEvoraFit,ChirToledoFit,ChirPortoFit,
                   DiptEvoraFit,DiptToledoFit,DiptPortoFit,
                   StrioEvoraFit,StrioToledoFit,StrioPortoFit)

allparams <- data.frame(allparams)
allparams$site <- c(rep(c(rep("Evora",5),rep("Toledo",5),rep("Porto",5)),3))
allparams$genus <- c(rep("Chironomus",15),rep("Cloeon",15),rep("Sympetrum",15))

write.csv(allparams,"../Results/SchoolField/ModelParameters.csv")
