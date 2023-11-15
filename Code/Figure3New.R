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

spp <- "Sympetrum"

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
    
    # get parameters ####                   
    params_fit<-tidy(fit) # tidy version of model outputs
    summary_fit<-glance(fit) # summary of model fit
    preds_fit<- augment(fit) #predicted values for plotting
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
    
    # get parameters ####                   
    params_fit<-tidy(fit) # tidy version of model outputs
    summary_fit<-glance(fit) # summary of model fit
    preds_fit<- augment(fit) #predicted values for plotting
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
    
    # get parameters ####                   
    params_fit<-tidy(fit) # tidy version of model outputs
    summary_fit<-glance(fit) # summary of model fit
    preds_fit<- augment(fit) #predicted values for plotting
  }
  rss <- sum((dataset$LogC - preds_fit$.fitted)^2)
  tss <- sum((dataset$LogC-mean(dataset$LogC))^2)
  Rsq <- 1 - (rss/tss)
  print(Rsq)
  return(params_fit)
}

#Evora <- subset(chir, chir$site == "Evora")
#Evora <- subset(dipt, dipt$site == "Evora")
Evora <- subset(strio, strio$site == "Evora")
#Toledo <- subset(chir, chir$site == "Toledo")
#Toledo <- subset(dipt, dipt$site == "Toledo")
Toledo <- subset(strio, strio$site == "Toledo")
#Porto <- subset(chir, chir$site == "Porto")
#Porto <- subset(dipt, dipt$site == "Porto")
Porto <- subset(strio, strio$site == "Porto")

EvoraFit <- fitmodelplz(Evora)
ToledoFit <- fitmodelplz(Toledo)
PortoFit <- fitmodelplz(Porto)


predictions <- function(dataset1,dataset2) {
  Tempe <- seq(from=283.15, to = 318.15, by = 0.5)
  loc.spp <- dataset1
  Masss1 <- seq(from = as.numeric(min(loc.spp$Mass)), to = as.numeric(max(loc.spp$Mass)), length.out = length(Tempe))
  params_fit <- dataset2
  if (dataset1$site[1]=="Evora") {
    d1 <- SchoolfieldMEvo(B0 = params_fit$estimate[1], 
                          b = params_fit$estimate[2],
                          m = Masss1, 
                          E = params_fit$estimate[3], 
                          Ed = params_fit$estimate[4], 
                          TempH = params_fit$estimate[5], 
                          Temp = Tempe) 
    Temperature <- seq(from = 10, to = 45, length.out = length(d1))
    Model1 <- data.frame(Temperature, d1)  
  }
  if (dataset1$site[1]=="Toledo") {
    d1 <- SchoolfieldMTol(B0 = params_fit$estimate[1], 
                          b = params_fit$estimate[2],
                          m = Masss1, 
                          E = params_fit$estimate[3], 
                          Ed = params_fit$estimate[4], 
                          TempH = params_fit$estimate[5], 
                          Temp = Tempe) 
    Temperature <- seq(from = 10, to = 45, length.out = length(d1))
    Model1 <- data.frame(Temperature, d1)  
  }
  if (dataset1$site[1]=="Porto") {
    d1 <- SchoolfieldMPor(B0 = params_fit$estimate[1], 
                          b = params_fit$estimate[2],
                          m = Masss1, 
                          E = params_fit$estimate[3], 
                          Ed = params_fit$estimate[4], 
                          TempH = params_fit$estimate[5], 
                          Temp = Tempe) 
    Temperature <- seq(from = 10, to = 45, length.out = length(d1))
    Model1 <- data.frame(Temperature, d1)  
  }
  return(Model1)
}

EvoraPreds <- predictions(Evora,EvoraFit)
ToledoPreds <- predictions(Toledo,ToledoFit)
PortoPreds <- predictions(Porto,PortoFit)



#define merged file
#BootRunsE <- read.csv("../Results/SchoolField/Boots/Chironomus/Evora/mergedChirEvo.csv")
#BootRunsE <- read.csv("../Results/SchoolField/Boots/Cloeon/Evora/mergedDiptEvo.csv")
BootRunsE <- read.csv("../Results/SchoolField/Boots/Sympetrum/Evora/mergedStrioEvo.csv")
#BootRunsP <- read.csv("../Results/SchoolField/Boots/Chironomus/Porto/mergedChirPor.csv")
#BootRunsP <- read.csv("../Results/SchoolField/Boots/Cloeon/Porto/mergedDiptPor.csv")
BootRunsP <- read.csv("../Results/SchoolField/Boots/Sympetrum/Porto/mergedStrioPor.csv")
#BootRunsT <- read.csv("../Results/SchoolField/Boots/Chironomus/Toledo/mergedChirTol.csv")
#BootRunsT <- read.csv("../Results/SchoolField/Boots/Cloeon/Toledo/mergedDiptTol.csv")
BootRunsT <- read.csv("../Results/SchoolField/Boots/Sympetrum/Toledo/mergedStrioTol.csv")

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

#EvoraCIs <- CIs(BootRunsE,Evora)
EvoraCIs <- read_csv("../Results/SchoolField/ModelCIs.csv")
EvoraCIs <- filter(EvoraCIs, genus == spp)
EvoraCIs <- filter(EvoraCIs, site == "Evora")
#ToledoCIs <- CIs(BootRunsT,Toledo)
ToledoCIs <- read_csv("../Results/SchoolField/ModelCIs.csv")
ToledoCIs <- filter(ToledoCIs, genus == spp)
ToledoCIs <- filter(ToledoCIs, site == "Toledo")
#PortoCIs <- CIs(BootRunsP,Porto)
PortoCIs <- read_csv("../Results/SchoolField/ModelCIs.csv")
PortoCIs <- filter(PortoCIs, genus == spp)
PortoCIs <- filter(PortoCIs, site == "Porto")

p <- ggplot() + theme_classic() +
  geom_point(data = Evora, aes(x=chamber.T,y=LogC), colour = "red", size = I(3), alpha = 0.4) +
  geom_line(data = EvoraPreds, aes(x=Temperature,y=d1), colour = "red", size = I(2), alpha = 1)+
  geom_ribbon(aes(Temperature, ymin = conf_lower, ymax = conf_upper), EvoraCIs, fill = 'red', alpha = 0.3) +
  geom_point(data = Toledo, aes(x=chamber.T,y=LogC), colour = "blue", size = I(3), alpha = 0.4) +
  geom_line(data = ToledoPreds, aes(x=Temperature,y=d1), colour = "blue", size = I(2), alpha = 1)+
  geom_ribbon(aes(Temperature, ymin = conf_lower, ymax = conf_upper), ToledoCIs, fill = 'blue', alpha = 0.3) +
  geom_point(data = Porto, aes(x=chamber.T,y=LogC), colour = "purple", size = I(3), alpha = 0.4) +
  geom_line(data = PortoPreds, aes(x=Temperature,y=d1), colour = "purple", size = I(2), alpha = 1)+
  geom_ribbon(aes(Temperature, ymin = conf_lower, ymax = conf_upper), PortoCIs, fill = 'purple', alpha = 0.3) +
  theme(axis.text=element_text(size=32)) + theme(axis.title = element_blank()) 
p
tiff(paste0("../Results/SchoolField/Boots/",spp,"All.tiff"), width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p)
dev.off()

preds <- rbind(EvoraPreds,ToledoPreds,PortoPreds)
CIs <- rbind(EvoraCIs,ToledoCIs,PortoCIs)
df <- cbind(preds,CIs)

write.csv(df, "../Results/SchoolField/Boots/Sympetrum/StrioPreds.csv")
