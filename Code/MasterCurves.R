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
source("Schoolfields.R")
source("GetParas.R")
source("Startcalls.R")

#set the number of starting values to sample
tries <- 10000

#Get the estimated paramters for both mass corrected and raw oxygen consumption from Arrhenius first
ChirToNo <- ToledoNo_strt(chir)
ChirEvNo <- EvoraNo_strt(chir)
ChirPoNo <- PortoNo_strt(chir)
ChirPeNo <- PenalaraNo_strt(chir)
ChirToM <- ToledoM_strt(chir)
ChirEvM <- EvoraM_strt(chir)
ChirPoM <- PortoM_strt(chir)
ChirPeM <- PenalaraM_strt(chir)

DiptMuNo <- MurciaNo_strt(dipt)
DiptToNo <- ToledoNo_strt(dipt)
DiptEvNo <- EvoraNo_strt(dipt)
DiptPoNo <- PortoNo_strt(dipt)
DiptMuM <- MurciaM_strt(dipt)
DiptToM <- ToledoM_strt(dipt)
DiptEvM <- EvoraM_strt(dipt)
DiptPoM <- PortoM_strt(dipt)

StriToNo <- ToledoNo_strt(strio)
StriEvNo <- EvoraNo_strt(strio)
StriPoNo <- PortoNo_strt(strio)
StriJaNo <- JacaNo_strt(strio)
StriToM <- ToledoM_strt(strio)
StriEvM <- EvoraM_strt(strio)
StriPoM <- PortoM_strt(strio)
StriJaM <- JacaM_strt(strio)


#Get the sample of parameters

ChirToNoR <- Start_random(ChirToNo, tries)
ChirEvNoR <- Start_random(ChirEvNo, tries)
ChirPoNoR <- Start_random(ChirPoNo, tries)
ChirPeNoR <- Start_random(ChirPeNo, tries)
ChirToMR <- Start_random(ChirToM, tries)
ChirEvMR <- Start_random(ChirEvM, tries)
ChirPoMR <- Start_random(ChirPoM, tries)
ChirPeMR <- Start_random(ChirPeM, tries)

DiptMuNoR <- Start_random(DiptMuNo, tries)
DiptToNoR <- Start_random(DiptToNo, tries)
DiptEvNoR <- Start_random(DiptEvNo, tries)
DiptPoNoR <- Start_random(DiptPoNo, tries)
DiptMuMR <- Start_random(DiptMuM, tries)
DiptToMR <- Start_random(DiptToM, tries)
DiptEvMR <- Start_random(DiptEvM, tries)
DiptPoMR <- Start_random(DiptPoM, tries)

StriToNoR <- Start_random(StriToNo, tries)
StriEvNoR <- Start_random(StriEvNo, tries)
StriPoNoR <- Start_random(StriPoNo, tries)
StriJaNoR <- Start_random(StriJaNo, tries)
StriToMR <- Start_random(StriToM, tries)
StriEvMR <- Start_random(StriEvM, tries)
StriPoMR <- Start_random(StriPoM, tries)
StriJaMR <- Start_random(StriJaM, tries)

#Run SchoolfieldNo for raw consumptions

ChirToNoSc <- Fit_NoSchoolfield(tries,ChirToNoR,subset(chir, chir$site == "Toledo"),13.4655318+273.15)
ChirEvNoSc <- Fit_NoSchoolfield(tries,ChirEvNoR,subset(chir, chir$site == "Evora"),14.91715089+273.15)
ChirPoNoSc <- Fit_NoSchoolfield(tries,ChirPoNoR,subset(chir, chir$site == "Porto"),12.6509693+273.15)
ChirPeNoSc <- Fit_NoSchoolfield(tries,ChirPeNoR,subset(chir, chir$site == "Penalara"),7.3462619+273.15)

DiptMuNoSc <- Fit_NoSchoolfield(tries,DiptMuNoR,subset(dipt, dipt$site == "Murcia"),15.605+273.15)
DiptToNoSc <- Fit_NoSchoolfield(tries,DiptToNoR,subset(dipt, dipt$site == "Toledo"),13.4655318+273.15)
DiptEvNoSc <- Fit_NoSchoolfield(tries,DiptEvNoR,subset(dipt, dipt$site == "Evora"),14.91715089+273.15)
DiptPoNoSc <- Fit_NoSchoolfield(tries,DiptPoNoR,subset(dipt, dipt$site == "Porto"),12.6509693+273.15)

StriToNoSc <- Fit_NoSchoolfield(tries,StriToNoR,subset(strio, strio$site == "Toledo"),13.4655318+273.15)
StriEvNoSc <- Fit_NoSchoolfield(tries,StriEvNoR,subset(strio, strio$site == "Evora"),14.91715089+273.15)
StriPoNoSc <- Fit_NoSchoolfield(tries,StriPoNoR,subset(strio, strio$site == "Porto"),12.6509693+273.15)
StriJaNoSc <- Fit_NoSchoolfield(tries,StriJaNoR,subset(strio, strio$site == "Jaca"),11.04755546+273.15)

#Run SchoolfieldNo for mass corrected

ChirToNoSc.corr <- Fit_NoSchoolfield.corr(tries,ChirToNoR,subset(chir, chir$site == "Toledo"),13.4655318+273.15)
ChirEvNoSc.corr <- Fit_NoSchoolfield.corr(tries,ChirEvNoR,subset(chir, chir$site == "Evora"),14.91715089+273.15)
ChirPoNoSc.corr <- Fit_NoSchoolfield.corr(tries,ChirPoNoR,subset(chir, chir$site == "Porto"),12.6509693+273.15)
ChirPeNoSc.corr <- Fit_NoSchoolfield.corr(tries,ChirPeNoR,subset(chir, chir$site == "Penalara"),7.3462619+273.15)

DiptMuNoSc.corr <- Fit_NoSchoolfield.corr(tries,DiptMuNoR,subset(dipt, dipt$site == "Murcia"),15.605+273.15)
DiptToNoSc.corr <- Fit_NoSchoolfield.corr(tries,DiptToNoR,subset(dipt, dipt$site == "Toledo"),13.4655318+273.15)
DiptEvNoSc.corr <- Fit_NoSchoolfield.corr(tries,DiptEvNoR,subset(dipt, dipt$site == "Evora"),14.91715089+273.15)
DiptPoNoSc.corr <- Fit_NoSchoolfield.corr(tries,DiptPoNoR,subset(dipt, dipt$site == "Porto"),12.6509693+273.15)

StriToNoSc.corr <- Fit_NoSchoolfield.corr(tries,StriToNoR,subset(strio, strio$site == "Toledo"),13.4655318+273.15)
StriEvNoSc.corr <- Fit_NoSchoolfield.corr(tries,StriEvNoR,subset(strio, strio$site == "Evora"),14.91715089+273.15)
StriPoNoSc.corr <- Fit_NoSchoolfield.corr(tries,StriPoNoR,subset(strio, strio$site == "Porto"),12.6509693+273.15)
StriJaNoSc.corr <- Fit_NoSchoolfield.corr(tries,StriJaNoR,subset(strio, strio$site == "Jaca"),11.04755546+273.15)

#Run SchoolfieldM for raw consumption

ChirToMSc <- Fit_MSchoolfield(tries,ChirToMR,subset(chir, chir$site == "Toledo"),13.4655318+273.15)
ChirEvMSc <- Fit_MSchoolfield(tries,ChirEvMR,subset(chir, chir$site == "Evora"),14.91715089+273.15)
ChirPoMSc <- Fit_MSchoolfield(tries,ChirPoMR,subset(chir, chir$site == "Porto"),12.6509693+273.15)
ChirPeMSc <- Fit_MSchoolfield(tries,ChirPeMR,subset(chir, chir$site == "Penalara"),7.3462619+273.15)

DiptMuMSc <- Fit_MSchoolfield(tries,DiptMuMR,subset(dipt, dipt$site == "Murcia"),15.605+273.15)
DiptToMSc <- Fit_MSchoolfield(tries,DiptToMR,subset(dipt, dipt$site == "Toledo"),13.4655318+273.15)
DiptEvMSc <- Fit_MSchoolfield(tries,DiptEvMR,subset(dipt, dipt$site == "Evora"),14.91715089+273.15)
DiptPoMSc <- Fit_MSchoolfield(tries,DiptPoMR,subset(dipt, dipt$site == "Porto"),12.6509693+273.15)

StriToMSc <- Fit_MSchoolfield(tries,StriToMR,subset(strio, strio$site == "Toledo"),13.4655318+273.15)
StriEvMSc <- Fit_MSchoolfield(tries,StriEvMR,subset(strio, strio$site == "Evora"),14.91715089+273.15)
StriPoMSc <- Fit_MSchoolfield(tries,StriPoMR,subset(strio, strio$site == "Porto"),12.6509693+273.15)
StriJaMSc <- Fit_MSchoolfield(tries,StriJaMR,subset(strio, strio$site == "Jaca"),11.04755546+273.15)

#rm(list=setdiff(ls(), c("ChirEvMSc","ChirPeMSc","ChirPoMSc","ChirToMSc","DiptEvMSc","DiptToMSc","DiptPoMSc","DiptMuMSc","StriToMSc","StriEvMSc","StriPoMSc","StriJaMSc")))

#No mass model selection
ChirToNoParas <- Select_startvalNo(ChirToNoSc)
ChirEvNoParas <- Select_startvalNo(ChirEvNoSc)
ChirPoNoParas <- Select_startvalNo(ChirPoNoSc)
ChirPeNoParas <- Select_startvalNo(ChirPeNoSc)

DiptMuNoParas <- Select_startvalNo(DiptMuNoSc)
DiptToNoParas <- Select_startvalNo(DiptToNoSc)
DiptEvNoParas <- Select_startvalNo(DiptEvNoSc)
DiptPoNoParas <- Select_startvalNo(DiptPoNoSc)

StriToNoParas <- Select_startvalNo(StriToNoSc)
StriEvNoParas <- Select_startvalNo(StriEvNoSc)
StriPoNoParas <- Select_startvalNo(StriPoNoSc)
StriJaNoParas <- Select_startvalNo(StriJaNoSc)


#Mass corrected model selection
ChirToNoParas.corr <- Select_startvalNo(ChirToNoSc.corr)
ChirEvNoParas.corr <- Select_startvalNo(ChirEvNoSc.corr)
ChirPoNoParas.corr <- Select_startvalNo(ChirPoNoSc.corr)
ChirPeNoParas.corr <- Select_startvalNo(ChirPeNoSc.corr)

DiptMuNoParas.corr <- Select_startvalNo(DiptMuNoSc.corr)
DiptToNoParas.corr <- Select_startvalNo(DiptToNoSc.corr)
DiptEvNoParas.corr <- Select_startvalNo(DiptEvNoSc.corr)
DiptPoNoParas.corr <- Select_startvalNo(DiptPoNoSc.corr)

StriToNoParas.corr <- Select_startvalNo(StriToNoSc.corr)
StriEvNoParas.corr <- Select_startvalNo(StriEvNoSc.corr)
StriPoNoParas.corr <- Select_startvalNo(StriPoNoSc.corr)
StriJaNoParas.corr <- Select_startvalNo(StriJaNoSc.corr)


#Mass scaling model selection
ChirToMParas <- Select_startvalM(ChirToMSc)
ChirEvMParas <- Select_startvalM(ChirEvMSc)
ChirPoMParas <- Select_startvalM(ChirPoMSc)
ChirPeMParas <- Select_startvalM(ChirPeMSc)

DiptMuMParas <- Select_startvalM(DiptMuMSc)
DiptToMParas <- Select_startvalM(DiptToMSc)
DiptEvMParas <- Select_startvalM(DiptEvMSc)
DiptPoMParas <- Select_startvalM(DiptPoMSc)

StriToMParas <- Select_startvalM(StriToMSc)
StriEvMParas <- Select_startvalM(StriEvMSc)
StriPoMParas <- Select_startvalM(StriPoMSc)
StriJaMParas <- Select_startvalM(StriJaMSc)


#Save the data in a new dataframe
ParasNo <- rbind(ChirToNoParas,ChirEvNoParas,ChirPoNoParas,ChirPeNoParas,DiptMuNoParas,DiptToNoParas,DiptEvNoParas,DiptPoNoParas,StriToNoParas,StriEvNoParas,StriPoNoParas,StriJaNoParas)
ParasNo.corr <- rbind(ChirToNoParas.corr,ChirEvNoParas.corr,ChirPoNoParas.corr,ChirPeNoParas.corr,DiptMuNoParas.corr,DiptToNoParas.corr,DiptEvNoParas.corr,DiptPoNoParas.corr,StriToNoParas.corr,StriEvNoParas.corr,StriPoNoParas.corr,StriJaNoParas.corr)
ParasM <- rbind(ChirToMParas,ChirEvMParas,ChirPoMParas,ChirPeMParas,DiptMuMParas,DiptToMParas,DiptEvMParas,DiptPoMParas,StriToMParas,StriEvMParas,StriPoMParas,StriJaMParas)

ParasNo$b <- "NA"
ParasNo$b_se <- "NA"
ParasNo.corr$b <- "NA"
ParasNo.corr$b_se <- "NA"
Models <- rbind(ParasNo,ParasNo.corr,ParasM)

info <- data.frame(matrix(ncol = 4, nrow = 36))
info <- rename(info, c("X1" = "Genus","X2" = "species","X3" = "Model", "X4" = "site"))

info[c(1:4,13:16,25:28),1] <- "Chironomus"
info[c(5:8,17:20,29:32),1] <- "Cloeon"
info[c(9:12,21:24,33:36),1] <- "Sympetrum"

info[c(5:8,17:20,29:32),2] <- "dipterum"
info[c(9:12,21:24,33:36),2] <- "striolatum"

info[1:12,3] <- "No Mass"
info[13:24,3] <- "0.75 Mass Corrected"
info[25:36,3] <- "Free Mass Scaling"

info[c(5,17,29),4] <-"Murcia"
info[c(1,6,9,13,18,21,25,30,33),4] <- "Toledo"
info[c(2,7,10,14,19,22,26,31,34),4] <- "Evora"
info[c(3,8,11,15,20,23,27,32,35),4] <- "Porto"
info[c(12,24,36),4] <-"Jaca"
info[c(4,16,28),4] <-"Penalara"


SchoolFieldRuns <- cbind(info,Models)
write.csv(SchoolFieldRuns, "../Results/SchoolField/Revised/SchoolFieldRunsMean.csv")
