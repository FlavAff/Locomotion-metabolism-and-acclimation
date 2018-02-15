rm(list = ls())
#dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)

setwd("~/Documents/MSc/CMEECourseWork/Project/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))

spp <- rbind(chir,dipt,strio)
mur <- subset(spp, spp$site == "Murcia")
tol <- subset(spp, spp$site == "Toledo")
evo <- subset(spp, spp$site == "Evora")
por <- subset(spp, spp$site == "Porto")
jac <- subset(spp, spp$site == "Jaca")
pen <- subset(spp, spp$site == "Penalara")
rm(spp)

values <- read.csv("../Results/SchoolField/Revised/UsedValues15.csv")
values$site <- factor(values$site, levels = c("Penalara","Jaca","Toledo","Evora","Porto","Murcia"))
source("Schoolfields2.R")
source("plot_functions.R")
source("further_models_fun.R")

#get the parameter values per site for the functions
mur_values <- subset(values, values$site == "Murcia")
tol_values <- subset(values, values$site == "Toledo")
evo_values <- subset(values, values$site == "Evora")
por_values <- subset(values, values$site == "Porto")
jac_values <- subset(values, values$site == "Jaca")
pen_values <- subset(values, values$site == "Penalara")

#plot the curves with points
Plot_per_site_points(mur,mur_values,c("Cloeon"))
Plot_per_site_points(tol,tol_values,c("Chironomus","Cloeon","Sympetrum"))
Plot_per_site_points(evo,evo_values,c("Chironomus","Cloeon","Sympetrum"))
Plot_per_site_points(por,por_values,c("Chironomus","Cloeon","Sympetrum"))
Plot_per_site_points(jac,jac_values,c("Sympetrum"))
Plot_per_site_points(pen,pen_values,c("Chironomus"))

#plot the curves alone
Plot_per_site_curve(mur,mur_values,c("Cloeon"))
Plot_per_site_curve(tol,tol_values,c("Chironomus","Cloeon","Sympetrum"))
Plot_per_site_curve(evo,evo_values,c("Chironomus","Cloeon","Sympetrum"))
Plot_per_site_curve(por,por_values,c("Chironomus","Cloeon","Sympetrum"))
Plot_per_site_curve(jac,jac_values,c("Sympetrum"))
Plot_per_site_curve(pen,pen_values,c("Chironomus"))


#get the parameter values per species for the functions
chir_values <- subset(values, values$Genus == "Chironomus")
dipt_values <- subset(values, values$Genus == "Cloeon")
stri_values <- subset(values, values$Genus == "Sympetrum")

#plot the species curves
Plot_four_curves(chir,c("Penalara","Toledo","Evora","Porto"),chir_values) 
#c(rgb(0,.7,1,.7),rgb(0,.8,0,.7),rgb(0,.4,0,.7),rgb(1,.8,0,.7))
#c("#0097E5","#2FE203","#B8E104","#E08205")
#annotate x=10, y=1.5
Plot_four_curves(dipt,c("Toledo","Evora","Porto","Murcia"),dipt_values) 
#c(rgb(0,.8,0,.7),rgb(0,.4,0,.7),rgb(1,.8,0,.7),rgb(1,.2,0,.7))
#c("#2FE203","#B8E104","#E08205","#DF0611")
#annotate x=12, y=3
Plot_four_curves(strio,c("Jaca","Toledo","Evora","Porto"),stri_values) 
#c(rgb(0,.8,0,.7),rgb(0,.4,0,.7),rgb(1,.8,0,.7),rgb(0,.2,1,.7))
#c("#2FE203","#B8E104","#E08205","#012BE3")
#annotate x=17, y=12

#colours <- c("#0097E5","#012BE3","#2FE203","#B8E104","#E08205","#DF0611")

Plot_four_curves_log(chir,c("Toledo","Evora","Porto","Penalara"),chir_values)
Plot_four_curves_log(dipt,c("Murcia","Toledo","Evora","Porto"),dipt_values)
Plot_four_curves_log(strio,c("Toledo","Evora","Porto","Jaca"),stri_values)

#plot the predator-prey curves
Plot_two_curves(spp1 = strio,spp2 = chir,locations = c("Toledo","Evora","Porto"),stri_values,chir_values)
Plot_two_curves(spp1 = strio,spp2 = dipt,locations = c("Toledo","Evora","Porto"),stri_values,dipt_values)

#get values for v0 transformation
chir_GCOT4 <- GCOT4(mean(chir$Mass)/1000)
dipt_GCOT4 <- GCOT4(mean(dipt$Mass)/1000)
stri_GCOT4 <- GCOT4(mean(strio$Mass)/1000)

chir_m <- mean(chir$Mass)/1000
dipt_m <- mean(dipt$Mass)/1000
stri_m <- mean(strio$Mass)/1000

#plot the predator-prey velocity curves
Plot_two_curves_v0(spp1 = strio,spp2 = chir,locations = c("Toledo","Evora","Porto"),stri_values,chir_values,stri_GCOT4,chir_GCOT4,stri_m,chir_m,"orange")
Plot_two_curves_v0(spp1 = strio,spp2 = dipt,locations = c("Toledo","Evora","Porto"),stri_values,dipt_values,stri_GCOT4,dipt_GCOT4,stri_m,dipt_m,"pink")
