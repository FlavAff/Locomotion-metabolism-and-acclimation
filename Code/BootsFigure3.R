setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")

library(minpack.lm)
library(ggplot2)
library(broom)
library(tidyverse)


source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("nls_multstart.R")

spp <- rbind(chir,dipt,strio)

#define location and species
loc <- subset(spp, spp$site == "Toledo")
loc.spp <- subset(loc, loc$genus == "Sympetrum")

#values <- read.csv("../Results/SchoolField/New/UsedValuesMeanNew.csv")
#values$site <- factor(values$site, levels = c("Porto","Toledo","Evora"))

#get the parameter values per species for the functions
#spp_values <- subset(values, values$Genus == "Chironomus")
#loc_values <- subset(spp_values, spp_values$site == "Evora")

rm(list=setdiff(ls(), c("loc.spp","k")))
source("Schoolfields2.R")

fit<-nls_multstart(LogC ~ SchoolfieldMTol(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                   data= loc.spp,  iter = 6000,
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

rss <- sum((loc.spp$LogC - preds_fit$.fitted)^2)
tss <- sum((loc.spp$LogC-mean(loc.spp$LogC))^2)
Rsq <- 1 - (rss/tss)

#d1 <- SchoolfieldM(B0 = loc_values$B0_end, 
#                   b = loc_values$b,
#                   m = Masss1, 
#                   E = loc_values$Ea_end, 
#                   Ed = loc_values$Ed_end, 
#                   TempH = loc_values$Tpk_end, 
                   #TempN = spp_values[spp_values$site == locations[1],]$Site.April.median.temp + 273.15, 
#                   Temp = Tempe)


Tempe <- seq(from=283.15, to = 318.15, by = 0.5)
Masss1 <- seq(from = as.numeric(min(loc.spp$Mass)), to = as.numeric(max(loc.spp$Mass)), length.out = length(Tempe))
d1 <- SchoolfieldMTol(B0 = params_fit$estimate[1], 
                   b = params_fit$estimate[2],
                   m = Masss1, 
                   E = params_fit$estimate[3], 
                   Ed = params_fit$estimate[4], 
                   TempH = params_fit$estimate[5], 
                   #TempN = spp_values[spp_values$site == locations[1],]$Site.April.median.temp + 273.15, 
                   Temp = Tempe)
Temperature1 <- seq(from = 10, to = 45, length.out = length(d1))
Model1 <- data.frame(Temperature1, d1)


#define merged file
BootRuns <- read.csv("../Results/SchoolField/Boots/Sympetrum/Toledo/mergedStrioTol.csv")
# select boot_num where Ed estimate < 20
#bad_runs <- in_csv |> filter(term == "Ed", estimate>20) |> pull(boot_num) |> unique()
#good_runs <- in_csv |> filter(!boot_num %in% bad_runs)

BootModels <- c()
for(i in unique(BootRuns$boot_num)){
  params <- subset(BootRuns, BootRuns$boot_num == i)
  B0 <- params[params$term == "B0",]$estimate
  b <- params[params$term == "b",]$estimate
  E <- params[params$term == "E",]$estimate
  Ed <- params[params$term == "Ed",]$estimate
  TempH <- params[params$term == "TempH",]$estimate
  curve <- SchoolfieldMEvo(B0, b, m = Masss1, E, Ed, TempH, Temp = Tempe)
  bootN <- rep(i,length(curve))
  output <- cbind(curve,bootN,Temperature1)
  print(i)
  
  BootModels <- rbind(output,BootModels)
}
BootModels <- as.data.frame(BootModels)
#BootModels <- read_csv("../Results/SchoolField/Boots/Chironomus/Porto/PredBootsChirPor.csv")

boot1_conf_preds <- group_by(BootModels, Temperature1) %>%
  summarise(conf_lower = quantile(curve, 0.025),
            conf_upper = quantile(curve, 0.975)) %>%
  ungroup()

p <- ggplot() + theme_classic() +
  geom_line(data = BootModels, aes(x=Temperature1, y=curve, group=bootN), colour = "grey", size = I(1), alpha = 0.4) +
  geom_point(data = loc.spp, aes(x=chamber.T,y=LogC), colour = "blue", size = I(1), alpha = 0.4) +
  geom_line(data = Model1, aes(x=Temperature1,y=d1), colour = "red", size = I(1), alpha = 1)+
  #geom_ribbon(aes(Temperature1, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  theme(axis.title=element_text(size=22)) + theme(axis.title = element_blank())
p
tiff("../Results/SchoolField/Boots/Chironomus/ChirEvo.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p)
dev.off()

write.csv(BootModels, "../Results/SchoolField/Boots/Chironomus/Evora/PredBootsChirEvo.csv")
