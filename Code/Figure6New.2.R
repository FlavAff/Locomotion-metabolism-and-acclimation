rm(list = ls())
dev.off()

library(tidyverse)

setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("multiplot.R")
source("further_models_fun.R")

params <- read_csv("../Results/SchoolField/ModelParameters.csv")

Temp <- seq(from = 273.15, to = 318.15, length.out = 200)
Pd2 <- 0.68
Pd3 <- 1.05
d0 <- 0.3
i <- 1
c <- 1

searchrates <- function(loc) {
  if (loc == "Toledo") {
    Tref <- 14.6 + 273.15
  }
  if (loc == "Porto") {
    Tref <- 16.9 + 273.15
  }
  if (loc == "Evora") {
    Tref <- 17.3 + 273.15
  }
  PreyParams1 <- params %>% filter(site==loc) %>% filter(genus=="Chironomus")
  PreyParams2 <- params %>% filter(site==loc) %>% filter(genus=="Cloeon")
  PredParams <- params %>% filter(site==loc) %>% filter(genus=="Sympetrum")
  
  mr1 <- mean(chir$Mass)
  mr2 <- mean(dipt$Mass)
  mc <- mean(strio$Mass)
  
  bc <- PredParams[PredParams$term=="b",]$estimate
  br1 <- PreyParams1[PreyParams1$term=="b",]$estimate
  br2 <- PreyParams2[PreyParams2$term=="b",]$estimate
  
  Ec <- PredParams[PredParams$term=="E",]$estimate
  Er1 <- PreyParams1[PreyParams1$term=="E",]$estimate
  Er2 <- PreyParams2[PreyParams2$term=="E",]$estimate
  
  v0c <- Vopt2(power(PredParams[PredParams$term=="B0",]$estimate),mc)
  v0r1 <- Vopt2(power(PreyParams1[PreyParams1$term=="B0",]$estimate),mr1)
  v0r2 <- Vopt2(power(PreyParams2[PreyParams2$term=="B0",]$estimate),mr2)

  sessile2D <- sessile.search.rate.2D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, mr = mr1, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c, Tref = Tref)
  active3D <- active.search.rate.3D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, b0r = v0r2, mr = mr2, Er = Er2, br = br2, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c, Tref = Tref)
  
  Temperature <- Temp - 273.15
  strats <- data.frame(Temperature,sessile2D,active3D)
  
  p <- ggplot() + theme_classic() +
    theme(axis.text = element_text(size=32)) +
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
    p <- p + geom_line(data = strats, aes(x = Temperature, y = sessile2D, colour = "Sessile 2D"), size = I(3), alpha = 1)
    p <- p + geom_line(data = strats, aes(x = Temperature, y = active3D, colour = "Active 3D"), size = I(3), alpha = 1)
    p <- p + xlim(10,45) + scale_color_manual(values=c("#FF5C4D","#DAD870"), name = "Strategy") + ylim(0,0.05)
    p <- p + theme(axis.title.x=element_blank()) + theme(legend.position = "none") + theme(axis.title.y=element_blank())
    p <- p + theme(legend.text = element_text(size = 22),legend.title = element_text(size=24))
  return(p)
}


pTol <- searchrates("Toledo")+ 
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="blue", size = I(2), alpha = 0.2)
pPor <- searchrates("Porto")+ 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="purple", size = I(2), alpha = 0.2) +
  theme(axis.text.y = element_blank())
pEvo <- searchrates("Evora")+
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="red", size = I(2), alpha = 0.2)+
  theme(axis.text.y = element_blank())


multiplot(pTol, pPor, pEvo, cols = 3)

tiff("../Results/SchoolField/combineda2.tiff", width = 60, height = 20, units = 'cm', res = 300, compression = 'lzw')
multiplot(pTol, pPor, pEvo, cols = 3)
dev.off()

