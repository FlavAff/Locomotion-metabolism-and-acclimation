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

searchrates <- function(preydat,preddat,preyname,loc,maxi) {
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
  
  #print(paste("v0 ratio is",(v0r/v0c)^2))
  #print(paste("m ratio is",((mr^(br))/(mc^(bc)))^2))
  #print(paste("E & T exp ratios are",exp(2/k*(1/Temp-1/Tref)*(Ec-Er))))
  #print(paste("temps are",2/k*(1/Temp-1/Tref)))
  #print(paste("Front bit",v0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/Tref))))
  #print(paste("total relative velocity is",sqrt(1 + (i*c*(v0r/v0c))^2 * ((mr^(br))/(mc^(bc)))^2 * exp(2/k*(1/Temp-1/Tref)*(Ec-Er)))))
  
  sessile2D <- sessile.search.rate.2D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c, Tref = Tref)
  sessile3D <- sessile.search.rate.3D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c, Tref = Tref)
  active2D <- active.search.rate.2D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, b0r = v0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c, Tref = Tref)
  active3D <- active.search.rate.3D(b0c = v0c, mc = mc, Ec = Ec, bc = bc, b0r = v0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c, Tref = Tref)
  
  Temperature <- Temp - 273.15
  strats <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  
  p <- ggplot() + theme_classic() +
    theme(axis.text = element_text(size=22)) +
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  if (preyname == "Chironomus") {
    p <- p + geom_line(data = strats, aes(x = Temperature, y = sessile2D, colour = "Sessile 2D"), size = I(3), alpha = 1)
    p <- p + geom_line(data = strats, aes(x = Temperature, y = sessile3D, colour = "Sessile 3D"), size = I(2), alpha = 0.4)  
    p <- p + xlim(10,45) + ylim(0,0.03) + scale_color_manual(values=c("#DAD870","#FFCD58"), name = "Strategy")
  }
  if (preyname == "Cloeon") {
    p <- p + geom_line(data = strats, aes(x = Temperature, y = active2D, colour = "Active 2D"), size = I(2), alpha = 0.4)
    p <- p + geom_line(data = strats, aes(x = Temperature, y = active3D, colour = "Active 3D"), size = I(3), alpha = 1)
    p <- p + xlim(10,45) + ylim(0,0.15) + scale_color_manual(values=c("#FF9636","#FF5C4D"), name = "Strategy")
  }
  p <- p + theme(axis.title.x=element_blank()) + theme(legend.position = "none") + theme(axis.title.y=element_blank())
  p <- p + theme(legend.text = element_text(size = 22),legend.title = element_text(size=24))
  return(p)
}


pTolCh<- searchrates(chir,strio,"Chironomus","Toledo")+ 
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="blue", size = I(2), alpha = 0.2)
pPorCh <- searchrates(chir,strio,"Chironomus","Porto")+ 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="purple", size = I(2), alpha = 0.2) +
  theme(axis.text.y = element_blank())
pEvoCh <- searchrates(chir,strio,"Chironomus","Evora")+
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="red", size = I(2), alpha = 0.2)+
  theme(axis.text.y = element_blank())

pTolCl <- searchrates(dipt,strio,"Cloeon","Toledo")+ 
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="blue", size = I(2), alpha = 0.2)
pPorCl <- searchrates(dipt,strio,"Cloeon","Porto")+ 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="purple", size = I(2), alpha = 0.2)+
  theme(axis.text.y = element_blank())
pEvoCl <- searchrates(dipt,strio,"Cloeon","Evora")+
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="red", size = I(2), alpha = 0.2)+
  theme(axis.text.y = element_blank())

multiplot(pTolCh, pTolCl, pPorCh, pPorCl, pEvoCh, pEvoCl, cols = 3)

tiff("../Results/SchoolField/combineda.tiff", width = 60, height = 30, units = 'cm', res = 300, compression = 'lzw')
multiplot(pTolCh, pTolCl, pPorCh, pPorCl, pEvoCh, pEvoCl, cols = 3)
dev.off()

