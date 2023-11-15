rm(list = ls())
dev.off()

library(minpack.lm)
library(tidyverse)
#library(gridExtra)

setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("Schoolfields2.R")
source("further_models_fun.R")
source("multiplot.R")

Chirpreds <- read_csv("../Results/SchoolField/Boots/Chironomus/ChirPreds.csv")
Diptpreds <- read_csv("../Results/SchoolField/Boots/Cloeon/DiptPreds.csv")
Striopreds <- read_csv("../Results/SchoolField/Boots/Sympetrum/StrioPreds.csv")

#values <- read.csv("../Results/SchoolField/ModelParameters.csv")
#values$site <- factor(values$site, levels = c("Toledo","Porto","Evora"))
getmass <- function(spp1,i) {
  Tempe <- seq(from=283.15, to = 318.15, by = 0.5)
  mass <- seq(from = as.numeric(min(spp1[spp1$site == i,]$Mass)), to = as.numeric(max(spp1[spp1$site == i,]$Mass)), length.out = length(Tempe))
  return(mass)
}

Chirpreds$mass <- c(getmass(chir,"Evora"),getmass(chir,"Toledo"),getmass(chir,"Porto"))
Diptpreds$mass <- c(getmass(dipt,"Evora"),getmass(dipt,"Toledo"),getmass(dipt,"Porto"))
Striopreds$mass <- c(getmass(strio,"Evora"),getmass(strio,"Toledo"),getmass(strio,"Porto"))

Chirpreds$speed <- Vopt2(power(Chirpreds$d1),mean(Chirpreds$mass))
Chirpreds$speedupper <- Vopt2(power(Chirpreds$conf_upper),mean(Chirpreds$mass))
Chirpreds$speedlower <- Vopt2(power(Chirpreds$conf_lower),mean(Chirpreds$mass))

Diptpreds$speed <- Vopt2(power(Diptpreds$d1),mean(Diptpreds$mass))
Diptpreds$speedupper <- Vopt2(power(Diptpreds$conf_upper),mean(Diptpreds$mass))
Diptpreds$speedlower <- Vopt2(power(Diptpreds$conf_lower),mean(Diptpreds$mass))

Striopreds$speed <- Vopt2(power(Striopreds$d1),mean(Striopreds$mass))
Striopreds$speedupper <- Vopt2(power(Striopreds$conf_upper),mean(Striopreds$mass))
Striopreds$speedlower <- Vopt2(power(Striopreds$conf_lower),mean(Striopreds$mass))


Chirpreds$Temperature <- rep(seq(from=10, to = 45, by = 0.5),3)
Diptpreds$Temperature <- rep(seq(from=10, to = 45, by = 0.5),3)
Striopreds$Temperature <- rep(seq(from=10, to = 45, by = 0.5),3)



plotsspeeds <- function(pred,prey,loc,preycol) {
  PredS <- filter(pred, site == loc)
  PreyS <- filter(prey, site == loc)
  
  p <- ggplot() + theme_classic() + #ylim(0,1.5) + 
    theme(axis.title=element_text(size=22)) +
    geom_line(data = PreyS, aes(x = Temperature, y = speed), colour = preycol, size = I(2), alpha = 0.7) + 
    geom_ribbon(aes(Temperature, ymin = speedlower, ymax = speedupper), fill = preycol, PreyS, alpha = 0.3) +
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Speed (m/s)"))) +
    geom_line(data = PredS, aes(x = Temperature, y = speed), colour = "orange", size = I(2), alpha = 0.7) +
    geom_ribbon(aes(Temperature, ymin = speedlower, ymax = speedupper), fill = "orange", PredS, alpha = 0.3) +
    theme(axis.title.x=element_blank()) + theme(legend.position = "none") + theme(axis.title.y=element_blank()) +
    theme(axis.text=element_text(size = 32))
  
  return(p)
}



pTolCh <- plotsspeeds(Striopreds,Chirpreds,"Toledo","lightgreen")+ 
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="blue", size = I(2), alpha = 0.2) + ylim(-0.15,0.05)
pPorCh <- plotsspeeds(Striopreds,Chirpreds,"Porto","lightgreen") + 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="purple", size = I(2), alpha = 0.2) + ylim(-0.15,0.05)
pEvoCh <- plotsspeeds(Striopreds,Chirpreds,"Evora","lightgreen") +
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="red", size = I(2), alpha = 0.2) + ylim(-0.15,0.05)

pTolCl <- plotsspeeds(Striopreds,Diptpreds,"Toledo","darkgreen")+ 
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="blue", size = I(2), alpha = 0.2) + ylim(-0.15,0.05)
pPorCl <- plotsspeeds(Striopreds,Diptpreds,"Porto","darkgreen") + 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="purple", size = I(2), alpha = 0.2) + ylim(-0.15,0.05)
pEvoCl <- plotsspeeds(Striopreds,Diptpreds,"Evora","darkgreen") +
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="red", size = I(2), alpha = 0.2) + ylim(-0.15,0.05)

multiplot(pTolCh, pTolCl, pPorCh, pPorCl, pEvoCh, pEvoCl, cols = 3)

tiff("../Results/SchoolField/combinedV0.tiff", width = 60, height = 30, units = 'cm', res = 300, compression = 'lzw')
multiplot(pTolCh, pTolCl, pPorCh, pPorCl, pEvoCh, pEvoCl, cols = 3)
dev.off()






############ ############ ############ ############ ############ ############ ############ ############ ############ ############ 
############ OLD CODE
oldcode <- function(variables) {
  
Plot_two_curves_v0 <- function(spp1,spp2,locations,spp1_values,spp2_values,m1,m2,preycol){
  
  for (i in locations){
    
    #get the predicted values for both models
    Tempe <- seq(from=283.15, to = 318.15, by = 0.5)
    
    Masss1 <- seq(from = as.numeric(min(spp1[spp1$site == i,]$Mass)), to = as.numeric(max(spp1[spp2$site == i,]$Mass)), length.out = length(Tempe))
    Masss2 <- seq(from = as.numeric(min(spp2[spp2$site == i,]$Mass)), to = as.numeric(max(spp2[spp2$site == i,]$Mass)), length.out = length(Tempe))
    
    d1 <- SchoolfieldM(B0 = spp1_values[spp1_values$site == i,]$B0_end, 
                       b = spp1_values[spp1_values$site == i,]$b,
                       m = Masss1,
                       E = spp1_values[spp1_values$site == i,]$Ea_end, 
                       Ed = spp1_values[spp1_values$site == i,]$Ed_end, 
                       TempH = spp1_values[spp1_values$site == i,]$Tpk_end, 
                       #TempN = spp1_values[spp1_values$site == i,]$Site.April.median.temp + 273.15,
                       Temp = Tempe)
    d2 <- SchoolfieldM(B0 = spp2_values[spp2_values$site == i,]$B0_end, 
                       b = spp2_values[spp2_values$site == i,]$b,
                       m = Masss2, 
                       E = spp2_values[spp2_values$site == i,]$Ea_end, 
                       Ed = spp2_values[spp2_values$site == i,]$Ed_end, 
                       TempH = spp2_values[spp2_values$site == i,]$Tpk_end, 
                       #TempN = spp2_values[spp2_values$site == i,]$Site.April.median.temp + 273.15,
                       Temp = Tempe)
    
    Temperature <- seq(from = 10, to = 45, length.out = length(d1))
    
    d1 <- 0.3*Vopt2(power(d1),m1)
    d2 <- 0.3*Vopt2(power(d2),m2)
    
    #create plottable data
    Model1 <- data.frame(Temperature, exp(d1))
    Model2 <- data.frame(Temperature, exp(d2))
    p <- ggplot() + theme_classic() + ylim(0,1.5) + 
      theme(axis.title=element_text(size=22)) +
      geom_line(data = Model1, aes(x = Temperature, y = exp(d1), colour = spp1$genus[1]), size = I(2), alpha = 0.7) + 
      xlab(expression(paste("Temperature (", degree, C, ")"))) + 
      ylab(expression(paste("Speed (m/s)"))) 
    p <- p + geom_line(data = Model2, aes(x = Temperature, y = exp(d2), colour = spp2$genus[1]), size = I(2), alpha = 0.7)
    #p <- p + scale_color_discrete(name = "Genus") #+ ggtitle(paste("Predator - Prey TPCs from",i)) +
    if (locations == "Evora" && preycol == "darkgreen"){
      p <- p + ggtitle("Evora") + theme(title=element_text(size=18)) + theme(plot.title = element_text(hjust=.5))
    }
    p <- p + scale_colour_manual(name="Genus", values=c(preycol,"orange"))
    p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20)) + theme(axis.text=element_text(size=15))
    #theme(plot.title = element_text(face="bold"))
    #open the pdf and put the plot in it
    #pdf(paste0("../Results/SchoolField/Revised/VS",i,spp2$genus[1],"SchoolTPCsv0.pdf"))
    #print(p)
    #dev.off()
    return(p)
    #print(paste("difference in b0 for",i,exp(d1)-exp(d2)))
  }
}

#get values for v0 transformation
chir_GCOT <- GCOT(mean(chir$Mass)/1000)
dipt_GCOT <- GCOT(mean(dipt$Mass)/1000)
stri_GCOT <- GCOT(mean(strio$Mass)/1000)

chir_m <- mean(chir$Mass)/1000
dipt_m <- mean(dipt$Mass)/1000
stri_m <- mean(strio$Mass)/1000

#plot the predator-prey velocity curves
pPorCh <- Plot_two_curves_v0(spp1 = strio,spp2 = chir,locations = c("Porto"),stri_values,chir_values,stri_m,chir_m,"darkgreen")
#remeber to unhash function to get title on plot here:
pEvoCh <- Plot_two_curves_v0(spp1 = strio,spp2 = chir,locations = c("Evora"),stri_values,chir_values,stri_m,chir_m,"darkgreen")
pTolCh <- Plot_two_curves_v0(spp1 = strio,spp2 = chir,locations = c("Toledo"),stri_values,chir_values,stri_m,chir_m,"darkgreen")

pPorCl <- Plot_two_curves_v0(spp1 = strio,spp2 = dipt,locations = c("Porto"),stri_values,dipt_values,stri_m,dipt_m,rgb(.1,1,.5))
pEvoCl <- Plot_two_curves_v0(spp1 = strio,spp2 = dipt,locations = c("Evora"),stri_values,dipt_values,stri_m,dipt_m,rgb(.1,1,.5))
pTolCl <- Plot_two_curves_v0(spp1 = strio,spp2 = dipt,locations = c("Toledo"),stri_values,dipt_values,stri_m,dipt_m,rgb(.1,1,.5))

#modify plots befor multiplot run
pPorCh <- pPorCh + ggtitle("Porto") + theme(title=element_text(size=18)) + theme(plot.title = element_text(hjust=.5)) +
  theme(axis.title.x=element_blank()) + theme(legend.position = "none") + theme(axis.title.y=element_blank()) +
  #theme(plot.margin=unit(c(0.5,1.5,1,4),"cm")) + 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pEvoCh <- pEvoCh + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())+
  theme(legend.position = "none") + 
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pTolCh <- pTolCh + ggtitle("Toledo") + theme(title=element_text(size=18)) + theme(plot.title = element_text(hjust=.5)) +
  theme(axis.title.x=element_blank()) + theme(legend.position = "none") + theme(axis.title.y=element_blank()) +
  #theme(plot.margin=unit(c(0.5,1.5,1,4),"cm")) + 
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)

pPorCl <- pPorCl + theme(legend.position = "none") + theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + #theme(plot.margin=unit(c(0.5,1.5,1.5,4),"cm")) + 
  geom_rect(aes(xmin=10, xmax=24.992, ymin=-Inf, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)
pEvoCl <- pEvoCl + theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank())+
  #theme(plot.margin=unit(c(0.5,0.5,1.5,1),"cm")) +
  geom_rect(aes(xmin=10, xmax=30.192, ymin=-Inf, ymax=Inf), fill="grey", size = I(2), alpha = 0.2)+ theme(legend.position = "none")
pTolCl <- pTolCl + theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank())+
  #theme(plot.margin=unit(c(0.5,0.5,1.5,1),"cm")) +
  geom_rect(aes(xmin=10, xmax=35, ymin=-Inf, ymax=Inf), fill="grey", size = I(2), alpha = 0.2) + theme(legend.position = "none")


source("../Code/multiplot.R")

multiplot(pTolCh, pTolCl, pPorCh, pPorCl, pEvoCh, pEvoCl, cols = 3)

#pCh <- grid_arrange_shared_legend(pPorCh, pEvoCh, ncol = 2, nrow = 1)
#pCl <- grid_arrange_shared_legend(pTolCl, pEvoCl, ncol = 2, nrow = 1)

#call multiplot
#tiff("../Results/SchoolField/Revised/combinedV0.tiff", width = 30, height = 30, units = 'cm', res = 300, compression = 'lzw')
#multiplot(pPorCh,pTolCl,pEvoCh,pEvoCl, cols =2, 
#          labs=list("Temperature (°C)","Speed (m/s)"))
#dev.off()

pEvoCh <- pEvoCh 
pEvoCl <- pEvoCl + theme(legend.position = "none")
tiff("../Results/SchoolField/Revised/V0PorCh.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pPorCh)
dev.off()
tiff("../Results/SchoolField/Revised/V0PorCl.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pTolCl)
dev.off()
tiff("../Results/SchoolField/Revised/V0EvoCh.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pEvoCh)
dev.off()
tiff("../Results/SchoolField/Revised/V0EvoCl.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pEvoCl)
dev.off()
}
