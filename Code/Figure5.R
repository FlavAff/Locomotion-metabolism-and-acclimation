rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)
library(gridExtra)

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/")
source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("Schoolfields2.R")
source("plot_functions.R")
source("further_models_fun.R")
source("multiplot.R")

values <- read.csv("../Results/SchoolField/Revised/UsedValuesMean.csv")
values$site <- factor(values$site, levels = c("Penalara","Jaca","Toledo","Evora","Porto","Murcia"))

#get the parameter values per species for the functions
chir_values <- subset(values, values$Genus == "Chironomus")
dipt_values <- subset(values, values$Genus == "Cloeon")
stri_values <- subset(values, values$Genus == "Sympetrum")

Plot_two_curves_v0 <- function(spp1,spp2,locations,spp1_values,spp2_values,GCOT1,GCOT2,m1,m2,preycol){
  
  for (i in locations){
    
    #get the predicted values for both models
    Tempe <- seq(from=283.15, to = 318.15, by = 0.5)
    
    Masss2 <- seq(from = as.numeric(min(spp2[spp2$site == i,]$Mass)), to = as.numeric(max(spp2[spp2$site == i,]$Mass)), length.out = length(Tempe))
    
    d1 <- SchoolfieldNo(B0 = spp1_values[spp1_values$site == i,]$B0_end, 
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
    
    d1 <- 0.3*Vopt(power(d1),m1,GCOT1)
    d2 <- 0.3*Vopt(power(d2),m2,GCOT2)
    
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
    if (locations == "Evora" && preycol == "orange"){
      p <- p + ggtitle("Warm site (Evora)") + theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5))
    }
    p <- p + scale_colour_manual(name="Genus", values=c(preycol,"darkred"))
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
pTolCh <- Plot_two_curves_v0(spp1 = strio,spp2 = chir,locations = c("Toledo"),stri_values,chir_values,stri_GCOT,chir_GCOT,stri_m,chir_m,"orange")
#remeber to unhash function to get title on plot here:
pEvoCh <- Plot_two_curves_v0(spp1 = strio,spp2 = chir,locations = c("Evora"),stri_values,chir_values,stri_GCOT,chir_GCOT,stri_m,chir_m,"orange")

pTolCl <- Plot_two_curves_v0(spp1 = strio,spp2 = dipt,locations = c("Toledo"),stri_values,dipt_values,stri_GCOT,dipt_GCOT,stri_m,dipt_m,"pink")
pEvoCl <- Plot_two_curves_v0(spp1 = strio,spp2 = dipt,locations = c("Evora"),stri_values,dipt_values,stri_GCOT,dipt_GCOT,stri_m,dipt_m,"pink")

#modify plots befor multiplot run
pTolCh <- pTolCh + ggtitle("Cool site (Toledo)") + theme(title=element_text(size=22)) + theme(plot.title = element_text(hjust=.5)) +
  theme(axis.title.x=element_blank()) + theme(legend.position = "none") + theme(axis.title.y=element_blank()) +
  #theme(plot.margin=unit(c(0.5,1.5,1,4),"cm")) + 
  geom_rect(aes(xmin=33.6, xmax=45, ymin=-Inf, ymax=Inf), size = I(2), alpha = 0.2)
pEvoCh <- pEvoCh + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())+
  #theme(plot.margin=unit(c(0.5,0.5,1,1),"cm")) + 
  geom_rect(aes(xmin=30.192, xmax=45, ymin=-Inf, ymax=Inf), size = I(2), alpha = 0.2)
pTolCl <- pTolCl + theme(legend.position = "none") + theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + #theme(plot.margin=unit(c(0.5,1.5,1.5,4),"cm")) + 
  geom_rect(aes(xmin=33.6, xmax=45, ymin=-Inf, ymax=Inf), size = I(2), alpha = 0.2)
pEvoCl <- pEvoCl + theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank())+
  #theme(plot.margin=unit(c(0.5,0.5,1.5,1),"cm")) +
  geom_rect(aes(xmin=30.192, xmax=45, ymin=-Inf, ymax=Inf), size = I(2), alpha = 0.2)

#pCh <- grid_arrange_shared_legend(pTolCh, pEvoCh, ncol = 2, nrow = 1)
#pCl <- grid_arrange_shared_legend(pTolCl, pEvoCl, ncol = 2, nrow = 1)

#call multiplot
#tiff("../Results/SchoolField/Revised/combinedV0.tiff", width = 30, height = 30, units = 'cm', res = 300, compression = 'lzw')
#multiplot(pTolCh,pTolCl,pEvoCh,pEvoCl, cols =2, 
#          labs=list("Temperature (°C)","Speed (m/s)"))
#dev.off()

pEvoCh <- pEvoCh + theme(legend.position = "none")
pEvoCl <- pEvoCl + theme(legend.position = "none")
tiff("../Results/SchoolField/Revised/V0TolCh.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pTolCh)
dev.off()
tiff("../Results/SchoolField/Revised/V0TolCl.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pTolCl)
dev.off()
tiff("../Results/SchoolField/Revised/V0EvoCh.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pEvoCh)
dev.off()
tiff("../Results/SchoolField/Revised/V0EvoCl.tiff", width = 15, height = 15, units = 'cm', res = 300, compression = 'lzw')
print(pEvoCl)
dev.off()
