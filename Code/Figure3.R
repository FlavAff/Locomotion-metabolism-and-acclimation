rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/")
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

values <- read.csv("../Results/SchoolField/Revised/UsedValuesMean.csv")
values$site <- factor(values$site, levels = c("Penalara","Jaca","Porto","Toledo","Evora","Murcia"))
source("Schoolfields2.R")

#get the parameter values per site for the functions
mur_values <- subset(values, values$site == "Murcia")
tol_values <- subset(values, values$site == "Toledo")
evo_values <- subset(values, values$site == "Evora")
por_values <- subset(values, values$site == "Porto")
jac_values <- subset(values, values$site == "Jaca")
pen_values <- subset(values, values$site == "Penalara")

#get the parameter values per species for the functions
chir_values <- subset(values, values$Genus == "Chironomus")
dipt_values <- subset(values, values$Genus == "Cloeon")
stri_values <- subset(values, values$Genus == "Sympetrum")

Plot_four_curves <- function(spp,locations,spp_values,colours,pos,LOG){
  
  #spp$site <- factor(spp$site, levels = c("Penalara","Jaca","Toledo","Evora","Porto","Murcia"))
  
  Tempe <- seq(from=278.15, to = 318.15, by = 0.5)
  
  Masss1 <- seq(from = as.numeric(min(spp[spp$site == locations[1],]$Mass)), to = as.numeric(max(spp[spp$site == locations[1],]$Mass)), length.out = length(Tempe))
  Masss2 <- seq(from = as.numeric(min(spp[spp$site == locations[2],]$Mass)), to = as.numeric(max(spp[spp$site == locations[2],]$Mass)), length.out = length(Tempe))
  Masss3 <- seq(from = as.numeric(min(spp[spp$site == locations[3],]$Mass)), to = as.numeric(max(spp[spp$site == locations[3],]$Mass)), length.out = length(Tempe))
  Masss4 <- seq(from = as.numeric(min(spp[spp$site == locations[4],]$Mass)), to = as.numeric(max(spp[spp$site == locations[4],]$Mass)), length.out = length(Tempe))
  
  #get the predicted values for all models
  d1 <- SchoolfieldM(B0 = spp_values[spp_values$site == locations[1],]$B0_end, 
                     b = spp_values[spp_values$site == locations[1],]$b,
                     m = Masss1, 
                     E = spp_values[spp_values$site == locations[1],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[1],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[1],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[1],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  d2 <- SchoolfieldM(B0 = spp_values[spp_values$site == locations[2],]$B0_end, 
                     b = spp_values[spp_values$site == locations[2],]$b,
                     m = Masss2, 
                     E = spp_values[spp_values$site == locations[2],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[2],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[2],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[2],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  d3 <- SchoolfieldM(B0 = spp_values[spp_values$site == locations[3],]$B0_end, 
                     b = spp_values[spp_values$site == locations[3],]$b,
                     m = Masss3, 
                     E = spp_values[spp_values$site == locations[3],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[3],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[3],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[3],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  d4 <- SchoolfieldM(B0 = spp_values[spp_values$site == locations[4],]$B0_end, 
                     b = spp_values[spp_values$site == locations[4],]$b,
                     m = Masss4, 
                     E = spp_values[spp_values$site == locations[4],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[4],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[4],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[4],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  
  Temperature1 <- seq(from = 5, to = 45, length.out = length(d1))
  Temperature2 <- seq(from = 5, to = 45, length.out = length(d2))
  Temperature3 <- seq(from = 5, to = 45, length.out = length(d3))
  Temperature4 <- seq(from = 5, to = 45, length.out = length(d4))
  
  #create plottable data
  Model1 <- data.frame(Temperature1, d1)
  Model2 <- data.frame(Temperature2, d2)
  Model3 <- data.frame(Temperature3, d3)
  Model4 <- data.frame(Temperature4, d4)
  
  #create plot
  p <- ggplot() + theme_classic() +
    theme(axis.title=element_text(size=22)) + theme(axis.title = element_blank())
    #xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    #ylab(expression(paste("log(Oxygen Consumption) (",mu,"mol/h)"))) 
  p <- p + scale_color_discrete(name = "Mean April temperature")
  p <- p + scale_colour_manual(name="Site of Origin", 
                               values=colours)
  p <- p + theme(legend.text=element_text(size=17), legend.title=element_text(size=20), axis.text=element_text(size=15))
  p <- p + annotate("text", x = pos[1], y = pos[2], label = paste(as.character(spp$genus[1]),as.character(spp$species[1])), size = 15, fontface = 'italic')
  if (LOG == T){
    p <- p + geom_line(data = Model1, aes(x = Temperature1, y = d1, colour = paste(as.character(format(spp_values[spp_values$site == locations[1],]$Site.April.mean.temp,digits=3)),"°C", paste0("(",locations[1],")"))), size = I(2), alpha = 0.7) 
    p <- p + geom_point(aes(x = spp[spp$site == locations[1],]$chamber.T, y = spp[spp$site == locations[1],]$LogC), colour = colours[1], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model2, aes(x = Temperature2, y = d2, colour = paste(as.character(format(spp_values[spp_values$site == locations[2],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[2],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[2],]$chamber.T, y = spp[spp$site == locations[2],]$LogC), colour = colours[2], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model3, aes(x = Temperature3, y = d3, colour = paste(as.character(format(spp_values[spp_values$site == locations[3],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[3],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[3],]$chamber.T, y = spp[spp$site == locations[3],]$LogC), colour = colours[3], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model4, aes(x = Temperature4, y = d4, colour = paste(as.character(format(spp_values[spp_values$site == locations[4],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[4],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[4],]$chamber.T, y = spp[spp$site == locations[4],]$LogC), colour = colours[4], size = I(1), alpha = 0.4)
    #open the pdf and put the plot in it
    tiff(paste0("../Results/SchoolField/Curves/LogSchool",as.character(spp$genus[1]),"TPCsMean.tiff"), width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
    print(p)
    dev.off()
    }
  else {
    p <- p + geom_line(data = Model1, aes(x = Temperature1, y = exp(d1), colour = paste(as.character(format(spp_values[spp_values$site == locations[1],]$Site.April.mean.temp,digits=3)),"°C", paste0("(",locations[1],")"))), size = I(2), alpha = 0.7) 
    p <- p + geom_point(aes(x = spp[spp$site == locations[1],]$chamber.T, y = spp[spp$site == locations[1],]$O2.consumption), colour = colours[1], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model2, aes(x = Temperature2, y = exp(d2), colour = paste(as.character(format(spp_values[spp_values$site == locations[2],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[2],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[2],]$chamber.T, y = spp[spp$site == locations[2],]$O2.consumption), colour = colours[2], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model3, aes(x = Temperature3, y = exp(d3), colour = paste(as.character(format(spp_values[spp_values$site == locations[3],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[3],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[3],]$chamber.T, y = spp[spp$site == locations[3],]$O2.consumption), colour = colours[3], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model4, aes(x = Temperature4, y = exp(d4), colour = paste(as.character(format(spp_values[spp_values$site == locations[4],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[4],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[4],]$chamber.T, y = spp[spp$site == locations[4],]$O2.consumption), colour = colours[4], size = I(1), alpha = 0.4)
    #open the pdf and put the plot in it
    tiff(paste0("../Results/SchoolField/Curves/School",as.character(spp$genus[1]),"TPCsMean.tiff"), width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
    print(p)
    dev.off()
    }
}

Plot_four_curves_0.75 <- function(spp,locations,spp_values,colours,pos,LOG){
  
  #spp$site <- factor(spp$site, levels = c("Penalara","Jaca","Toledo","Evora","Porto","Murcia"))
  
  Tempe <- seq(from=278.15, to = 318.15, by = 0.5)
  
  #get the predicted values for all models
  d1 <- SchoolfieldNo(B0 = spp_values[spp_values$site == locations[1],]$B0_end, 
                     E = spp_values[spp_values$site == locations[1],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[1],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[1],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[1],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  d2 <- SchoolfieldNo(B0 = spp_values[spp_values$site == locations[2],]$B0_end, 
                     E = spp_values[spp_values$site == locations[2],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[2],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[2],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[2],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  d3 <- SchoolfieldNo(B0 = spp_values[spp_values$site == locations[3],]$B0_end, 
                     E = spp_values[spp_values$site == locations[3],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[3],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[3],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[3],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  d4 <- SchoolfieldNo(B0 = spp_values[spp_values$site == locations[4],]$B0_end, 
                     E = spp_values[spp_values$site == locations[4],]$Ea_end, 
                     Ed = spp_values[spp_values$site == locations[4],]$Ed_end, 
                     TempH = spp_values[spp_values$site == locations[4],]$Tpk_end, 
                     #TempN = spp_values[spp_values$site == locations[4],]$Site.April.median.temp + 273.15, 
                     Temp = Tempe)
  
  Temperature1 <- seq(from = 5, to = 45, length.out = length(d1))
  Temperature2 <- seq(from = 5, to = 45, length.out = length(d2))
  Temperature3 <- seq(from = 5, to = 45, length.out = length(d3))
  Temperature4 <- seq(from = 5, to = 45, length.out = length(d4))
  
  #create plottable data
  Model1 <- data.frame(Temperature1, d1)
  Model2 <- data.frame(Temperature2, d2)
  Model3 <- data.frame(Temperature3, d3)
  Model4 <- data.frame(Temperature4, d4)
  
  #create plot
  p <- ggplot() + theme_classic() +
    theme(axis.title=element_text(size=22)) + theme(axis.title = element_blank())
    #xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    #ylab(expression(paste("log(Oxygen Consumption) (",mu,"mol/h)"))) 
  p <- p + scale_color_discrete(name = "Mean April temperature")
  p <- p + scale_colour_manual(name="Site of Origin", 
                               values=colours)
  p <- p + theme(legend.text=element_text(size=17), legend.title=element_text(size=20), axis.text=element_text(size=15))
  p <- p + annotate("text", x = pos[1], y = pos[2], label = paste(as.character(spp$genus[1]),as.character(spp$species[1])), size = 15,fontface = 'italic')
  if (LOG == T){
    p <- p + geom_line(data = Model1, aes(x = Temperature1, y = d1, colour = paste(as.character(format(spp_values[spp_values$site == locations[1],]$Site.April.mean.temp,digits=3)),"°C", paste0("(",locations[1],")"))), size = I(2), alpha = 0.7) 
    p <- p + geom_point(aes(x = spp[spp$site == locations[1],]$chamber.T, y = spp[spp$site == locations[1],]$LogC), colour = colours[1], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model2, aes(x = Temperature2, y = d2, colour = paste(as.character(format(spp_values[spp_values$site == locations[2],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[2],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[2],]$chamber.T, y = spp[spp$site == locations[2],]$LogC), colour = colours[2], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model3, aes(x = Temperature3, y = d3, colour = paste(as.character(format(spp_values[spp_values$site == locations[3],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[3],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[3],]$chamber.T, y = spp[spp$site == locations[3],]$LogC), colour = colours[3], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model4, aes(x = Temperature4, y = d4, colour = paste(as.character(format(spp_values[spp_values$site == locations[4],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[4],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[4],]$chamber.T, y = spp[spp$site == locations[4],]$LogC), colour = colours[4], size = I(1), alpha = 0.4)
    #open the pdf and put the plot in it
    tiff(paste0("../Results/SchoolField/Curves/LogSchool",as.character(spp$genus[1]),"TPCsMean.tiff"), width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
    print(p)
    dev.off()
    }
  else {
    p <- p + geom_line(data = Model1, aes(x = Temperature1, y = exp(d1), colour = paste(as.character(format(spp_values[spp_values$site == locations[1],]$Site.April.mean.temp,digits=3)),"°C", paste0("(",locations[1],")"))), size = I(2), alpha = 0.7) 
    p <- p + geom_point(aes(x = spp[spp$site == locations[1],]$chamber.T, y = spp[spp$site == locations[1],]$O2.consumption), colour = colours[1], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model2, aes(x = Temperature2, y = exp(d2), colour = paste(as.character(format(spp_values[spp_values$site == locations[2],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[2],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[2],]$chamber.T, y = spp[spp$site == locations[2],]$O2.consumption), colour = colours[2], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model3, aes(x = Temperature3, y = exp(d3), colour = paste(as.character(format(spp_values[spp_values$site == locations[3],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[3],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[3],]$chamber.T, y = spp[spp$site == locations[3],]$O2.consumption), colour = colours[3], size = I(1), alpha = 0.4)
    p <- p + geom_line(data = Model4, aes(x = Temperature4, y = exp(d4), colour = paste(as.character(format(spp_values[spp_values$site == locations[4],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[4],")"))), size = I(2), alpha = 0.7)
    p <- p + geom_point(aes(x = spp[spp$site == locations[4],]$chamber.T, y = spp[spp$site == locations[4],]$O2.consumption), colour = colours[4], size = I(1), alpha = 0.4)
    #open the pdf and put the plot in it
    tiff(paste0("../Results/SchoolField/Curves/School",as.character(spp$genus[1]),"TPCsMean.tiff"), width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
    print(p)
    dev.off()
    }
}

#plot the species curves
chircol <- c("#2FE203","#B8E104","#E08205","#0097E5")
chirpos <- c(15,1.5)
Plot_four_curves(chir,c("Penalara","Porto","Toledo","Evora"),chir_values,chircol,chirpos,LOG = T) 
diptcol <- c("#2FE203","#B8E104","#E08205","#DF0611")
diptpos <- c(18,1.8)
Plot_four_curves(dipt,c("Porto","Toledo","Evora","Murcia"),dipt_values,diptcol,diptpos,LOG = T) 
striocol <- c("#012BE3","#2FE203","#B8E104","#E08205")
striopos<- c(23,2)
Plot_four_curves_0.75(strio,c("Jaca","Toledo","Evora","Porto"),stri_values,striocol,striopos,LOG = T)

#colours <- c("#0097E5","#012BE3","#2FE203","#B8E104","#E08205","#DF0611")
