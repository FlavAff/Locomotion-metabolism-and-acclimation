rm(list = ls())
dev.off()

library(plyr)
library(minpack.lm)
library(ggplot2)
library(data.table)

setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
source("Schoolfields2.R")
k <- 8.617*(10^-5)

Tempe <- seq(from=273.15, to = 328.15, by = 0.5)

Masss <- seq(from = 0.2, to = 1.2, length.out = length(Tempe))


#get the predicted values for all models
d1 <- SchoolfieldMEvo(B0 = 0.4, 
                   b = 0.75,
                   m = Masss, 
                   E = 0.65, 
                   Ed = 2.5, 
                   TempH = 35+273.15, 
                   Temp = Tempe)
dV <- SchoolfieldMEvo(B0 = 0.6, 
                   b = 0.75,
                   m = Masss, 
                   E = 0.65, 
                   Ed = 2.5, 
                   TempH = 35+273.15, 
                   Temp = Tempe)
dE <- SchoolfieldMEvo(B0 = 0.4, 
                   b = 0.75,
                   m = Masss, 
                   E = 0.85, 
                   Ed = 2.5, 
                   TempH = 35+273.15, 
                   Temp = Tempe)

Temperature <- seq(from = 0, to = 55, length.out = length(d1))
Temperature2 <- seq(from = 10, to = 60, length.out = length(d1))


#create plottable data
Model1 <- data.frame(Temperature, d1)
Model2 <- data.frame(Temperature, dV)
Model3 <- data.frame(Temperature, dE)

#create plot 1
#p <- ggplot() + theme_classic() +
#  geom_line(data = Model1, aes(x = Temperature, y = d1), size = I(2), alpha = 0.7) + 
#  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
#  theme(axis.title = element_blank())
  #xlab(expression(paste("Temperature (", degree, C, ")"))) + 
  #ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)")))# + ylim(0,1.3)
#p <- p + geom_vline(xintercept=5, linetype="dotted") + geom_vline(xintercept=33, linetype="dotted")
#p <- p + annotate("text", x = 37, y = max(Model1$d1)+.4, label = "T[pk]", size = 10, parse = T)
#p <- p + annotate("text", x = 25, y = -0.8, label = "E[a]", size = 10, parse = T)
#p <- p + annotate("text", x = 43, y = -0.7, label = "E[d]", size = 10, parse = T)
#p <- p + annotate("text", x = 18, y = .8, label = "Operational \n Temperature \n Range", size = 10)
#p <- p + annotate("text", x = 1, y = 1.2, label = "A)", size = 10)
#p <- p + geom_segment(aes(x=36, xend=36, y=max(Model1$d1)+.25, yend=max(Model1$d1)+.05), size = 2,
#                      arrow = arrow(length = unit(0.5, "cm")))
#p <- p + geom_segment(aes(x=20, xend=25, y=-0.9, yend=-.4), size = 2,
#                      arrow = arrow(length = unit(0.5, "cm")))
#p <- p + geom_segment(aes(x=43, xend=47, y=-.3, yend=-0.9), size = 2,
#                      arrow = arrow(length = unit(0.5, "cm")))
#p <- p + ylim(min(d1),1.2)
#p1 <- p + geom_rect(aes(xmin=5, xmax=33, ymin=min(d1), ymax=Inf), size = I(2), alpha = 0.1)
#tiff("../Results/ConceptA.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
#print(p1)
#dev.off()

#create plot 2
p <- ggplot() + theme_classic() +
  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
  geom_line(data = Model1, aes(x = Temperature, y = d1), size = I(2), alpha = 0.7, colour = "orange", linetype = "dotted") + 
  geom_line(data = Model2, aes(x = Temperature, y = dV), size = I(2), alpha = 1, colour = "orange") +
  theme(axis.title = element_blank()) + geom_point(aes(x = 20, y = -1.1), size = I(5), alpha = 1, colour = "black") +
  geom_point(aes(x = 23, y = -0.4), size = I(5), alpha = 1, colour = "black")
  #xlab(expression(paste("Temperature (", degree, C, ")"))) + xlim(0,60) +
  #ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) #+ ylim(0,1.25)
p <- p + annotate("text", x = 9, y = 1.1, label = "Increase in \n performance", size = 10)
#p <- p + annotate("text", x = 50, y = 1.9, label = "Warm adapted \n species", size = 10)
p <- p + annotate("text", x = 1, y = 1.2, label = "a)", size = 10)
p <- p + geom_segment(aes(x=23, xend=23, y=min(d1), yend=-0.4), size = 2, linetype = "dashed", alpha = 0.5)
p <- p + geom_segment(aes(x=20, xend=20, y=min(d1), yend=-1.1), size = 2, linetype = "dashed", alpha = 0.5)
p <- p + ylim(min(d1),1.2)
p2 <- p + annotate("text", x=21, y=.3, label = "b[0]", size=10, parse = TRUE)
#p2 <- p + annotate("text", x = 32, y = 0.1, label = "Cold adapted species", size = 10)
tiff("../Results/ConceptA.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p2)
dev.off()

#create plot 3
#p <- ggplot() + theme_classic() +
#  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
#  geom_line(data = Model1, aes(x = Temperature, y = d1), size = I(2), alpha = 0.7, colour = "darkgreen", linetype = "dotted") + 
#  geom_line(data = Model1, aes(x = Temperature2, y = d1), size = I(2), alpha = 1, colour = "darkgreen") +
#  theme(axis.title = element_blank()) 
  #xlab(expression(paste("Temperature (", degree, C, ")"))) + 
  #ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) #+ ylim(0,1.25)
#p <- p + annotate("text", x = 12, y = 1.1, label = "Increase in \n peak temperature", size = 10)
#p <- p + annotate("text", x = 8.5, y = 0.4, label = "Cold adapted \n species", size = 11)
#p <- p + annotate("text", x = 1, y = 1.2, label = "B)", size = 10)
#p <- p + geom_segment(aes(x=34, xend=44, y=.75, yend=.75), size = 2,
#                      arrow = arrow(length = unit(0.5, "cm")))
#p3 <- p + annotate("text", x = 38, y = 0.05, label = "Warm adapted species", size = 11)
#p <- p + ylim(min(d1),1.2)
#p3 <- p + annotate("text", x=40, y=.95, label = "T[pk]", size=10, parse = TRUE)
#tiff("../Results/ConceptB.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
#print(p3)
#dev.off()

#create plot 4
p <- ggplot() + theme_classic() +
  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
  geom_line(data = Model1, aes(x = Temperature, y = d1), size = I(2), alpha = 0.7, colour = rgb(.1,1,.5), linetype = "dotted") + 
  geom_line(data = Model2, aes(x = Temperature, y = dE), size = I(2), alpha = 1, colour = rgb(.1,1,.5)) +
  theme(axis.title = element_blank()) 
p <- p + annotate("text", x = 9, y = 1.1, label = "Increase in \n activation energy", size = 10)
p <- p + annotate("text", x = 1, y = 1.2, label = "b)", size = 10)
p <- p + geom_segment(aes(x=16, xend=23, y=-1.455, yend=-.75), size = 2, colour = "grey")#,
                      #arrow = arrow(length = unit(0.5, "cm")))
p <- p + geom_segment(aes(x=16, xend=23, y=-1.55, yend=-.55), size = 2)#,
                      #arrow = arrow(length = unit(0.5, "cm")))
p <- p + ylim(min(d1),1.2)
p4 <- p + annotate("text", x=15, y=-.95, label = "E[a]", size=10, parse = TRUE)
#p2 <- p + annotate("text", x = 32, y = 0.1, label = "Cold adapted species", size = 10)
tiff("../Results/ConceptC.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p4)
dev.off()
####################################################
#MassPred <- seq(from = 0.2, to = 1.2, length.out = length(Tempe))
#MassPrey <- seq(from = 0.05, to = 0.8, length.out = length(Tempe))

#get the predicted values for all models
#dP1 <- SchoolfieldM(B0 = 0.4, 
#                   b = 0.75,
#                   m = MassPred, 
#                   E = 0.65, 
#                   Ed = 1.5, 
#                   TempH = 35+273.15, 
#                   Temp = Tempe)
#dP2 <- SchoolfieldM(B0 = 0.9, 
#                   b = 0.75,
#                   m = MassPrey, 
#                   E = 0.67, 
#                   Ed = 1.5, 
#                   TempH = 27+273.15, 
#                   Temp = Tempe)


#Temperature <- seq(from = 0, to = 55, length.out = length(d1))


#create plottable data
#ModelP1 <- data.frame(Temperature, exp(dP1))
#ModelP2 <- data.frame(Temperature, exp(dP2))

#create plot 4
#p <- ggplot() + theme_classic() +
#  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
#  geom_line(data = Model1, aes(x = Temperature, y = exp(dP2)), size = I(2), alpha = 0.7, colour = "orange", linetype = "dotted") + 
#  geom_line(data = Model2, aes(x = Temperature, y = exp(dP1)), size = I(2), alpha = 1, colour = "orange") + 
#  xlab(expression(paste("Temperature (", degree, C, ")"))) + 
#  ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) #+ ylim(0,1.25)
#p <- p + annotate("text", x = 9, y = 0.45, label = "Cold adapted \n prey", size = 11)
#p <- p + annotate("text", x = 10, y = 1.6, label = "Predator \n adaptation", size = 10)
#p4 <- p + annotate("text", x = 1, y = 1.7, label = "D)", size = 10)
#p4 <- p + annotate("text", x = 30, y = 0.1, label = "Warm adapted predator", size = 11)
#tiff("../Results/ConceptD", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
#print(p4)
#dev.off()

#get the predicted values for all models
#dP3 <- SchoolfieldM(B0 = 0.6, 
#                    b = 0.75,
#                    m = MassPred, 
#                   E = 0.65, 
#                    Ed = 1.5, 
#                    TempH = 27+273.15, 
#                    Temp = Tempe)
#dP4 <- SchoolfieldM(B0 = 0.8, 
#                    b = 0.75,
#                    m = MassPrey, 
#                    E = 0.67, 
#                    Ed = 1.5, 
#                    TempH = 32+273.15, 
#                    Temp = Tempe)

#create plottable data
#ModelP3 <- data.frame(Temperature, exp(dP3))
#ModelP4 <- data.frame(Temperature, exp(dP4))

#create plot 4
#p <- ggplot() + theme_classic() +
#  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
#  geom_line(data = ModelP3, aes(x = Temperature, y = exp(dP3)), size = I(2), alpha = 0.7, colour = "darkgreen", linetype = "dotted") + 
#  geom_line(data = ModelP4, aes(x = Temperature, y = exp(dP4)), size = I(2), alpha = 1, colour = "darkgreen") + 
#  xlab(expression(paste("Temperature (", degree, C, ")"))) + 
#  ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) #+ ylim(0,1.25)
#p <- p + annotate("text", x = 9, y = 0.45, label = "Cold adapted \n prey", size = 11)
#p <- p + annotate("text", x = 9, y = 1.6, label = "Prey \n adaptation", size = 10)
#p5 <- p + annotate("text", x = 1, y = 1.7, label = "E)", size = 10)
#p4 <- p + annotate("text", x = 30, y = 0.1, label = "Warm adapted predator", size = 11)
#tiff("../Results/ConceptE", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
#print(p5)
#dev.off()

dV <- SchoolfieldMEvo(B0 = 0.5, 
                      b = 0.75,
                      m = Masss, 
                      E = 0.7, 
                      Ed = 2.5, 
                      TempH = 35+273.15, 
                      Temp = Tempe)
dE <- SchoolfieldMEvo(B0 = 0.4, 
                      b = 0.75,
                      m = Masss, 
                      E = 0.5, 
                      Ed = 2.5, 
                      TempH = 35+273.15, 
                      Temp = Tempe)

p <- ggplot() + theme_classic() +
  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
  geom_line(data = Model1, aes(x = Temperature, y = d1), size = I(2), alpha = 0.7, colour = "orange") + 
  geom_line(data = Model1, aes(x = Temperature, y = dV-.2), size = I(2), alpha = 1, colour = "darkgreen") +
  geom_line(data = Model1, aes(x = Temperature, y = dE), size = I(2), alpha = 1, colour = rgb(.1,1,.5)) +
  theme(axis.title = element_blank()) 
  #xlab(expression(paste("Temperature (", degree, C, ")"))) + 
  #ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) #+ ylim(0,1.25)
p <- p + annotate("text", x = 9, y = 1.1, label = "Original \n mismatch", size = 10)
p <- p + annotate("text", x = 19, y = -2, label = "Prey A", size = 10, colour = "darkgreen")
p <- p + annotate("text", x = 10, y = -1.6, label = "Prey B", size = 10, colour = rgb(.1,1,.5))
p <- p + annotate("text", x = 15, y = -.8, label = "Predator", size = 10, colour = "orange")
p <- p + ylim(min(d1),1.2)
p <- p + geom_rect(aes(xmin=0, xmax=33, ymin=min(d1), ymax=Inf), size = I(2), alpha = 0.1)
p6 <- p + annotate("text", x = 1, y = 1.2, label = "c)", size = 10)
tiff("../Results/ConceptD.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p6)
dev.off()

p <- ggplot() + theme_classic() +
  theme(axis.title=element_text(size=28,face="bold"), axis.text=element_blank()) +
  geom_line(data = Model2, aes(x = Temperature, y = dV-0.2), size = I(2), alpha = 0.7, colour = "orange") + 
  geom_line(data = Model1, aes(x = Temperature2, y = d1), size = I(2), alpha = 1, colour = "darkgreen") +
  geom_line(data = Model3, aes(x = Temperature, y = dE), size = I(2), alpha = 0.7, colour = rgb(.1,1,.5)) +
  theme(axis.title = element_blank()) 
  #xlab(expression(paste("Temperature (", degree, C, ")"))) + 
  #ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) #+ ylim(0,1.25)
p <- p + annotate("text", x = 9, y = 1.1, label = "New \n mismatch", size = 10)
p <- p + annotate("text", x = 10, y = -1, label = "Predator", size = 10, colour = "orange")
p <- p + annotate("text", x = 23, y = -2.5, label = "Prey A", size = 10, colour = "darkgreen")
p <- p + annotate("text", x = 43, y = .7, label = "Prey B", size = 10, colour = rgb(.1,1,.5))
p <- p + ylim(min(d1),1.2)
p <- p + geom_rect(aes(xmin=0, xmax=33, ymin=min(d1), ymax=Inf), size = I(2), alpha = 0.1)
p7 <- p + annotate("text", x = 1, y = 1.2, label = "d)", size = 10)
tiff("../Results/ConceptE.tiff", width = 25, height = 20, units = 'cm', res = 300, compression = 'lzw')
print(p7)
dev.off()

################################################################################################
#source("multiplot.R")

#multiplot(p1, p2, p3, p4, cols=2)
