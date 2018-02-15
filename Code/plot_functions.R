Plot_per_site_points <- function(df,site_values,genera){
  
  for (i in genera){
    
    location <- subset(df, df$genus == i)
    
    DataToPlot <- data.frame(Temperature = location[,"chamber.T"], O2.consumption = location[,"O2.consumption"])
    
    Tempe <- seq(from = as.numeric(min(DataToPlot$Temperature)) + 273.15, to = as.numeric(max(DataToPlot$Temperature)) + 273.15, length.out = length(DataToPlot$Temperature))
    Masss <- seq(from = as.numeric(min(location$Mass)), to = as.numeric(max(location$Mass)), length.out = length(Tempe))
    
    d2 <- SchoolfieldM(B0 = site_values[site_values$Genus == i,]$B0_end, 
                       b = site_values[site_values$Genus == i,]$b,
                       m = Masss, 
                       E = site_values[site_values$Genus == i,]$Ea_end, 
                       Ed = site_values[site_values$Genus == i,]$Ed_end, 
                       TempH = site_values[site_values$Genus == i,]$Tpk_end,
                       #TempN = site_values[site_values$Genus == i,]$Site.April.median.temp + 273.15,
                       Temp = Tempe)
    Model <- data.frame(Temperature = Tempe - 273.15, O2.consumption = exp(d2))
    
    p <- ggplot() + theme_classic() + geom_point(data = DataToPlot, aes(x = Temperature, y = O2.consumption), size = I(3), colour = "blue", alpha = 0.7) 
    p <- p + geom_line(data = Model, aes(x= Temperature, y = O2.consumption), size = I(2), colour = "red", alpha=0.7)
    p <- p + xlab(expression(paste("Temperature (", degree, C, ")"))) + ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) 
#    p <- p +  annotate("text", x = 15, y = as.numeric(max(DataToPlot$O2.consumption))*0.9, 
#                       label = paste("Ea =", format(site_values[site_values$Genus == i,]$Ea_end, digits = 2), "eV", 
#                                     "\nEd =", format(site_values[site_values$Genus == i,]$Ed_end, digits = 2), "eV", 
#                                     "\nTpk =", format(site_values[site_values$Genus == i,]$Tpk_end - 273.15, digits = 2),"°C", 
#                                     "\nB0 =", format(exp(site_values[site_values$Genus == i,]$B0_end), digits = 2),
#                                     "\nb =", format(site_values[site_values$Genus == i,]$b, digits = 3),
#                                     "\nR2 =", format(site_values[site_values$Genus == i,]$R_sq, digits = 3))) 

    pdf(paste0("../Results/SchoolField/Revised/School",df$site[1],i,"TPCs.pdf"))
    print(p)
    dev.off()
  }
}


Plot_per_site_curve <- function(df,site_values,genera){
  
  for (i in genera){
    
    location <- subset(df, df$genus == i)
    
    DataToPlot <- data.frame(Temperature = location[,"chamber.T"], O2.consumption = location[,"O2.consumption"])
    
    Tempe <- seq(from = as.numeric(min(DataToPlot$Temperature)) + 273.15, to = as.numeric(max(DataToPlot$Temperature)) + 273.15, length.out = length(DataToPlot$Temperature))
    Masss <- seq(from = as.numeric(min(location$Mass)), to = as.numeric(max(location$Mass)), length.out = length(Tempe))
    
    d2 <- SchoolfieldM(B0 = site_values[site_values$Genus == i,]$B0_end, 
                       b = site_values[site_values$Genus == i,]$b,
                       m = Masss, 
                       E = site_values[site_values$Genus == i,]$Ea_end, 
                       Ed = site_values[site_values$Genus == i,]$Ed_end, 
                       TempH = site_values[site_values$Genus == i,]$Tpk_end, 
                       #TempN = site_values[site_values$Genus == i,]$Site.April.median.temp + 273.15,
                       Temp = Tempe)
    Model <- data.frame(Temperature = Tempe - 273.15, O2.consumption = exp(d2))
    
    p <- ggplot() + geom_line(data = Model, aes(x= Temperature, y = O2.consumption), size = I(2), colour = "blue", alpha=0.7)
    p <- p + xlab(expression(paste("Temperature (", degree, C, ")"))) + ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) 
    p <- p + ggtitle(paste(location$site[1],i,"metabolism")) + theme(plot.title = element_text(face="bold"))
    
    pdf(paste0("../Results/SchoolField/Revised/SchoolCurve",df$site[1],i,"TPCs.pdf"))
    print(p)
    dev.off()
  }
}


Plot_four_curves <- function(spp,locations,spp_values){
  
  #spp$site <- factor(spp$site, levels = c("Penalara","Jaca","Toledo","Evora","Porto","Murcia"))

  Tempe <- seq(from=273.15, to = 318.15, by = 0.5)
  
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
  
  Temperature1 <- seq(from = 0, to = 45, length.out = length(d1))
  Temperature2 <- seq(from = 0, to = 45, length.out = length(d2))
  Temperature3 <- seq(from = 0, to = 45, length.out = length(d3))
  Temperature4 <- seq(from = 0, to = 45, length.out = length(d4))
  
  #create plottable data
  Model1 <- data.frame(Temperature1, exp(d1))
  Model2 <- data.frame(Temperature2, exp(d2))
  Model3 <- data.frame(Temperature3, exp(d3))
  Model4 <- data.frame(Temperature4, exp(d4))

  #create plot
  p <- ggplot() + theme_classic() +
    theme(axis.title=element_text(size=22)) +
    geom_line(data = Model1, aes(x = Temperature1, y = exp(d1), colour = paste(as.character(format(spp_values[spp_values$site == locations[1],]$Site.April.mean.temp,digits=3)),"°C", paste0("(",locations[1],")"))), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Oxygen Consumption (",mu,"mol/h)"))) 
  p <- p + geom_line(data = Model2, aes(x = Temperature2, y = exp(d2), colour = paste(as.character(format(spp_values[spp_values$site == locations[2],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[2],")"))), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = Model3, aes(x = Temperature3, y = exp(d3), colour = paste(as.character(format(spp_values[spp_values$site == locations[3],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[3],")"))), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = Model4, aes(x = Temperature4, y = exp(d4), colour = paste(as.character(format(spp_values[spp_values$site == locations[4],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[4],")"))), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Mean April temperature") #+ ggtitle(paste(spp$genus[1],"TPCs")) +
    #theme(plot.title = element_text(face="bold"))
  p <- p + scale_colour_manual(name="Site of Origin", 
                              values=c("#2FE203","#B8E104","#E08205","#012BE3"))
  p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20)) + theme(axis.text=element_text(size=15))
  p <- p + annotate("text", x = 17, y = 12, label = paste(as.character(spp$genus[1]),as.character(spp$species[1])), size = 7,fontface = 'italic')
  #open the pdf and put the plot in it
  pdf(paste0("../Results/SchoolField/Revised/School",as.character(spp$genus[1]),"TPCs.pdf"))
  print(p)
  dev.off()}


Plot_two_curves <- function(spp1,spp2,locations,spp1_values,spp2_values){
  
  for (i in locations){
    
    #get the predicted values for both models
    Tempe <- seq(from=273.15, to = 318.15, by = 0.5)
    
    Masss1 <- seq(from = as.numeric(min(spp1[spp1$site == i,]$Mass)), to = as.numeric(max(spp1[spp1$site == i,]$Mass)), length.out = length(Tempe))
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
    
    Temperature <- seq(from = 0, to = 45, length.out = length(d1))
    
    #create plottable data
    Model1 <- data.frame(Temperature, exp(d1))
    Model2 <- data.frame(Temperature, exp(d2))
    p <- ggplot() + theme_classic() +
      geom_line(data = Model1, aes(x = Temperature, y = exp(d1), colour = spp1$genus[1]), size = I(2), alpha = 0.7) + 
      xlab(expression(paste("Temperature (", degree, C, ")"))) + 
      ylab(expression(paste("Mass Corrected Oxygen Consumption (",mu,"mol/h)"))) 
    p <- p + geom_line(data = Model2, aes(x = Temperature, y = exp(d2), colour = spp2$genus[1]), size = I(2), alpha = 0.7)
    p <- p + scale_color_discrete(name = "Genus") #+ ggtitle(paste("Predator - Prey TPCs from",i)) +
      #theme(plot.title = element_text(face="bold"))
    #open the pdf and put the plot in it
    pdf(paste0("../Results/SchoolField/Revised/VS",i,spp2$genus[1],"SchoolTPCs.pdf"))
    print(p)
    dev.off()
  }
}

Plot_two_curves_v0 <- function(spp1,spp2,locations,spp1_values,spp2_values,GCOT1,GCOT2,m1,m2,preycol){
  
  for (i in locations){
    
    #get the predicted values for both models
    Tempe <- seq(from=273.15, to = 318.15, by = 0.5)
    
    Masss1 <- seq(from = as.numeric(min(spp1[spp1$site == i,]$Mass)), to = as.numeric(max(spp1[spp1$site == i,]$Mass)), length.out = length(Tempe))
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
    
    Temperature <- seq(from = 0, to = 45, length.out = length(d1))
    
    d1 <- 0.3*Vopt(power(d1),m1,GCOT1)
    d2 <- 0.3*Vopt(power(d2),m2,GCOT2)
    
    #create plottable data
    Model1 <- data.frame(Temperature, exp(d1))
    Model2 <- data.frame(Temperature, exp(d2))
    p <- ggplot() + theme_classic() + ylim(0,2) + 
      theme(axis.title=element_text(size=22)) +
      geom_line(data = Model1, aes(x = Temperature, y = exp(d1), colour = spp1$genus[1]), size = I(2), alpha = 0.7) + 
      xlab(expression(paste("Temperature (", degree, C, ")"))) + 
      ylab(expression(paste("Speed (m/s)"))) 
    p <- p + geom_line(data = Model2, aes(x = Temperature, y = exp(d2), colour = spp2$genus[1]), size = I(2), alpha = 0.7)
    #p <- p + scale_color_discrete(name = "Genus") #+ ggtitle(paste("Predator - Prey TPCs from",i)) +
    p <- p + scale_colour_manual(name="Genus", values=c(preycol,"darkred"))
    p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20)) + theme(axis.text=element_text(size=15))
    #theme(plot.title = element_text(face="bold"))
    #open the pdf and put the plot in it
    pdf(paste0("../Results/SchoolField/Revised/VS",i,spp2$genus[1],"SchoolTPCsv0.pdf"))
    print(p)
    dev.off()
    
    #print(paste("difference in b0 for",i,exp(d1)-exp(d2)))
  }
}


Plot_four_curves_log <- function(spp,locations,spp_values){
  
  Tempe <- seq(from=273.15, to = 318.15, by = 0.5)
  
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
  
  Temperature1 <- seq(from = 0, to = 45, length.out = length(d1))
  Temperature2 <- seq(from = 0, to = 45, length.out = length(d2))
  Temperature3 <- seq(from = 0, to = 45, length.out = length(d3))
  Temperature4 <- seq(from = 0, to = 45, length.out = length(d4))
  
  #create plottable data
  Model1 <- data.frame(Temperature1, d1)
  Model2 <- data.frame(Temperature2, d2)
  Model3 <- data.frame(Temperature3, d3)
  Model4 <- data.frame(Temperature4, d4)
  
  #create plot
  p <- ggplot() + theme_classic() +
    theme(axis.title=element_text(size=22)) +
    geom_line(data = Model1, aes(x = Temperature1, y = d1, colour = paste(as.character(format(spp_values[spp_values$site == locations[1],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[1],")"))), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("log(Oxygen Consumption (",mu,"mol/h))"))) 
  p <- p + geom_line(data = Model2, aes(x = Temperature2, y = d2, colour = paste(as.character(format(spp_values[spp_values$site == locations[2],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[2],")"))), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = Model3, aes(x = Temperature3, y = d3, colour = paste(as.character(format(spp_values[spp_values$site == locations[3],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[3],")"))), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = Model4, aes(x = Temperature4, y = d4, colour = paste(as.character(format(spp_values[spp_values$site == locations[4],]$Site.April.mean.temp,digits=4)),"°C", paste0("(",locations[4],")"))), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Mean April temperature") #+ ggtitle(paste(spp$genus[1],"TPCs")) +
  #theme(plot.title = element_text(face="bold"))
  #open the pdf and put the plot in it
  pdf(paste0("../Results/SchoolField/Revised/School",as.character(spp$genus[1]),"TPCsLog.pdf"))
  print(p)
  dev.off()}