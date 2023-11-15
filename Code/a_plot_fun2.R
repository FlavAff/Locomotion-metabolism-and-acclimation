
SiteSearchRate.Vopt4.version1 <- function(sit,sppC,sppR,consumer,resource,maxi){
  
  values <- subset(values, values$site == sit)
  sppC <- subset(sppC, sppC$site == sit)
  sppR <- subset(sppR, sppR$site == sit)
  
  mc <- mean(sppC$Mass)
  mr <- mean(sppR$Mass)
  
  br <- values[values$Genus == resource,]$b
  bc <- values[values$Genus == consumer,]$b
  
  Er <- values[values$Genus == resource,]$Ea_end
  Ec <- values[values$Genus == consumer,]$Ea_end
  
  GMRr <- power(values[values$Genus == resource,]$B0_end)
  GMRc <- power(values[values$Genus == consumer,]$B0_end)
  GCOTr <- GCOT(mr/1000)
  GCOTc <- GCOT(mc/1000)
  b0c <- Vopt(GMRr,mr/1000,GCOTr)
  b0r <- Vopt(GMRc,mc/1000,GCOTc)
  
  
  Temp <- seq(from = 273.15, to = 318.15, length.out = 200)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() + theme_classic() +
    theme(axis.title=element_text(size=22)) +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 1) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(1), alpha = 0.4)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(1), alpha = 0.4)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(1), alpha = .4)
  p <- p + scale_colour_viridis_d(name = "Model Strategy") + xlim(10,45) + ylim(0,maxi)#+ ggtitle(paste(sit,"Model Predictions")) + theme(plot.title = element_text(face="bold")) 
  p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20)) + theme(axis.text=element_text(size=15))
  
  #open the pdf and put the plot in it
  #pdf(paste0("../Results/FurtherMods/Corrected2/",sit,resource,"as_Vopt4_v1.pdf"))
  #print(p)
  #dev.off()
  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_Vopt4_v1.csv"))
  return(p)
}

SiteSearchRate.Vopt4.version2 <- function(sit,sppC,sppR,consumer,resource, pred.b0.c,pred.b0.r,maxi){
  
  values <- subset(values, values$site == sit)
  sppC <- subset(sppC, sppC$site == sit)
  sppR <- subset(sppR, sppR$site == sit)
  
  mc <- mean(sppC$Mass)
  mr <- mean(sppR$Mass)
  
  br <- values[values$Genus == resource,]$b
  bc <- values[values$Genus == consumer,]$b
  
  Er <- values[values$Genus == resource,]$Ea_end
  Ec <- values[values$Genus == consumer,]$Ea_end
  
  Temp <- seq(from = 273.15, to = 313.15, length.out = 200)

  GMRr <- power(pred.b0.r$O2.consumption)
  GMRc <- power(pred.b0.c$O2.consumption)
  GCOTr <- GCOT4(mr/1000)
  GCOTc <- GCOT4(mc/1000)
  b0r <- Vopt(GMRr,mr/1000,GCOTr)
  b0c <- Vopt(GMRc,mc/1000,GCOTc)
  
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() + theme_classic() +
    theme(axis.title=element_text(size=22)) +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 1) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(1), alpha = 0.4)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(1), alpha = 0.4)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(1), alpha = 0.4)
  p <- p + scale_color_discrete(name = "Model Strategy") + xlim(10,40) + ylim(0,maxi)#+ ggtitle(paste(sit,"Model Predictions")) + theme(plot.title = element_text(face="bold")) 
  p <- p + theme(legend.text=element_text(size=17)) + theme(legend.title=element_text(size=20)) + theme(axis.text=element_text(size=15))
  
  #open the pdf and put the plot in it
  #pdf(paste0("../Results/FurtherMods/Corrected/",sit,resource,"as_Vopt4_v2.pdf"))
  #print(p)
  #dev.off()
  
  #a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  #write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_Vopt4_v2.csv"))
  return(p)
}