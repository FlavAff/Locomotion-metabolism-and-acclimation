SiteSearchRate.v0 <- function(sit,sppC,sppR,consumer,resource){
  
  values <- subset(values, values$site == sit)
  sppC <- subset(sppC, sppC$site == sit)
  sppR <- subset(sppR, sppR$site == sit)
  
  mc <- mean(sppC$Mass)
  mr <- mean(sppR$Mass)
  
  br <- values[values$Genus == resource,]$b
  bc <- values[values$Genus == consumer,]$b
  
  Er <- values[values$Genus == resource,]$Ea_end
  Ec <- values[values$Genus == consumer,]$Ea_end
  
  P0r <- power(values[values$Genus == resource,]$B0_end)
  P0c <- power(values[values$Genus == consumer,]$B0_end)
  Lr <- mean(sppC$Length)/1000
  Lc <- mean(sppR$Length)/1000
  b0c <- v0(P0c,Lc,visc)
  b0r <- v0(P0r,Lr,visc)

  
  Temp <- seq(from = 273.15, to = 313.15, by = 0.5)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Model Strategy") + ggtitle(paste(sit,"Model Predictions")) +
    theme(plot.title = element_text(face="bold"))
  
  if (resource == "Chironomus" && sit == "Porto"){
    p <- p + geom_point(aes(x=15.1147834951,y=0.7710610339), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=15.1147834951,ymin = 0.7710610339-0.1054280698,ymax = 0.7710610339+0.1054280698))
  }
  if (resource == "Chironomus" && sit == "Evora"){
    p <- p + geom_point(aes(x=12.5511900369,y=2.0500255675), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.5511900369, ymin = 2.0500255675-0.2588407187,ymax = 2.0500255675+0.2588407187))
  }
  if (resource == "Cloeon" && sit == "Toledo"){
    p <- p + geom_point(aes(x=12.1199240069,y=0.1744503421), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.1199240069, ymin = 0.1744503421-0.049078476,ymax = 0.1744503421+0.049078476))
  }
  #open the pdf and put the plot in it
  pdf(paste0("../Results/FurtherMods/",sit,resource,"as_v0.pdf"))
  print(p)
  dev.off()

  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_v0.csv"))
}


SiteSearchRate.b0 <- function(sit,sppC,sppR,consumer,resource){
  
  values <- subset(values, values$site == sit)
  sppC <- subset(sppC, sppC$site == sit)
  sppR <- subset(sppR, sppR$site == sit)
  
  mc <- mean(sppC$Mass)
  mr <- mean(sppR$Mass)
  
  br <- values[values$Genus == resource,]$b
  bc <- values[values$Genus == consumer,]$b
  
  Er <- values[values$Genus == resource,]$Ea_end
  Ec <- values[values$Genus == consumer,]$Ea_end
  
  b0c <- values[values$Genus == consumer,]$B0_end
  b0r <- values[values$Genus == resource,]$B0_end

  Temp <- seq(from = 273.15, to = 313.15, by = 0.5)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Model Strategy") + ggtitle(paste(sit,"Model Predictions")) +
    theme(plot.title = element_text(face="bold"))
  
  if (resource == "Chironomus" && sit == "Porto"){
    p <- p + geom_point(aes(x=15.1147834951,y=0.7710610339), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=15.1147834951,ymin = 0.7710610339-0.1054280698,ymax = 0.7710610339+0.1054280698))
  }
  if (resource == "Chironomus" && sit == "Evora"){
    p <- p + geom_point(aes(x=12.5511900369,y=2.0500255675), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.5511900369, ymin = 2.0500255675-0.2588407187,ymax = 2.0500255675+0.2588407187))
  }
  if (resource == "Cloeon" && sit == "Toledo"){
    p <- p + geom_point(aes(x=12.1199240069,y=0.1744503421), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.1199240069, ymin = 0.1744503421-0.049078476,ymax = 0.1744503421+0.049078476))
  }
  #open the pdf and put the plot in it
  pdf(paste0("../Results/FurtherMods/",sit,resource,"as_b0.pdf"))
  print(p)
  dev.off()
  
  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_b0.csv"))
}


SiteSearchRate.Vopt1 <- function(sit,sppC,sppR,consumer,resource){
  
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
  GCOTr <- GCOT1(mr/1000)
  GCOTc <- GCOT1(mc/1000)
  b0c <- Vopt(GMRr,mr/1000,GCOTr)
  b0r <- Vopt(GMRc,mc/1000,GCOTc)
  
  
  Temp <- seq(from = 273.15, to = 313.15, by = 0.5)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Model Strategy") + ggtitle(paste(sit,"Model Predictions")) +
    theme(plot.title = element_text(face="bold"))
  
  if (resource == "Chironomus" && sit == "Porto"){
    p <- p + geom_point(aes(x=15.1147834951,y=0.7710610339), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=15.1147834951,ymin = 0.7710610339-0.1054280698,ymax = 0.7710610339+0.1054280698))
  }
  if (resource == "Chironomus" && sit == "Evora"){
    p <- p + geom_point(aes(x=12.5511900369,y=2.0500255675), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.5511900369, ymin = 2.0500255675-0.2588407187,ymax = 2.0500255675+0.2588407187))
  }
  if (resource == "Cloeon" && sit == "Toledo"){
    p <- p + geom_point(aes(x=12.1199240069,y=0.1744503421), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.1199240069, ymin = 0.1744503421-0.049078476,ymax = 0.1744503421+0.049078476))
  }
  #open the pdf and put the plot in it
  pdf(paste0("../Results/FurtherMods/",sit,resource,"as_Vopt1.pdf"))
  print(p)
  dev.off()
  
  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_Vopt1.csv"))
}


SiteSearchRate.Vopt2 <- function(sit,sppC,sppR,consumer,resource){
  
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
  Lr <- mean(sppC$Length)/1000
  Lc <- mean(sppR$Length)/1000
  GCOTr <- GCOT2(mr/1000,Lr)
  GCOTc <- GCOT2(mc/1000,Lc)
  b0c <- Vopt(GMRr,mr/1000,GCOTr)
  b0r <- Vopt(GMRc,mc/1000,GCOTc)
  
  
  Temp <- seq(from = 273.15, to = 313.15, by = 0.5)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Model Strategy") + ggtitle(paste(sit,"Model Predictions")) +
    theme(plot.title = element_text(face="bold"))
  
  if (resource == "Chironomus" && sit == "Porto"){
    p <- p + geom_point(aes(x=15.1147834951,y=0.7710610339), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=15.1147834951,ymin = 0.7710610339-0.1054280698,ymax = 0.7710610339+0.1054280698))
  }
  if (resource == "Chironomus" && sit == "Evora"){
    p <- p + geom_point(aes(x=12.5511900369,y=2.0500255675), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.5511900369, ymin = 2.0500255675-0.2588407187,ymax = 2.0500255675+0.2588407187))
  }
  if (resource == "Cloeon" && sit == "Toledo"){
    p <- p + geom_point(aes(x=12.1199240069,y=0.1744503421), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.1199240069, ymin = 0.1744503421-0.049078476,ymax = 0.1744503421+0.049078476))
  }
  #open the pdf and put the plot in it
  pdf(paste0("../Results/FurtherMods/",sit,resource,"as_Vopt2.pdf"))
  print(p)
  dev.off()
  
  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_Vopt2.csv"))
}


SiteSearchRate.Vopt3 <- function(sit,sppC,sppR,consumer,resource){
  
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
  Lr <- mean(sppC$Length)/1000
  Lc <- mean(sppR$Length)/1000
  GCOTr <- GCOT3(mr/1000,Lr)
  GCOTc <- GCOT3(mc/1000,Lc)
  b0c <- Vopt(GMRr,mr/1000,GCOTr)
  b0r <- Vopt(GMRc,mc/1000,GCOTc)
  
  
  Temp <- seq(from = 273.15, to = 313.15, by = 0.5)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Model Strategy") + ggtitle(paste(sit,"Model Predictions")) +
    theme(plot.title = element_text(face="bold"))
  
  if (resource == "Chironomus" && sit == "Porto"){
    p <- p + geom_point(aes(x=15.1147834951,y=0.7710610339), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=15.1147834951,ymin = 0.7710610339-0.1054280698,ymax = 0.7710610339+0.1054280698))
  }
  if (resource == "Chironomus" && sit == "Evora"){
    p <- p + geom_point(aes(x=12.5511900369,y=2.0500255675), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.5511900369, ymin = 2.0500255675-0.2588407187,ymax = 2.0500255675+0.2588407187))
  }
  if (resource == "Cloeon" && sit == "Toledo"){
    p <- p + geom_point(aes(x=12.1199240069,y=0.1744503421), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.1199240069, ymin = 0.1744503421-0.049078476,ymax = 0.1744503421+0.049078476))
  }
  #open the pdf and put the plot in it
  pdf(paste0("../Results/FurtherMods/",sit,resource,"as_Vopt3.pdf"))
  print(p)
  dev.off()
  
  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_Vopt3.csv"))
}


SiteSearchRate.Vopt4 <- function(sit,sppC,sppR,consumer,resource){
  
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
  GCOTr <- GCOT4(mr/1000)
  GCOTc <- GCOT4(mc/1000)
  b0c <- Vopt(GMRr,mr/1000,GCOTr)
  b0r <- Vopt(GMRc,mc/1000,GCOTc)
  
  
  Temp <- seq(from = 273.15, to = 313.15, by = 0.5)
  
  sessile2D <- sessile.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  sessile3D <- sessile.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, mr = mr, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  active2D <- active.search.rate.2D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd2, i = i, c = c)
  active3D <- active.search.rate.3D(b0c = b0c, mc = mc, Ec = Ec, bc = bc, b0r = b0r, mr = mr, Er = Er, br = br, Temp = Temp, d0 = d0, Pd = Pd3, i = i, c = c)
  
  
  Temperature <- Temp - 273.15
  aS2D <- data.frame(Temperature,sessile2D)
  aS3D <- data.frame(Temperature,sessile3D)
  aA2D <- data.frame(Temperature,active2D)
  aA3D <- data.frame(Temperature,active3D)
  
  p <- ggplot() +
    geom_line(data = aS2D, aes(x = Temperature, y = sessile2D, colour = "2D sessile prey"), size = I(2), alpha = 0.7) + 
    xlab(expression(paste("Temperature (", degree, C, ")"))) + 
    ylab(expression(paste("Search rate"))) 
  p <- p + geom_line(data = aS3D, aes(x = Temperature, y = sessile3D, colour = "3D sessile prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA2D, aes(x = Temperature, y = active2D, colour = "2D active prey"), size = I(2), alpha = 0.7)
  p <- p + geom_line(data = aA3D, aes(x = Temperature, y = active3D, colour = "3D active prey"), size = I(2), alpha = 0.7)
  p <- p + scale_color_discrete(name = "Model Strategy") + ggtitle(paste(sit,"Model Predictions")) +
    theme(plot.title = element_text(face="bold"))
  
  if (resource == "Chironomus" && sit == "Porto"){
    p <- p + geom_point(aes(x=15.1147834951,y=0.7710610339), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=15.1147834951,ymin = 0.7710610339-0.1054280698,ymax = 0.7710610339+0.1054280698))
  }
  if (resource == "Chironomus" && sit == "Evora"){
    p <- p + geom_point(aes(x=12.5511900369,y=2.0500255675), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.5511900369, ymin = 2.0500255675-0.2588407187,ymax = 2.0500255675+0.2588407187))
  }
  if (resource == "Cloeon" && sit == "Toledo"){
    p <- p + geom_point(aes(x=12.1199240069,y=0.1744503421), color="black", size = I(2), alpha = 0.7)
    p <- p + geom_errorbar(aes(x=12.1199240069, ymin = 0.1744503421-0.049078476,ymax = 0.1744503421+0.049078476))
  }
  #open the pdf and put the plot in it
  pdf(paste0("../Results/FurtherMods/",sit,resource,"as_Vopt4.pdf"))
  print(p)
  dev.off()
  
  a.results <- data.frame(Temperature,sessile2D,sessile3D,active2D,active3D)
  write.csv(a.results,paste0("../Results/FurtherMods/a_predictions_",sit,resource,"_Vopt4.csv"))
}