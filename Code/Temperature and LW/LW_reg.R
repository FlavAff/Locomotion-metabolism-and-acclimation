rm(list = ls())
setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/Temperature and LW/")
LW <- read.csv("../../Data/LW_reg.csv")


for (i in LW$Taxa){
  dat <- subset(LW, LW$Taxa == i)
    
  dat <- subset(dat, dat$Org.Weight.g. >= 0)
  dat$log.mass <- log(dat$Org.Weight.g.)
  dat <- subset(dat, dat$Org.Weight.g. != "NaN")
  dat <- subset(dat, dat$Body.Length.mm. >= 0)
    
  m1 <- lm(dat$Org.Weight.g. ~ dat$Body.Length.mm.)
  m2 <- lm(dat$log.mass ~ dat$Body.Length.mm.)
  
  if (summary(m1)$r.squared < summary(m2)$r.squared){
    
    pdf(paste0("Results/LW_reg/",i,"_logreg.pdf"))
    plot(dat$log.mass ~ dat$Body.Length.mm., xlab = "Length (mm)", ylab = "Log Weight (g)", main = paste(i,"length-weight regression"))
    abline(a = m2$coefficients[1], b = m2$coefficients[2])
    dev.off()
    
    LMFit <- data.frame(Intercept = m2$coefficients[1],
                        Slope = m2$coefficients[2],
                        R.squared = summary(m2)$r.squared,
                        F.statistic = anova(m2)[1,4],
                        p.value = anova(m2)[1,5],
                        AIC = AIC(m2),
                        BIC = BIC(m2))
    
    write.csv(LMFit, paste0("Results/LW_reg/",i,"_log_LMFit.csv"))
  }
  
  
  if (summary(m1)$r.squared > summary(m2)$r.squared){
    
  pdf(paste0("Results/LW_reg/",i,"_reg.pdf"))
  plot(dat$Org.Weight.g. ~ dat$Body.Length.mm., xlab = "Length (mm)", ylab = "Weight (g)", main = paste(i,"length-weight regression"))
  abline(a = m1$coefficients[1], b = m1$coefficients[2])
  dev.off()
  
  LMFit <- data.frame(Intercept = m1$coefficients[1],
                        Slope = m1$coefficients[2],
                        R.squared = summary(m1)$r.squared,
                        F.statistic = anova(m1)[1,4],
                        p.value = anova(m1)[1,5],
                        AIC = AIC(m1),
                        BIC = BIC(m1))
  
  write.csv(LMFit, paste0("Results/LW_reg/",i,"_LMFit.csv"))
  }
  
}
