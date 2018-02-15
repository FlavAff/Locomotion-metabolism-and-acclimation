rm(list = ls())
dev.off()

setwd("~/Documents/Stuff/Project/Code/Temperature and other code/")
library(plyr)
library(data.table)
library(ggplot2)

dat <- read.csv("../../Data/2017Temp.csv")
dat2 <- read.csv("../../Data/2016Temp.csv")
Tdat <- rbind(dat,dat2)


for (i in levels(Tdat$Site)){
  data <- subset(Tdat, Tdat$Site == i)
  
  p <- ggplot(data, aes(Month, Temperature))
  
  p <- p + geom_boxplot()
  
  pdf(paste0("../Results/Temperature/",i,data$Year[1],"Boxplot.pdf"))
  print(p)
  dev.off()
}


for (i in levels(Tdat$Site)){
  dat <- subset(Tdat, Tdat$Site == i)
  
  p <- ggplot(data=dat, aes(dat$Temperature)) + geom_histogram(binwidth = 1, alpha=.2, col = "red", fill = "blue")
  p <- p + labs(title=paste(i,"temperature histogram")) + labs(x="Temperature", y="Count")
  
  pdf(paste0("../Results/Temperature/",i,data$Year[1],"THist.pdf"))
  print(p)
  dev.off()
}


MonthVal <- data.frame(Site=character(),Month=character(),Mean.temp=numeric(),Var.temp=numeric(),Med.temp=numeric())

for (i in levels(Tdat$Site)){
  data <- subset(dat, dat$Site == i)
    for (j in levels(data$Month)){
      data2 <- subset(data, data$Month == j)
    
      T.mean <- mean(data2$Temperature)
      T.var <- var(data2$Temperature)
      T.median <- median(data2$Temperature)
      
      MonthPara <- data.frame(Site = i, Month = j, Mean.temp = T.mean, Var.temp = T.var, Med.temp = T.median)
      MonthVal <- rbind(MonthVal,MonthPara)
    }
  write.csv(MonthVal, paste0("../Results/Temperature/MonthlyTemp.csv"))
}
  