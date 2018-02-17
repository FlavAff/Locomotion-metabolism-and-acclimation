rm(list =ls())
dev.off()

setwd("~/Documents/GitHub/Locomotion-metabolism-and-acclimation/Code/Temperature and LW/")
library(plyr)
library(data.table)

dat <- read.csv("../../Data/Mesocosms_temperatures4.csv")
dat$Temperature <- as.numeric(levels(dat$Temperature)[dat$Temperature])
dat <- subset(dat, dat$Temperature != "NA")
#dat <- subset(dat, dat$Temperature <= 30)
#dat <- subset(dat, dat$Temperature >= 0)
names(dat)
levels(dat$Site)
#recorded air temperatures for each site
MaxTemp <- data.frame(Site = c(rep("Murcia",12),rep("Toledo",12),rep("Evora",12),rep("Porto",12),rep("Jaca",12),rep("Penalara",12)),
                      Month = c(rep(c(1:12),6)),
                      Max = c(16.4,18.2,20.4,22.5,25.8,30,33.4,33.6,30.2,25,20,17,
                              11.2,13.6,17.1,18.8,23.1,29,33.6,33.1,28.4,21.4,15.2,11.5,
                              12.8,13.7,16.4,17.9,21.2,26.3,30.2,30.1,27.2,21.4,16.7,13.6,
                              13.5,14.3,16.2,17.5,19.6,22.7,24.7,25,24,20.9,16.7,13.9,
                              8.5,11.5,15,17.2,21.3,26.5,30.8,30.2,25.4,19.1,12.9,9.1,
                              2,2.5,4.7,5.7,10.2,16.3,21.2,21.2,16.6,9.8,5.4,3.2),
                      Min = c(3.9,5.9,6.7,8.7,12.2,16.2,19,19.9,16.9,12.7,8.2,5.2,
                              1.6,3,4.8,6.9,10.8,15.2,18.5,18.3,14.8,9.9,5.3,3,
                              5.8,6.6,7.7,8.8,11,14,16.2,16.4,15.6,12.5,9.4,7.2,
                              5.1,5.9,6.8,8.3,10.6,13.5,15,14.6,13.9,11.4,7.9,5.9,
                              1.3,2.5,4.2,5.7,9.3,12.9,16.1,16.3,13.5,9.5,4.9,2.1,
                              -3.1,-2.9,-1.7,-0.8,2.8,7.5,11.3,11.3,8.2,3.6,0.2,-1.7),
                      Norm = c(10.1,11.7,13.5,15.6,19,23.1,26.2,26.7,23.6,18.8,14.1,11.1,
                               6.3,8.3,11,12.9,16.9,22.1,26,25.7,21.6,15.6,10.3,7.3,
                               9.3,10.1,12,13.4,16.1,20.1,23.2,23.3,21.4,17,13.1,10.4,
                               9.3,10.1,11.5,12.9,15.1,18.1,19.9,19.8,19,16.2,12.3,9.9,
                               4.9,7,9.6,11.4,15.3,19.7,23.4,23.3,19.5,14.3,8.9,5.6,
                               -0.6,-0.2,1.5,2.5,6.5,11.9,16.2,16.2,12.4,6.7,2.8,0.7))
#loop to rid any points over the max recorded ar temp of a given site
#stores each month individually in a list per site and creates a list containing each site's list
month <- list()
sites <- list()
for (j in unique(dat$Site)){
  loc <- subset(dat, dat$Site == j)
  MaxTempL <- subset(MaxTemp, MaxTemp$Site == j)
  for (k in c(1:12)){
    locM <- subset(loc, loc$Month == k)
    MaxTempM <- subset(MaxTempL, MaxTempL$Month == k)
    
    newdat <- subset(locM, locM$Temperature <= MaxTempM$Max)
    newdat <- subset(newdat, newdat$Temperature >= 0)
  month[[k]] <- newdat
  }
  sites[[j]] <- month}
#extract dataframes per site and bind them all to one
MesoTemps <- rbind(dplyr::bind_rows(sites$Murcia),dplyr::bind_rows(sites$Toledo),dplyr::bind_rows(sites$Evora),
                dplyr::bind_rows(sites$Porto),dplyr::bind_rows(sites$Jaca),dplyr::bind_rows(sites$Penalara))
MesoTempsSub <- subset(MesoTemps, MesoTemps$Month %in% c(1:5,8:12))

Results <- data.frame(matrix(nrow=6, ncol=6))
Results <- rename(Results, c("X1" = "Mean Temperature","X2" = "Variance in Temperature","X3" = "Median Temperature",
                             "X4" = "Minimum Temperature", "X5"= "Maximum Temperature", "X6"="Mean SE"))
setattr(Results, "row.names", c("Evora","Jaca","Murcia","Penalara","Porto","Toledo"))

for (i in c("Evora","Jaca","Murcia","Penalara","Porto","Toledo")){
  
  site <- subset(MesoTemps, MesoTemps$Site == i)
  
  T.mean <- mean(site$Temperature)
  T.var <- var(site$Temperature)
  T.median <- median(site$Temperature)
  T.min <- min(site$Temperature)
  T.max <- max(site$Temperature)
  T.mean.se <- sd(site$Temperature)/sqrt(length(site$Temperature))
  
  Results[i,"Mean Temperature"] = T.mean
  Results[i,"Variance in Temperature"] = T.var
  Results[i,"Median Temperature"] = T.median
  Results[i,"Minimum Temperature"] = T.min
  Results[i,"Maximum Temperature"] = T.max
  Results[i,"Mean SE"] = T.mean.se
}

write.csv(Results, paste0("../../Results/Temperature/MeanTemp.csv"))
tiff("../../Results/Temperature/TBox.tiff", width = 20, height = 15, units = 'cm', res = 300, compression = 'lzw')
plot(MesoTemps$Site,MesoTemps$Temperature)
dev.off()
tiff("../../Results/Temperature/TBox2.tiff", width = 20, height = 15, units = 'cm', res = 300, compression = 'lzw')
plot(MesoTempsSub$Site,MesoTempsSub$Temperature)
dev.off()
