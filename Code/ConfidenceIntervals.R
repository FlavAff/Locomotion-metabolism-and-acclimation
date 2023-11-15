
#Import values
values <- read.csv("../Results/SchoolField/New/SchoolFieldRunsMean.csv")
values$site <- factor(values$site, levels = c("Porto","Toledo","Evora"))

site <- subset(chir, chir$site == "Toledo")
N <- values

#Run NLS
NLS <- nlsLM(LogC ~ SchoolfieldM(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                       data = site,
                       start = list(B0 = N$constant_strt[1], b = 0.75, E = N$E_a_strt[1], Ed = N$E_d_strt[1], TempH = N$T_pk_strt[1]),
                       control = nls.lm.control(maxiter = 1000, ftol = .Machine$double.eps, ptol = .Machine$double.eps,maxfev = 1000),
                       lower = c(-Inf, -Inf, 0, 0, 0),
                       upper = c(Inf, Inf, Inf, Inf, 353.15))
#Get varcov matrix
V <- vcov(NLS)

source("Derivatives.R")

#Get derivatives of each parameter
Mean.mass <- seq(from = min(site$Mean.mass), to = max(site$Mass), length.out = 71)
chamber.T.Kelvin <- seq(from=283.15, to = 318.15, by = 0.5)
df <- data.frame(cbind(Mean.mass,chamber.T.Kelvin))
df$B0 <- 1/N$B0_end[19]
df$b <-NA
df$E <- NA
df$Ed <- NA
df$TempH <- NA
for (i in 1:nrow(df)) {
  df[i,"b"] <- log(df[i,"Mean.mass"])
  df[i,"E"] <- dy.dE(E = N$Ea_end[19], Ed = N$Ed_end[19], Temp = df[i,"chamber.T.Kelvin"], TempH = N$Tpk_end[19])
  df[i,"Ed"] <- dy.dEd(E = N$Ea_end[19], Ed = N$Ed_end[19], Temp = df[i,"chamber.T.Kelvin"], TempH = N$Tpk_end[19])
  df[i,"TempH"] <- dy.dTempH(E = N$Ea_end[19], Ed = N$Ed_end[19], Temp = df[i,"chamber.T.Kelvin"], TempH = N$Tpk_end[19])
}
#Make derivative matrix
X <- cbind(df$B0,df$b,df$E,df$Ed,df$TempH)
colnames(X) <- c("B0","b","E","Ed","TempH")
#Calculate se of the curve
se.fit <- sqrt(diag(X %*% V %*% t(X)))

fit <- ChirPredictions$d1

predframe <- data.frame(cbind(fit,lwr=fit-1.96*se.fit,upr=fit+1.96*se.fit,chamber.T.Kelvin))


p <- ggplot(data=predframe,aes(x=chamber.T.Kelvin))+
  theme_classic(base_size = 10,base_family = "sans") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("Metabolic rate") + xlab("Temperature") + 
 # geom_point(data = site, aes(x=chamber.T.Kelvin,y=LogC)) +
  geom_line(aes(y=fit),colour="red")+
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.3)
