MyData <- read.csv("../Data/Combined_Systems.csv")

names(MyData)

#remove missing values in dataset
MyData <- subset(MyData, MyData$slope..vol.corr...umol.hr....Unisense.rate.corrected.for.chamber.volume != "NA")
MyData <- subset(MyData, MyData$biomass..mg..calculate.using.L.W.regressions != "NA")
MyData <- subset(MyData, MyData$species != "NA")
MyData <- subset(MyData, MyData$species == "dipterum" | MyData$species == "territans" | MyData$species == "striolatum" | MyData$genus == "Chironomus")

#set Boltzmann constant
k <- 8.617*(10^-5)

#Subset for predator-prey couple of interest
MyData$chamber.T.Kelvin <- MyData$acute.temp + 273.15


#Make dataset neater with more useful column names and necessary columns for MTE application
MyData$O2.consumption <- MyData$O2.CONSUMP...inverse.slope....then.take.the.inverse
MyData$chamber.T <- MyData$acute.temp
MyData$Mass <- MyData$biomass..mg..calculate.using.L.W.regressions
MyData$Length <- MyData$body.length..mm..calculate.using.microscope.determined.length
MyData$Mass.corrC <- MyData$O2.consumption/((MyData$Mass)^0.75)
MyData$LogC.corr <- log(MyData$Mass.corrC)
MyData$LogC <- log(MyData$O2.consumption)


NewData <- MyData[,c("family","genus","species","site","O2.consumption","Mass.corrC","LogC","LogC.corr","Mass","Length","chamber.T","chamber.T.Kelvin")]

#Remove any non sensical values
NewData <- subset(NewData, NewData$LogC!="NaN")
NewData <- subset(NewData, NewData$LogC!="Inf")
NewData <- subset(NewData, NewData$LogC.corr!="Inf")
NewData <- subset(NewData, NewData$Mass.corrC!="Inf")
NewData <- subset(NewData, NewData$Mass >= 0)
NewData$One.over.kT <- (1/(k*NewData$chamber.T.Kelvin))
NewData$work <- 0.1242548*NewData$O2.consumption


#create species specific datasets to call functions on to avoid slow double loops
strio <- subset(NewData, NewData$species == "striolatum")
terr <- subset(NewData, NewData$species == "territans")
dipt <- subset(NewData, NewData$species == "dipterum")
chir <- subset(NewData, NewData$genus == "Chironomus")



source("get_mean_mass.R")