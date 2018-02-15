
# function that calculates activation energy
GetECorr <- function(site, k=8.62e-5)                
{
  #get the maximum O2 consumption rate
  MaxC <- max(site$Mass.corrC)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$Mass.corrC == MaxC,]$chamber.T.Kelvin[1]
  
  #get subset until and after max Temperature
  upwardDF <- site[site$chamber.T.Kelvin <= MaxTemp,]
  
  #run linear models according to MTE
  LMa <- lm(upwardDF$LogC.corr ~ upwardDF$One.over.kT)
  
  #put all parameters in a dataframe
  return(as.numeric(abs(LMa$coefficients[2])))
}

GetEdCorr <- function(site, k=8.86e-5)
{
  #get the maximum O2 consumption rate
  MaxC <- max(site$Mass.corrC)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$Mass.corrC == MaxC,]$chamber.T.Kelvin[1]
  
  #get subset until and after max Temperature
  downwardDF <- site[site$chamber.T.Kelvin >= MaxTemp,]
  
  #run linear models according to MTE
  LMd <- lm(downwardDF$LogC.corr ~ downwardDF$One.over.kT)
  
  #put all parameters in a dataframe
  return(as.numeric(abs(LMd$coefficients[2])))
}


# function that calculate the normalising constant
GetB0Corr <- function(site)
{
  #get the maximum O2 consumption rate
  MaxC <- max(site$Mass.corrC)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$Mass.corrC == MaxC,]$chamber.T.Kelvin[1]
  
  #get subset until and after max Temperature
  upwardDF <- site[site$chamber.T.Kelvin <= MaxTemp,]
  
  #run linear models according to MTE
  LMa <- lm(upwardDF$LogC.corr ~ upwardDF$One.over.kT)
  
  #put all parameters in a dataframe
  return(as.numeric(LMa$coefficients[1]))
}


# the temperature at which the highest trait value occurs
GetTpkCorr <- function(site)
{
  MaxC <- max(site$Mass.corrC)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$Mass.corrC == MaxC,]$chamber.T.Kelvin[1]
  
  return(MaxTemp)
}




# function that calculates activation energy
GetE <- function(site, k=8.62e-5)                
{
  #get the maximum O2 consumption rate
  MaxC <- max(site$O2.consumption)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$O2.consumption == MaxC,]$chamber.T.Kelvin[1]
  
  #get subset until and after max Temperature
  upwardDF <- site[site$chamber.T.Kelvin <= MaxTemp,]
  
  #run linear models according to MTE
  LMa <- lm(upwardDF$LogC ~ upwardDF$One.over.kT)
  
  #put all parameters in a dataframe
  return(as.numeric(abs(LMa$coefficients[2])))
}

GetEd <- function(site, k=8.86e-5)
{
  #get the maximum O2 consumption rate
  MaxC <- max(site$O2.consumption)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$O2.consumption == MaxC,]$chamber.T.Kelvin[1]
  
  #get subset until and after max Temperature
  downwardDF <- site[site$chamber.T.Kelvin >= MaxTemp,]
  
  #run linear models according to MTE
  LMd <- lm(downwardDF$LogC ~ downwardDF$One.over.kT)
  
  #put all parameters in a dataframe
  return(as.numeric(abs(LMd$coefficients[2])))
}


# function that calculate the normalising constant
GetB0 <- function(site)
{
  #get the maximum O2 consumption rate
  MaxC <- max(site$O2.consumption)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$O2.consumption == MaxC,]$chamber.T.Kelvin[1]
  
  #get subset until and after max Temperature
  upwardDF <- site[site$chamber.T.Kelvin <= MaxTemp,]
  
  #run linear models according to MTE
  LMa <- lm(upwardDF$LogC ~ upwardDF$One.over.kT)
  
  #put all parameters in a dataframe
  return(as.numeric(LMa$coefficients[1]))
}


# the temperature at which the highest trait value occurs
GetTpk <- function(site)
{
  MaxC <- max(site$O2.consumption)
  
  #identify temperature at which this trait occurs
  MaxTemp <- site[site$O2.consumption == MaxC,]$chamber.T.Kelvin[1]
  
  return(MaxTemp)
}