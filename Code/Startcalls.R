source("StartingValues.R")


#Starting parameter estimations

MurciaNo_strt <- function(df){
  location <- subset(df, df$site == "Murcia")
  
  B0 <- GetB0(location)
  Ea <- GetE(location)
  Ed <- GetEd(location)
  Tpk <- GetTpk(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

ToledoNo_strt <- function(df){
  location <- subset(df, df$site == "Toledo")
  
  B0 <- GetB0(location)
  Ea <- GetE(location)
  Ed <- GetEd(location)
  Tpk <- GetTpk(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

EvoraNo_strt <- function(df){
  location <- subset(df, df$site == "Evora")
  
  B0 <- GetB0(location)
  Ea <- GetE(location)
  Ed <- GetEd(location)
  Tpk <- GetTpk(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

PortoNo_strt <- function(df){
  location <- subset(df, df$site == "Porto")
  
  B0 <- GetB0(location)
  Ea <- GetE(location)
  Ed <- GetEd(location)
  Tpk <- GetTpk(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

JacaNo_strt <- function(df){
  location <- subset(df, df$site == "Jaca")
  
  B0 <- GetB0(location)
  Ea <- GetE(location)
  Ed <- GetEd(location)
  Tpk <- GetTpk(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

PenalaraNo_strt <- function(df){
  location <- subset(df, df$site == "Penalara")
  
  B0 <- GetB0(location)
  Ea <- GetE(location)
  Ed <- GetEd(location)
  Tpk <- GetTpk(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}












MurciaM_strt <- function(df){
  location <- subset(df, df$site == "Murcia")
  
  B0 <- GetB0Corr(location)
  Ea <- GetECorr(location)
  Ed <- GetEdCorr(location)
  Tpk <- GetTpkCorr(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

ToledoM_strt <- function(df){
  location <- subset(df, df$site == "Toledo")
  
  B0 <- GetB0Corr(location)
  Ea <- GetECorr(location)
  Ed <- GetEdCorr(location)
  Tpk <- GetTpkCorr(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

EvoraM_strt <- function(df){
  location <- subset(df, df$site == "Evora")
  
  B0 <- GetB0Corr(location)
  Ea <- GetECorr(location)
  Ed <- GetEdCorr(location)
  Tpk <- GetTpkCorr(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

PortoM_strt <- function(df){
  location <- subset(df, df$site == "Porto")
  
  B0 <- GetB0Corr(location)
  Ea <- GetECorr(location)
  Ed <- GetEdCorr(location)
  Tpk <- GetTpkCorr(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

JacaM_strt <- function(df){
  location <- subset(df, df$site == "Jaca")
  
  B0 <- GetB0Corr(location)
  Ea <- GetECorr(location)
  Ed <- GetEdCorr(location)
  Tpk <- GetTpkCorr(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}

PenalaraM_strt <- function(df){
  location <- subset(df, df$site == "Penalara")
  
  B0 <- GetB0Corr(location)
  Ea <- GetECorr(location)
  Ed <- GetEdCorr(location)
  Tpk <- GetTpkCorr(location)
  
  return(data.frame(B0,Ea,Ed,Tpk))
}