SchoolfieldMTol <- function(B0, b, m, E, Ed, TempH, Temp) {
  #Where:
  # B0 - trait value at 273.15K
  # E - activation energy
  # Ed - enzyme's high temperature deactivation energy
  # Temp - temperature
  # TempH - temperature at which the enzyme is 50% high-temperature deactivated
  #let's log the model as our data is currently loged
  return(log(B0) + b*log(m) + log((exp((-E/k) * ((1/Temp) - (1/287.75))))/(1+ (E/(Ed - E)) * exp((Ed/k) * (1/TempH - 1/Temp)))))
}

SchoolfieldMEvo <- function(B0, b, m, E, Ed, TempH, Temp) {
  #Where:
  # B0 - trait value at 273.15K
  # E - activation energy
  # Ed - enzyme's high temperature deactivation energy
  # Temp - temperature
  # TempH - temperature at which the enzyme is 50% high-temperature deactivated
  #let's log the model as our data is currently loged
  return(log(B0) + b*log(m) + log((exp((-E/k) * ((1/Temp) - (1/290.45))))/(1+ (E/(Ed - E)) * exp((Ed/k) * (1/TempH - 1/Temp)))))
}

SchoolfieldMPor <- function(B0, b, m, E, Ed, TempH, Temp) {
  #Where:
  # B0 - trait value at 273.15K
  # E - activation energy
  # Ed - enzyme's high temperature deactivation energy
  # Temp - temperature
  # TempH - temperature at which the enzyme is 50% high-temperature deactivated
  #let's log the model as our data is currently loged
  return(log(B0) + b*log(m) + log((exp((-E/k) * ((1/Temp) - (1/290.05))))/(1+ (E/(Ed - E)) * exp((Ed/k) * (1/TempH - 1/Temp)))))
}