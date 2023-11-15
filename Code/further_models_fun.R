power <- function(b0){
  
  return((b0*(31.988*10^(-3))*(3.34*1000)*4.1868)/3600)
}

GCOT <- function(M) {
  
  return(1.1*M^(-0.38))
}

Vopt <- function(GMR,M,GCOT) {
  
  return(GMR/(GCOT*M*9.82))
}

Vopt2 <- function(GMR,M) {
  
  return(GMR/(1.1*M^(0.62)*9.82))
}

active.search.rate.2D <- function(b0c,mc,Ec,bc,b0r,mr,Er,br,Temp,d0,Pd,i,c,Tref){
  
  return(sqrt(b0r^2*mr^(2*br)*exp((-2*Er/k)*(1/Temp-1/Tref))+b0c^2*mc^(2*bc)*exp((-2*Ec/k)*(1/Temp-1/Tref)))*2*d0*(mr*mc)^(Pd))
}

active.search.rate.3D <- function(b0c,mc,Ec,bc,b0r,mr,Er,br,Temp,d0,Pd,i,c,Tref){
  
  return(sqrt(b0r^2*mr^(2*br)*exp((-2*Er/k)*(1/Temp-1/Tref))+b0c^2*mc^(2*bc)*exp((-2*Ec/k)*(1/Temp-1/Tref)))*pi*(d0*(mr*mc)^(Pd))^2)
}

sessile.search.rate.2D <- function(b0c,mc,Ec,bc,mr,Temp,d0,Pd,i,c,Tref){
  
  return(i*c*b0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/Tref)) * 2*d0*(mr*mc)^(Pd))
}

sessile.search.rate.3D <- function(b0c,mc,Ec,bc,mr,Temp,d0,Pd,i,c,Tref){
  
  return((i*c*b0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/Tref)) * pi*(d0*(mr*mc)^(Pd))^2))
}