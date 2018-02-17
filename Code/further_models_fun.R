power <- function(b0){
  
  return((b0*(31.988*10^(-3))*(3.34*1000)*4.1868)/3600)
}

GCOT <- function(M) {
  
  return(1.1*M^(-0.38))
}

Vopt <- function(GMR,M,GCOT) {
  
  return(GMR/(GCOT*M*9.82))
}

active.search.rate.2D <- function(b0c,mc,Ec,bc,b0r,mr,Er,br,Temp,d0,Pd,i,c){
  
  return((i*c*b0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/283.15)) + sqrt(1 + (i*c*(b0r/b0c))^2 * ((mr^(br))/(mc^(bc)))^2 * exp(2/k*(1/Temp-1/278.15)*(Ec-Er))))*2*d0*(mr*mc)^(Pd))
}

active.search.rate.3D <- function(b0c,mc,Ec,bc,b0r,mr,Er,br,Temp,d0,Pd,i,c){
  
  return((i*c*b0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/283.15)) + sqrt(1 + (i*c*(b0r/b0c))^2 * ((mr^(br))/(mc^(bc)))^2 * exp(2/k*(1/Temp-1/278.15)*(Ec-Er))))*pi*(d0*(mr*mc)^(Pd))^2)
}

sessile.search.rate.2D <- function(b0c,mc,Ec,bc,mr,Temp,d0,Pd,i,c){
  
  return(i*c*b0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/283.15)) * 2*d0*(mr*mc)^(Pd))
}

sessile.search.rate.3D <- function(b0c,mc,Ec,bc,mr,Temp,d0,Pd,i,c){
  
  return((i*c*b0c*mc^(bc)*exp((-Ec/k)*(1/Temp-1/283.15)) * pi*(d0*(mr*mc)^(Pd))^2))
}