chir$Mean.mass <- NA
dipt$Mean.mass <- NA
strio$Mean.mass <- NA

results <- data.frame()

add_mean_mass_chir <- function(dataset){

  for (j in c("Evora","Penalara","Porto","Toledo")){
    
    location <- subset(dataset, dataset$site == j)
  
    m10 <- mean(location[location$chamber.T == 10,]$Mass)
    m15 <- mean(location[location$chamber.T == 15,]$Mass)
    m20 <- mean(location[location$chamber.T == 20,]$Mass)
    m25 <- mean(location[location$chamber.T == 25,]$Mass)
    m30 <- mean(location[location$chamber.T == 30,]$Mass)
    m35 <- mean(location[location$chamber.T == 35,]$Mass)
    m40 <- mean(location[location$chamber.T == 40,]$Mass)
    m45 <- mean(location[location$chamber.T == 45,]$Mass)

      for (i in 1:nrow(location)){
  
        if (location[i,"chamber.T"] == 10){
        location[i,"Mean.mass"] <- m10
      }
  
        if (location[i,"chamber.T"] == 15){
        location[i,"Mean.mass"] <- m15
      }
  
        if (location[i,"chamber.T"] == 20){
        location[i,"Mean.mass"] <- m20
      }
  
        if (location[i,"chamber.T"] == 25){
        location[i,"Mean.mass"] <- m25
      }
  
        if (location[i,"chamber.T"] == 30){
        location[i,"Mean.mass"] <- m30
      }
  
        if (location[i,"chamber.T"] == 35){
        location[i,"Mean.mass"] <- m35
      }
  
        if (location[i,"chamber.T"] == 40){
        location[i,"Mean.mass"] <- m40
      }
  
        if (location[i,"chamber.T"] == 45){
        location[i,"Mean.mass"] <- m45
      }
    }
    results <- rbind(results, location)
  }
  return(results)
}


chir <- add_mean_mass_chir(chir)



add_mean_mass_dipt <- function(dataset){
  
  for (j in c("Evora","Murcia","Porto","Toledo")){
    
    location <- subset(dataset, dataset$site == j)
    
    m10 <- mean(location[location$chamber.T == 10,]$Mass)
    m15 <- mean(location[location$chamber.T == 15,]$Mass)
    m20 <- mean(location[location$chamber.T == 20,]$Mass)
    m25 <- mean(location[location$chamber.T == 25,]$Mass)
    m30 <- mean(location[location$chamber.T == 30,]$Mass)
    m35 <- mean(location[location$chamber.T == 35,]$Mass)
    m40 <- mean(location[location$chamber.T == 40,]$Mass)
    m45 <- mean(location[location$chamber.T == 45,]$Mass)
    
    for (i in 1:nrow(location)){
      
      if (location[i,"chamber.T"] == 10){
        location[i,"Mean.mass"] <- m10
      }
      
      if (location[i,"chamber.T"] == 15){
        location[i,"Mean.mass"] <- m15
      }
      
      if (location[i,"chamber.T"] == 20){
        location[i,"Mean.mass"] <- m20
      }
      
      if (location[i,"chamber.T"] == 25){
        location[i,"Mean.mass"] <- m25
      }
      
      if (location[i,"chamber.T"] == 30){
        location[i,"Mean.mass"] <- m30
      }
      
      if (location[i,"chamber.T"] == 35){
        location[i,"Mean.mass"] <- m35
      }
      
      if (location[i,"chamber.T"] == 40){
        location[i,"Mean.mass"] <- m40
      }
      
      if (location[i,"chamber.T"] == 45){
        location[i,"Mean.mass"] <- m45
      }
    }
    results <- rbind(results, location)
  }
  return(results)
}

dipt <- add_mean_mass_dipt(dipt)



add_mean_mass_strio <- function(dataset){
  
  for (j in c("Evora","Jaca","Porto","Toledo")){
    
    location <- subset(dataset, dataset$site == j)
    
    m10 <- mean(location[location$chamber.T == 10,]$Mass)
    m15 <- mean(location[location$chamber.T == 15,]$Mass)
    m20 <- mean(location[location$chamber.T == 20,]$Mass)
    m25 <- mean(location[location$chamber.T == 25,]$Mass)
    m30 <- mean(location[location$chamber.T == 30,]$Mass)
    m35 <- mean(location[location$chamber.T == 35,]$Mass)
    m40 <- mean(location[location$chamber.T == 40,]$Mass)
    m45 <- mean(location[location$chamber.T == 45,]$Mass)
    
    for (i in 1:nrow(location)){
      
      if (location[i,"chamber.T"] == 10){
        location[i,"Mean.mass"] <- m10
      }
      
      if (location[i,"chamber.T"] == 15){
        location[i,"Mean.mass"] <- m15
      }
      
      if (location[i,"chamber.T"] == 20){
        location[i,"Mean.mass"] <- m20
      }
      
      if (location[i,"chamber.T"] == 25){
        location[i,"Mean.mass"] <- m25
      }
      
      if (location[i,"chamber.T"] == 30){
        location[i,"Mean.mass"] <- m30
      }
      
      if (location[i,"chamber.T"] == 35){
        location[i,"Mean.mass"] <- m35
      }
      
      if (location[i,"chamber.T"] == 40){
        location[i,"Mean.mass"] <- m40
      }
      
      if (location[i,"chamber.T"] == 45){
        location[i,"Mean.mass"] <- m45
      }
    }
    results <- rbind(results, location)
  }
  return(results)
}


strio <- add_mean_mass_strio(strio)

