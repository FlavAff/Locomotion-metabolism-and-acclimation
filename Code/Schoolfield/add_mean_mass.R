#This function adds the Mean.mass column to the dataset for estimation of the b parameter with NLLS
#requires an input species specific dataset and a vector of sites at which to estimate mean mass
#requires a column called Mass containing the mass of each individual
#calculates and adds mean mass values for 10 to 45 °C by 5 °C
#returns a the same dataframe that was inputted with an extra Mean.mass column

add_mean_mass <- function(dataset,sites){
  
  results <- data.frame()
  
  for (j in sites){
    
    location <- subset(dataset, dataset$site == j)
    
    location$Mean.mass <- NA
    
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