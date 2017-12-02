rm(list = ls())
dev.off()

#Analysis will require the following packages
if("minpack.lm" %in% rownames(installed.packages()) == FALSE){
  install.packages("minpack.lm")
}
if("truncnorm" %in% rownames(installed.packages()) == FALSE){
  install.packages("truncnorm")
}

#Set the directory and source the function scripts
setwd("Clean Code/")
source("add_mean_mass.R")
source("ExplainedNecessFuns.R")

#Import a dataset
dataset <- read.csv("Data.csv")

#Remember, we need it to be species specific, in this example I extract one species from my df
#this df must contain the right column names and data explained in the sourced scripts
strio <- subset(dataset, dataset$species == "striolatum")

#First we add the Mean.mass column (this could be automated in another script)
strio <- add_mean_mass(dataset = strio, sites = c("Toledo","Evora","Porto","Jaca"))

#Get the starting values for this species at a given site
strioTo <- Start_vals(strio,"Toledo")

#Create the dataframe to store 10 NLLS output and randomise starting values
strioTo <- Start_random(Paras = strioTo, Ntries = 10)

#Run the 10 NLLS fits using the Schoolfield model to estimate b with mass
strioTo <- Fit_Schoolfield(Ntries = 100, N = strioTo, site = subset(strio, strio$site == "Toledo"), TempN = 284.642, mass = TRUE)
#ignore the warnings, there should be some converged runs

#Run the model selection function
strioTo <- Select_startval(N = strioTo)

#You now have 1 row dataframe containing all the information corresponding to the best fit NLLS run
print(strioTo)
