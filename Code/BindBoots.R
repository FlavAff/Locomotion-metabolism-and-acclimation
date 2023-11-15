setwd("~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Code/")
library(tidyverse)

df <- list.files(path='~/Documents/My Papers/Open/Git-Locomotion-metabolism-and-acclimation/Results/SchoolField/Boots/Chironomus/Toledo/',  # Identify all CSV files
                 pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(df, "../Results/SchoolField/Boots/Chironomus/Toledo/mergedChirTol.csv")

