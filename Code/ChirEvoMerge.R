library(tidyverse)

df <- list.files(path='../Results/SchoolField/Boots/Chironomus/Evora/',  # Identify all CSV files
                 pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(df, "../Results/SchoolField/Boots/Chironomus/Evora/mergedChirEvo.csv")