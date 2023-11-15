library(tidyverse)

df <- list.files(path='../Results/SchoolField/Boots/Sympetrum/Evora',  # Identify all CSV files
                 pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(df, "../Results/SchoolField/Boots/Sympetrum/Evora/mergedStrioEvo.csv")