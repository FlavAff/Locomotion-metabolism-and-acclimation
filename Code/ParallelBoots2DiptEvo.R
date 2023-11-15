library(doParallel)

source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("nls_multstart.R")
source("Schoolfields2.R")

spp <- subset(dipt, dipt$site == "Evora")


runstrap <- function(data) {
  fit_boots <- data %>% 
    modelr::bootstrap(n = 1, id = 'boot_num') %>% #bootstrap
    group_by(boot_num) %>%
    mutate(fit = map(strap, ~nls_multstart(LogC ~ SchoolfieldMEvo(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                                           data= data.frame(.),  iter = 6000,
                                           start_lower = c(0.01, 0.1, 0.2, 1, 0), #set lower bound of starting points in paramaters
                                           start_upper = c(2, 2, 3, 40, 353.15), #set upper bound of starting points in paramaters
                                           supp_errors = 'Y',
                                           convergence_count = 2000,
                                           na.action = na.omit,
                                           lower=c(0.01, 0.5, 0.5, 2, 300)) #set lower bound of parameter values acceptable
    ))
}

# Use the environment variable SLURM_CPUS_PER_TASK to set the number of cores.
# This is for SLURM. Replace SLURM_CPUS_PER_TASK by the proper variable for your system.
# Avoid manually setting a number of cores.
ncores = Sys.getenv("SLURM_CPUS_PER_TASK") 

registerDoParallel(cores=ncores)# Shows the number of Parallel Workers to be used
print(ncores) # this how many cores are available, and how many you have requested.
getDoParWorkers()# you can compare with the number of actual workers

N <- 10000

a <- Sys.time()

foreach(i=iter(c(N:1)),
        .packages=c("plyr","minpack.lm","data.table","broom","tidyverse"), #indicate which packages to use
        .export  = ls(globalenv()), #export everything from the current environment to the core - can edit to just what is needed
        .combine = rbind #combine all input in one (not necessary to do this)
) %dopar% {
  
  fit_boots <- runstrap(spp)
  
  params_boot <- map(fit_boots$fit, tidy) %>% #tidy up parameters from each model
    map_df(as_tibble) %>% #make this into a dataframe
    ungroup() %>% #ungroup
    mutate(boot_num = rep(i, each = 5))
  
  write.csv(params_boot,paste0("../Results/SchoolField/Boots/Cloeon/Evora/Boot",i,".csv"))
}

Sys.time() - a