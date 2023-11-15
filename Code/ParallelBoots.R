library(doParallel)

source("DataClean.R")
rm(list=setdiff(ls(), c("chir","dipt","strio","k")))
source("nls_multstart.R")
source("Schoolfields2.R")

tol.chir <- subset(chir, chir$site == "Toledo")


runstrap <- function(data) {
  fit_boots <- data %>% 
    modelr::bootstrap(n = 1, id = 'boot_num') %>% #bootstrap
    group_by(boot_num) %>%
    mutate(fit = map(strap, ~nls_multstart(LogC ~ SchoolfieldM(B0, b, m = Mean.mass, E, Ed, TempH, Temp = chamber.T.Kelvin),
                                           data= data.frame(.),  iter = 6000,
                                           start_lower = c(0.01, 0.1, 0.2, 1, 0), #set lower bound of starting points in paramaters
                                           start_upper = c(2, 2, 3, 40, 353.15), #set upper bound of starting points in paramaters
                                           supp_errors = 'Y',
                                           convergence_count = 2000,
                                           na.action = na.omit,
                                           lower=c(0.1, 0.5, 0.5, 2, 300)) #set lower bound of parameter values acceptable
    ))
}

print(Sys.getenv("NODESLIST"))

# Create an array from the NODESLIST environnement variable
nodeslist = unlist(strsplit(Sys.getenv("NODESLIST"), split=" "))

# Create the cluster with the nodes name. One process per count of node name.
# nodeslist = node1 node1 node2 node2, means we are starting 2 processes on node1, likewise on node2.
cl = makeCluster(nodeslist, type = "PSOCK") 
registerDoParallel(cl)

N <- 2

a <- Sys.time()

foreach(i=iter(c(N:1)),
        .packages=c("plyr","minpack.lm","data.table","broom","tidyverse"), #indicate which packages to use
        .export  = ls(globalenv()), #export everything from the current environment to the core - can edit to just what is needed
        .combine = rbind #combine all input in one (not necessary to do this)
) %dopar% {
  
  fit_boots <- runstrap(tol.chir)
  
  params_boot <- map(fit_boots$fit, tidy) %>% #tidy up parameters from each model
    map_df(as_tibble) %>% #make this into a dataframe
    ungroup() %>% #ungroup
    mutate(boot_num = rep(i, each = 5))
  
  write.csv(params_boot,paste0("../Results/SchoolField/Boots/Boot",i,".csv"))
}

Sys.time() - a

stopCluster(clus)
