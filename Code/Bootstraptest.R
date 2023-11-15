library(tidyverse)
library(broom)

#totally made up data for a reproducible example
data_test=data.frame(test_temperature=rep(seq(5, 30, 2), 3), 
                     growth_rate=c(1, 2, 4, 6, 12, 18, 29, 33, 37, 35, 22, 14, 2,
                                   2, 3, 2, 7, 13, 21, 31, 35, 42, 42, 25, 13, 0,
                                   1, 1, 3, 5, 11, 15, 22, 31, 40, 45, 18, 5, 1))



fit<-nls_multstart(growth_rate ~ ((a*exp(b*test_temperature)*(1-((test_temperature-z)/(w/2))^2))),
                   data= data_test,  iter = 6000,
                   start_lower = c(a = 0.1, b=0.0001, z=5, w=5), #set lower bound of starting points in paramaters
                   start_upper = c(a = 0.6, b=0.2, z=40, w=40), #set upper bound of starting points in paramaters
                   supp_errors = 'Y',
                   convergence_count = 2000,
                   na.action = na.omit,
                   lower=c(a = 0, b=0, z=-2000, w=0)) #set lower bound of parameter values acceptable

# get parameters ####                   
params_fit<-tidy(fit) # tidy version of model outputs
summary_fit<-glance(fit) # summary of model fit
preds_fit<- augment(fit) #predicted values for plotting


#bootstrap this ######
nboot=20 #set number of boots (make this higher after troubleshooting)

fit_boots <- data_test %>% 
  modelr::bootstrap(n = nboot, id = 'boot_num') %>% #bootstrap
  group_by(boot_num) %>%
  mutate(fit = map(strap, ~nls_multstart(growth_rate 
                                         ~ (a*exp(b*test_temperature)*(1-((test_temperature-z)/(w/2))^2)) ,
                                         data= data.frame(.),  iter = 6000, 
                                         start_lower = c(a = 0.1, b=0.0001, z=5, w=5),
                                         start_upper = c(a = 0.6, b=0.2, z=40, w=40),
                                         supp_errors = 'Y',
                                         convergence_count = 2000,
                                         na.action = na.omit,
                                         lower=c(a = 0, b=0, z=-2000, w=0))
  ))

# get parameters ####
params_boot <- map(fit_boots$fit, tidy) %>% #tidy up parameters from each model
  map_df(as_tibble) %>% #make this into a dataframe
  ungroup() %>% #ungroup
  mutate(boot_num = rep(c(1:nboot), each = 4)) #clunky: add boot number (beware this is low replicability)


library(ggplot2)
temperature <- seq(5, 30, 2)
curve <- ((params_fit$estimate[1]*exp(params_fit$estimate[2]*temperature)*(1-((temperature-params_fit$estimate[3])/(params_fit$estimate[4]/2))^2)))
Model1 <- data.frame(temperature, curve)

BootModels <- c()
for(i in 1:nboot){
  params <- subset(params_boot, params_boot$boot_num == i)
  a <- params[params$term == "a",]$estimate
  b <- params[params$term == "b",]$estimate
  z <- params[params$term == "z",]$estimate
  w <- params[params$term == "w",]$estimate
  curve <- ((a*exp(b*temperature)*(1-((temperature-z)/(w/2))^2)))
  bootN <- rep(i,length(curve))
  output <- cbind(curve,bootN,temperature)
  #print(output)
  
  BootModels <- rbind(output,BootModels)
}


BootModels <- as.data.frame(BootModels)


p <- ggplot() + geom_point(data = preds_fit, aes(x=test_temperature, y=growth_rate), colour = "blue", size = I(1), alpha = 0.4) +
                geom_line(data = BootModels, aes(x=temperature, y=curve, group=bootN), colour = "grey", size = I(1), alpha = 0.4) +
                             geom_line(data = Model1, aes(x=temperature,y=curve), colour = "red", size = I(1), alpha = 1)
              
p                           
                           