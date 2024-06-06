# Load packages 

library(dplyr)

# Load in relevant functions

source('generateDecompData.R')
source('runProposedMethod.R')
source('runExistingMethod.R')


runFullSim <- function(scenario = 1, nSamp = 500, nPred = 1000, nBoot = 200){
  
  # nPred denotes the number of mediator and outcome predictions drawn for each
  # person in the study. These are used to reduce MC standard error
  # nBoot denotes the number of bootstrap samples used to obtain confidence intervals
  
  if(scenario == 1){
    u_coef_m1 = 0; u_coef_m2 = 0; int_coef = 0; binary_m = c(F,F)
  }
  if(scenario == 2){
    u_coef_m1 = 0.7; u_coef_m2 = 0.7; int_coef = 0; binary_m = c(F,F)
  }
  if(scenario == 3){
    u_coef_m1 = 1.25; u_coef_m2 = 1.25; int_coef = 0; binary_m = c(F,F)
  }
  if(scenario == 4){
    u_coef_m1 = 0; u_coef_m2 = 0; int_coef = 0.5; binary_m = c(F,F)
  }
  if(scenario == 5){
    u_coef_m1 = 0.7; u_coef_m2 = 0.7; int_coef = 0.5; binary_m = c(F,F)
  }
  if(scenario == 6){
    u_coef_m1 = 1.25; u_coef_m2 = 1.25; int_coef = 0.5; binary_m = c(F,F)
  }
  if(scenario == 7){
    u_coef_m1 = 0; u_coef_m2 = 0; int_coef = 0; binary_m = c(T,F)
  }
  if(scenario == 8){
    u_coef_m1 = 0.7; u_coef_m2 = 0.7; int_coef = 0; binary_m = c(T,F)
  }
  if(scenario == 9){
    u_coef_m1 = 1.25; u_coef_m2 = 1.25; int_coef = 0; binary_m = c(T,F)
  }
  if(scenario == 10){
    u_coef_m1 = 0; u_coef_m2 = 0; int_coef = 0.5; binary_m = c(T,F)
  }
  if(scenario == 11){
    u_coef_m1 = 0.7; u_coef_m2 = 0.7; int_coef = 0.5; binary_m = c(T,F)
  }
  if(scenario == 12){
    u_coef_m1 = 1.25; u_coef_m2 = 1.25; int_coef = 0.5; binary_m = c(T,F)
  }
  if(scenario == 13){
    u_coef_m1 = 0; u_coef_m2 = 0; int_coef = 0; binary_m = c(T,T)
  }
  if(scenario == 14){
    u_coef_m1 = 0.7; u_coef_m2 = 0.7; int_coef = 0; binary_m = c(T,T)
  }
  if(scenario == 15){
    u_coef_m1 = 1.25; u_coef_m2 = 1.25; int_coef = 0; binary_m = c(T,T)
  }
  if(scenario == 16){
    u_coef_m1 = 0; u_coef_m2 = 0; int_coef = 0.5; binary_m = c(T,T)
  }
  if(scenario == 17){
    u_coef_m1 = 0.7; u_coef_m2 = 0.7; int_coef = 0.5; binary_m = c(T,T)
  }
  if(scenario == 18){
    u_coef_m1 = 1.25; u_coef_m2 = 1.25; int_coef = 0.5; binary_m = c(T,T)
  }
  
  
  data <- generateData(nSamp = nSamp, u_coef_m1 = u_coef_m1, u_coef_m2 = u_coef_m2, int_coef = int_coef,
                       binary_m = binary_m, 
                       computeTrueEffects = F)
  
  interaction <- ifelse(int_coef == 0, F, T)
  
  full_results_proposed <- runProposed(data = data, reps = nPred, int = interaction, binary_m = binary_m)
  full_results_existing <- runExisting(data = data, reps = nPred, int = interaction, binary_m = binary_m)
  
  boot_results_proposed <- matrix(NA, nrow = nBoot, ncol = length(full_results_proposed))
  boot_results_existing <- matrix(NA, nrow = nBoot, ncol = length(full_results_proposed))
  
  for(i in 1:nBoot){
    
    idx <- sample.int(nrow(data), replace = T)
    boot <- data[idx,]
    boot_results_proposed[i,] <- runProposed(data = boot, reps = nPred, int = interaction, binary_m = binary_m)
    boot_results_existing[i,] <- runExisting(data = boot, reps = nPred, int = interaction, binary_m = binary_m)
    
  }
  
  lower_proposed <- apply(boot_results_proposed, 2, function(x) quantile(x, probs = 0.025))
  upper_proposed <- apply(boot_results_proposed, 2, function(x) quantile(x, probs = 0.975))
  
  lower_existing <- apply(boot_results_existing, 2, function(x) quantile(x, probs = 0.025))
  upper_existing <- apply(boot_results_existing, 2, function(x) quantile(x, probs = 0.975))
  
  names(lower_proposed) <- names(lower_existing) <- paste0(names(full_results_proposed), '_lower')
  names(upper_proposed) <- names(upper_existing) <- paste0(names(full_results_proposed), '_upper')
  
  to_return <- rbind(c(full_results_proposed, lower_proposed, upper_proposed), 
                     c(full_results_existing, lower_existing, upper_existing))
  rownames(to_return) <- c('Proposed method', 'Existing method')
  
  return(to_return)
  
}

array_num <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
scenario <- as.numeric(commandArgs(T)[1])

set.seed(scenario*10000 + array_num)

out <- runFullSim(scenario = scenario, nSamp = 500, nPred = 500, nBoot = 200)
write.csv(out, paste0('scenario', scenario, '_sim', array_num, '.csv'), row.names = F)
