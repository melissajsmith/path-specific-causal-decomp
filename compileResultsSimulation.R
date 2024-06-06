# Load packages 

library(dplyr)
library(ggplot2)
library(ggpubr)

# Read in true values

true_cor <- read.csv("true_values.csv", header = T)[,13]
true_vals_rr <- read.csv("true_values.csv", header = T)[,14:20]
true_vals_rd <- read.csv("true_values.csv", header = T)[,1:7]

# Create function to compute measures of interest

analyzeSimulation <- function(results, scenario = 1, contrast = 'RR'){
  
  if(contrast == 'RR'){
  
  true_vals <- true_vals_rr[scenario,]
  true_vals_rep <- do.call("rbind", replicate(nrow(results), true_vals, simplify = F))
  avg_est <- colMeans(results[,13:19])
  
  bias <- avg_est - true_vals
  percent_bias <- 100*(avg_est -true_vals) / true_vals
  rmse <- sqrt(colMeans((results[,13:19])- true_vals_rep)^2)
  avg_int_width <- c(mean(results[['natural_rr_upper']] - results[['natural_rr_lower']]),
                     mean(results[['count_00_rr_upper']] - results[['count_00_rr_lower']]),
                     mean(results[['count_01_rr_upper']] - results[['count_01_rr_lower']]),
                     mean(results[['count_10_rr_upper']] - results[['count_10_rr_lower']]), 
                     mean(results[['red_00_rr_upper']] - results[['red_00_rr_lower']]),
                     mean(results[['red_01_rr_upper']] - results[['red_01_rr_lower']]),
                     mean(results[['red_10_rr_upper']] - results[['red_10_rr_lower']])
                     )
  coverage <- c(colMeans(cbind(ifelse(c(true_vals[1]) > results[['natural_rr_lower']] & c(true_vals[1]) < results[['natural_rr_upper']], 1, 0),
                ifelse(c(true_vals[2]) > results[['count_00_rr_lower']] & c(true_vals[2]) < results[['count_00_rr_upper']], 1, 0),
                ifelse(c(true_vals[3]) > results[['count_01_rr_lower']] & c(true_vals[3]) < results[['count_01_rr_upper']], 1, 0),
                ifelse(c(true_vals[4]) > results[['count_10_rr_lower']] & c(true_vals[4]) < results[['count_10_rr_upper']], 1, 0),
                ifelse(c(true_vals[5]) > results[['red_00_rr_lower']] & c(true_vals[5]) < results[['red_00_rr_upper']], 1, 0),
                ifelse(c(true_vals[6]) > results[['red_01_rr_lower']] & c(true_vals[6]) < results[['red_01_rr_upper']], 1, 0),
                ifelse(c(true_vals[7]) > results[['red_10_rr_lower']] & c(true_vals[7]) < results[['red_10_rr_upper']], 1, 0))))
  } else{
    
    true_vals <- true_vals_rd[scenario,]
    true_vals_rep <- do.call("rbind", replicate(nrow(results), true_vals, simplify = F))
    avg_est <- colMeans(results[,c(1,2,4,3,5,7,6)])
    bias <- avg_est - true_vals
    percent_bias <- 100*(avg_est-true_vals) / true_vals
    rmse <- sqrt(colMeans((results[,c(1,2,4,3,5,7,6)] - true_vals_rep)^2))
    avg_int_width <- c(mean(results[['nat_diff_upper']] - results[['nat_diff_lower']]),
                       mean(results[['count_00_diff_upper']] - results[['count_00_diff_lower']]),
                       mean(results[['count_01_diff_upper']] - results[['count_01_diff_lower']]),
                       mean(results[['count_10_diff_upper']] - results[['count_10_diff_lower']]),
                       mean(results[['red_00_upper']] - results[['red_00_lower']]),
                       mean(results[['red_01_upper']] - results[['red_01_lower']]),
                       mean(results[['red_10_upper']] - results[['red_10_lower']]))
    coverage <- colMeans(cbind(ifelse(c(true_vals[1]) > results[['nat_diff_lower']] & c(true_vals[1]) < results[['nat_diff_upper']], 1, 0),
                               ifelse(c(true_vals[2]) > results[['count_00_diff_lower']] & c(true_vals[2]) < results[['count_00_diff_upper']], 1, 0),
                               ifelse(c(true_vals[3]) > results[['count_01_diff_lower']] & c(true_vals[3]) < results[['count_01_diff_upper']], 1, 0),
                               ifelse(c(true_vals[4]) > results[['count_10_diff_lower']] & c(true_vals[4]) < results[['count_10_diff_upper']], 1, 0),
                               ifelse(c(true_vals[5]) > results[['red_00_lower']] & c(true_vals[5]) < results[['red_00_upper']], 1, 0),
                               ifelse(c(true_vals[6]) > results[['red_01_lower']] & c(true_vals[6]) < results[['red_01_upper']], 1, 0),
                               ifelse(c(true_vals[7]) > results[['red_10_lower']] & c(true_vals[7]) < results[['red_10_upper']], 1, 0))
    )
    
    
  }
  to_return <- cbind(c(as.matrix(true_vals)), c(as.matrix(avg_est)), c(as.matrix(bias)), c(as.matrix(percent_bias)),
                     c(as.matrix(rmse)), c(as.matrix(avg_int_width)), c(as.matrix(coverage)))
  colnames(to_return) <- c('true_value', 'avg_est', 'bias', 'percent_bias', 'rmse', 'avg_int_width', 'coverage')
  rownames(to_return) <- names(true_vals)
  
  return(to_return)
                   
}


# Read in results

# scenario 1

scenario1_proposed <- read.csv('scenario1_sim1.csv')[1,]
scenario1_existing <- read.csv('scenario1_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario1_proposed <- rbind(scenario1_proposed, read.csv(paste0('scenario1_sim', i, '.csv'))[1,]))
  try(scenario1_existing <- rbind(scenario1_existing, read.csv(paste0('scenario1_sim', i, '.csv'))[2,]))
}


rr_p_1 <- analyzeSimulation(scenario1_proposed, scenario = 1, contrast = 'RR')
rr_e_1 <- analyzeSimulation(scenario1_existing, scenario = 1, contrast = 'RR')

rd_p_1 <- analyzeSimulation(scenario1_proposed, scenario = 1, contrast = 'RD')
rd_e_1 <- analyzeSimulation(scenario1_existing, scenario = 1, contrast = 'RD')

# scenario 2

scenario2_proposed <- read.csv('scenario2_sim1.csv')[1,]
scenario2_existing <- read.csv('scenario2_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario2_proposed <- rbind(scenario2_proposed, read.csv(paste0('scenario2_sim', i, '.csv'))[1,]))
  try(scenario2_existing <- rbind(scenario2_existing, read.csv(paste0('scenario2_sim', i, '.csv'))[2,]))
}

rr_p_2 <- analyzeSimulation(scenario2_proposed, scenario = 2, contrast = 'RR')
rr_e_2 <- analyzeSimulation(scenario2_existing, scenario = 2, contrast = 'RR')

rd_p_2 <- analyzeSimulation(scenario2_proposed, scenario = 2, contrast = 'RD')
rd_e_2 <- analyzeSimulation(scenario2_existing, scenario = 2, contrast = 'RD')


# Scenario 3

scenario3_proposed <- read.csv('scenario3_sim1.csv')[1,]
scenario3_existing <- read.csv('scenario3_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario3_proposed <- rbind(scenario3_proposed, read.csv(paste0('scenario3_sim', i, '.csv'))[1,]))
  try(scenario3_existing <- rbind(scenario3_existing, read.csv(paste0('scenario3_sim', i, '.csv'))[2,]))
}

rr_p_3 <- analyzeSimulation(scenario3_proposed, scenario = 3, contrast = 'RR')
rr_e_3 <- analyzeSimulation(scenario3_existing, scenario = 3, contrast = 'RR')

rd_p_3 <- analyzeSimulation(scenario3_proposed, scenario = 3, contrast = 'RD')
rd_e_3 <- analyzeSimulation(scenario3_existing, scenario = 3, contrast = 'RD')

# Scenario 4

scenario4_proposed <- read.csv('scenario4_sim1.csv')[1,]
scenario4_existing <- read.csv('scenario4_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario4_proposed <- rbind(scenario4_proposed, read.csv(paste0('scenario4_sim', i, '.csv'))[1,]))
  try(scenario4_existing <- rbind(scenario4_existing, read.csv(paste0('scenario4_sim', i, '.csv'))[2,]))
}

rr_p_4 <- analyzeSimulation(scenario4_proposed, scenario = 4, contrast = 'RR')
rr_e_4 <- analyzeSimulation(scenario4_existing, scenario = 4, contrast = 'RR')

rd_p_4 <- analyzeSimulation(scenario4_proposed, scenario = 4, contrast = 'RD')
rd_e_4 <- analyzeSimulation(scenario4_existing, scenario = 4, contrast = 'RD')

# Scenario 5

scenario5_proposed <- read.csv('scenario5_sim1.csv')[1,]
scenario5_existing <- read.csv('scenario5_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario5_proposed <- rbind(scenario5_proposed, read.csv(paste0('scenario5_sim', i, '.csv'))[1,]))
  try(scenario5_existing <- rbind(scenario5_existing, read.csv(paste0('scenario5_sim', i, '.csv'))[2,]))
}

rr_p_5 <- analyzeSimulation(scenario5_proposed, scenario = 5, contrast = 'RR')
rr_e_5 <- analyzeSimulation(scenario5_existing, scenario = 5, contrast = 'RR')

rd_p_5 <- analyzeSimulation(scenario5_proposed, scenario = 5, contrast = 'RD')
rd_e_5 <- analyzeSimulation(scenario5_existing, scenario = 5, contrast = 'RD')


# Scenario 6

scenario6_proposed <- read.csv('scenario6_sim1.csv')[1,]
scenario6_existing <- read.csv('scenario6_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario6_proposed <- rbind(scenario6_proposed, read.csv(paste0('scenario6_sim', i, '.csv'))[1,]))
  try(scenario6_existing <- rbind(scenario6_existing, read.csv(paste0('scenario6_sim', i, '.csv'))[2,]))
}

rr_p_6 <- analyzeSimulation(scenario6_proposed, scenario = 6, contrast = 'RR')
rr_e_6 <- analyzeSimulation(scenario6_existing, scenario = 6, contrast = 'RR')

rd_p_6 <- analyzeSimulation(scenario6_proposed, scenario = 6, contrast = 'RD')
rd_e_6 <- analyzeSimulation(scenario6_existing, scenario = 6, contrast = 'RD')

# Scenario 7

scenario7_proposed <- read.csv('scenario7_sim1.csv')[1,]
scenario7_existing <- read.csv('scenario7_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario7_proposed <- rbind(scenario7_proposed, read.csv(paste0('scenario7_sim', i, '.csv'))[1,]))
  try(scenario7_existing <- rbind(scenario7_existing, read.csv(paste0('scenario7_sim', i, '.csv'))[2,]))
}

rr_p_7 <- analyzeSimulation(scenario7_proposed, scenario = 7, contrast = 'RR')
rr_e_7 <- analyzeSimulation(scenario7_existing, scenario = 7, contrast = 'RR')

rd_p_7 <- analyzeSimulation(scenario7_proposed, scenario = 7, contrast = 'RD')
rd_e_7 <- analyzeSimulation(scenario7_existing, scenario = 7, contrast = 'RD')

# Scenario 8

scenario8_proposed <- read.csv('scenario8_sim1.csv')[1,]
scenario8_existing <- read.csv('scenario8_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario8_proposed <- rbind(scenario8_proposed, read.csv(paste0('scenario8_sim', i, '.csv'))[1,]))
  try(scenario8_existing <- rbind(scenario8_existing, read.csv(paste0('scenario8_sim', i, '.csv'))[2,]))
}

rr_p_8 <- analyzeSimulation(scenario8_proposed, scenario = 8, contrast = 'RR')
rr_e_8 <- analyzeSimulation(scenario8_existing, scenario = 8, contrast = 'RR')

rd_p_8 <- analyzeSimulation(scenario8_proposed, scenario = 8, contrast = 'RD')
rd_e_8 <- analyzeSimulation(scenario8_existing, scenario = 8, contrast = 'RD')

# Scenario 9

scenario9_proposed <- read.csv('scenario9_sim1.csv')[1,]
scenario9_existing <- read.csv('scenario9_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario9_proposed <- rbind(scenario9_proposed, read.csv(paste0('scenario9_sim', i, '.csv'))[1,]))
  try(scenario9_existing <- rbind(scenario9_existing, read.csv(paste0('scenario9_sim', i, '.csv'))[2,]))
}

rr_p_9 <- analyzeSimulation(scenario9_proposed, scenario = 9, contrast = 'RR')
rr_e_9 <- analyzeSimulation(scenario9_existing, scenario = 9, contrast = 'RR')

rd_p_9 <- analyzeSimulation(scenario9_proposed, scenario = 9, contrast = 'RD')
rd_e_9 <- analyzeSimulation(scenario9_existing, scenario = 9, contrast = 'RD')

# Scenario 10

scenario10_proposed <- read.csv('scenario10_sim1.csv')[1,]
scenario10_existing <- read.csv('scenario10_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario10_proposed <- rbind(scenario10_proposed, read.csv(paste0('scenario10_sim', i, '.csv'))[1,]))
  try(scenario10_existing <- rbind(scenario10_existing, read.csv(paste0('scenario10_sim', i, '.csv'))[2,]))
}

rr_p_10 <- analyzeSimulation(scenario10_proposed, scenario = 10, contrast = 'RR')
rr_e_10 <- analyzeSimulation(scenario10_existing, scenario = 10, contrast = 'RR')

rd_p_10 <- analyzeSimulation(scenario10_proposed, scenario = 10, contrast = 'RD')
rd_e_10 <- analyzeSimulation(scenario10_existing, scenario = 10, contrast = 'RD')

# Scenario 11 

scenario11_proposed <- read.csv('scenario11_sim1.csv')[1,]
scenario11_existing <- read.csv('scenario11_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario11_proposed <- rbind(scenario11_proposed, read.csv(paste0('scenario11_sim', i, '.csv'))[1,]))
  try(scenario11_existing <- rbind(scenario11_existing, read.csv(paste0('scenario11_sim', i, '.csv'))[2,]))
}

rr_p_11 <- analyzeSimulation(scenario11_proposed, scenario = 11, contrast = 'RR')
rr_e_11 <- analyzeSimulation(scenario11_existing, scenario = 11, contrast = 'RR')

rd_p_11 <- analyzeSimulation(scenario11_proposed, scenario = 11, contrast = 'RD')
rd_e_11 <- analyzeSimulation(scenario11_existing, scenario = 11, contrast = 'RD')

# Scenario 12

scenario12_proposed <- read.csv('scenario12_sim1.csv')[1,]
scenario12_existing <- read.csv('scenario12_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario12_proposed <- rbind(scenario12_proposed, read.csv(paste0('scenario12_sim', i, '.csv'))[1,]))
  try(scenario12_existing <- rbind(scenario12_existing, read.csv(paste0('scenario12_sim', i, '.csv'))[2,]))
}

rr_p_12 <- analyzeSimulation(scenario12_proposed, scenario = 12, contrast = 'RR')
rr_e_12 <- analyzeSimulation(scenario12_existing, scenario = 12, contrast = 'RR')

rd_p_12 <- analyzeSimulation(scenario12_proposed, scenario = 12, contrast = 'RD')
rd_e_12 <- analyzeSimulation(scenario12_existing, scenario = 12, contrast = 'RD')



# Scenario 13

scenario13_proposed <- read.csv('scenario13_sim1.csv')[1,]
scenario13_existing <- read.csv('scenario13_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario13_proposed <- rbind(scenario13_proposed, read.csv(paste0('scenario13_sim', i, '.csv'))[1,]))
  try(scenario13_existing <- rbind(scenario13_existing, read.csv(paste0('scenario13_sim', i, '.csv'))[2,]))
}

rr_p_13 <- analyzeSimulation(scenario13_proposed, scenario = 13, contrast = 'RR')
rr_e_13 <- analyzeSimulation(scenario13_existing, scenario = 13, contrast = 'RR')

rd_p_13 <- analyzeSimulation(scenario13_proposed, scenario = 13, contrast = 'RD')
rd_e_13 <- analyzeSimulation(scenario13_existing, scenario = 13, contrast = 'RD')

# Scenario 14

scenario14_proposed <- read.csv('scenario14_sim1.csv')[1,]
scenario14_existing <- read.csv('scenario14_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario14_proposed <- rbind(scenario14_proposed, read.csv(paste0('scenario14_sim', i, '.csv'))[1,]))
  try(scenario14_existing <- rbind(scenario14_existing, read.csv(paste0('scenario14_sim', i, '.csv'))[2,]))
}

rr_p_14 <- analyzeSimulation(scenario14_proposed, scenario = 14, contrast = 'RR')
rr_e_14 <- analyzeSimulation(scenario14_existing, scenario = 14, contrast = 'RR')

rd_p_14 <- analyzeSimulation(scenario14_proposed, scenario = 14, contrast = 'RD')
rd_e_14 <- analyzeSimulation(scenario14_existing, scenario = 14, contrast = 'RD')

# Scenario 15

scenario15_proposed <- read.csv('scenario15_sim1.csv')[1,]
scenario15_existing <- read.csv('scenario15_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario15_proposed <- rbind(scenario15_proposed, read.csv(paste0('scenario15_sim', i, '.csv'))[1,]))
  try(scenario15_existing <- rbind(scenario15_existing, read.csv(paste0('scenario15_sim', i, '.csv'))[2,]))
}

rr_p_15 <- analyzeSimulation(scenario15_proposed, scenario = 15, contrast = 'RR')
rr_e_15 <- analyzeSimulation(scenario15_existing, scenario = 15, contrast = 'RR')

rd_p_15 <- analyzeSimulation(scenario15_proposed, scenario = 15, contrast = 'RD')
rd_e_15 <- analyzeSimulation(scenario15_existing, scenario = 15, contrast = 'RD')

# Scenario 16

scenario16_proposed <- read.csv('scenario16_sim1.csv')[1,]
scenario16_existing <- read.csv('scenario16_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario16_proposed <- rbind(scenario16_proposed, read.csv(paste0('scenario16_sim', i, '.csv'))[1,]))
  try(scenario16_existing <- rbind(scenario16_existing, read.csv(paste0('scenario16_sim', i, '.csv'))[2,]))
}

rr_p_16 <- analyzeSimulation(scenario16_proposed, scenario = 16, contrast = 'RR')
rr_e_16 <- analyzeSimulation(scenario16_existing, scenario = 16, contrast = 'RR')

rd_p_16 <- analyzeSimulation(scenario16_proposed, scenario = 16, contrast = 'RD')
rd_e_16 <- analyzeSimulation(scenario16_existing, scenario = 16, contrast = 'RD')

# Scenario 17

scenario17_proposed <- read.csv('scenario17_sim1.csv')[1,]
scenario17_existing <- read.csv('scenario17_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario17_proposed <- rbind(scenario17_proposed, read.csv(paste0('scenario17_sim', i, '.csv'))[1,]))
  try(scenario17_existing <- rbind(scenario17_existing, read.csv(paste0('scenario17_sim', i, '.csv'))[2,]))
}

rr_p_17 <- analyzeSimulation(scenario17_proposed, scenario = 17, contrast = 'RR')
rr_e_17 <- analyzeSimulation(scenario17_existing, scenario = 17, contrast = 'RR')

rd_p_17 <- analyzeSimulation(scenario17_proposed, scenario = 17, contrast = 'RD')
rd_e_17 <- analyzeSimulation(scenario17_existing, scenario = 17, contrast = 'RD')

# Scenario 18

scenario18_proposed <- read.csv('scenario18_sim1.csv')[1,]
scenario18_existing <- read.csv('scenario18_sim1.csv')[2,]

for(i in 2:1000){
  try(scenario18_proposed <- rbind(scenario18_proposed, read.csv(paste0('scenario18_sim', i, '.csv'))[1,]))
  try(scenario18_existing <- rbind(scenario18_existing, read.csv(paste0('scenario18_sim', i, '.csv'))[2,]))
}

rr_p_18 <- analyzeSimulation(scenario18_proposed, scenario = 18, contrast = 'RR')
rr_e_18 <- analyzeSimulation(scenario18_existing, scenario = 18, contrast = 'RR')

rd_p_18 <- analyzeSimulation(scenario18_proposed, scenario = 18, contrast = 'RD')
rd_e_18 <- analyzeSimulation(scenario18_existing, scenario = 18, contrast = 'RD')

# Compile results:

# Scenarios 1-6 = two continuous
# Scenarios 7-12 = one continuous, one binary
# Scenarios 13-18 = two binary


# Two continuous, RR

tc_rr_df <- as.data.frame(rbind(rr_p_1, rr_p_2, rr_p_3, rr_p_4, rr_p_5, rr_p_6,
      rr_e_1, rr_e_2, rr_e_3, rr_e_4, rr_e_5, rr_e_6))
tc_rr_df$scenario <- rep(rep(c('r = 0\nNo int.', 'r = 0.3\nNo int.', 'r = 0.6\nNo int.',
                               'r = 0\nWith int.', 'r = 0.3\nWith int.', 'r = 0.6\nWith int.'), each = 7), 2)
tc_rr_df$Method <- rep(c('Proposed', 'Existing'), each = 7*6)
tc_rr_df$quantity <- rep(rownames(rr_p_1), 2*6)

# Two continuous, RD

tc_rd_df <- as.data.frame(rbind(rd_p_1, rd_p_2, rd_p_3, rd_p_4, rd_p_5, rd_p_6, 
                                rd_e_1, rd_e_2, rd_e_3, rd_e_4, rd_e_5, rd_e_6))
tc_rd_df$scenario <- rep(rep(c('r = 0\nNo int.', 'r = 0.3\nNo int.', 'r = 0.6\nNo int.',
                               'r = 0\nWith int.', 'r = 0.3\nWith int.', 'r = 0.6\nWith int.'), each = 7), 2)
tc_rd_df$Method <- rep(c('Proposed', 'Existing'), each = 7*6)
tc_rd_df$quantity <- rep(rownames(rd_p_1), 2*6)

# One continuous, one binary, RR

oc_rr_df <- as.data.frame(rbind(rr_p_7, rr_p_8, rr_p_9, rr_p_10, rr_p_11, rr_p_12, 
                                rr_e_7, rr_e_8, rr_e_9, rr_e_10, rr_e_11, rr_e_12))
oc_rr_df$scenario <- rep(rep(c('r = 0\nNo int.', 'r = 0.3\nNo int.', 'r = 0.6\nNo int.',
                               'r = 0\nWith int.', 'r = 0.3\nWith int.', 'r = 0.6\nWith int.'), each = 7), 2)
oc_rr_df$Method <- rep(c('Proposed', 'Existing'), each = 7*6)
oc_rr_df$quantity <- rep(rownames(rr_p_9), 2*6)

# One continuous, one binary, RD

oc_rd_df <- as.data.frame(rbind(rd_p_7, rd_p_8, rd_p_9, rd_p_10, rd_p_11, rd_p_12, 
                                rd_e_7, rd_e_8, rd_e_9, rd_e_10, rd_e_11, rd_e_12))
oc_rd_df$scenario <- rep(rep(c('r = 0\nNo int.', 'r = 0.3\nNo int.', 'r = 0.6\nNo int.',
                               'r = 0\nWith int.', 'r = 0.3\nWith int.', 'r = 0.6\nWith int.'), each = 7), 2)
oc_rd_df$Method <- rep(c('Proposed', 'Existing'), each = 7*6)
oc_rd_df$quantity <- rep(rownames(rd_p_9), 2*6)

# Two binary, RR

tb_rr_df <- as.data.frame(rbind(rr_p_13, rr_p_14, rr_p_15, rr_p_16, rr_p_17, rr_p_18,
                                rr_e_13, rr_e_14, rr_e_15, rr_e_16, rr_e_17, rr_e_18))
tb_rr_df$scenario <- rep(rep(c('r = 0\nNo int.', 'r = 0.3\nNo int.', 'r = 0.6\nNo int.',
                               'r = 0\nWith int.', 'r = 0.3\nWith int.', 'r = 0.6\nWith int.'), each = 7), 2)
tb_rr_df$Method <- rep(c('Proposed', 'Existing'), each = 7*6)
tb_rr_df$quantity <- rep(rownames(rr_p_17), 2*6)

# Two binary, RD

tb_rd_df <- as.data.frame(rbind(rd_p_13, rd_p_14, rd_p_15, rd_p_16, rd_p_17, rd_p_18,
                                rd_e_13, rd_e_14, rd_e_15, rd_e_16, rd_e_17, rd_e_18))
tb_rd_df$scenario <- rep(rep(c('r = 0\nNo int.', 'r = 0.3\nNo int.', 'r = 0.6\nNo int.',
                               'r = 0\nWith int.', 'r = 0.3\nWith int.', 'r = 0.6\nWith int.'), each = 7), 2)
tb_rd_df$Method <- rep(c('proposed', 'existing'), each = 7*6)
tb_rd_df$quantity <- rep(rownames(rd_p_17), 2*6)


# Make plots of different metrics

###################################################################

########## TC #############################################

# Plot bias for RR, TC

base_plot <- ggplot(data = subset(tc_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 20) +
  xlab('Scenario') +
  ylab('Percent bias') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.box.background = element_rect(color = "black"),
        legend.background = element_blank())


nat_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 10, linetype = 2)
  
count_00_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'count_00_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RR - joint intervention') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)

count_01_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'count_01_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[1])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)

count_10_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'count_10_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[2])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)

red_00_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'red_00_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 80) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RR - joint intervention') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)

red_01_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'red_01_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(0, 80) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[1])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)

red_10_tc_rr_bias <- ggplot(data = subset(tc_rr_df, quantity == 'red_10_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 80) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[2])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)

overall_legend <- get_legend(base_plot)

tc_rr_bias <- ggarrange(nat_tc_rr_bias,
                        count_00_tc_rr_bias, count_01_tc_rr_bias, count_10_tc_rr_bias,
                        red_00_tc_rr_bias, red_01_tc_rr_bias, red_10_tc_rr_bias, overall_legend,
                        nrow = 2, ncol = 4)  

tc_rr_bias <- annotate_figure(tc_rr_bias,
                              left = text_grob("Percent bias", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))

tc_rr_bias

ggsave('bias_tc_rr.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')
ggsave('bias_tc_rr.tiff', dpi = 600,
       width = 15, height = 8, bg = 'white')

# Plot average interval width for RR, TC

nat_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2.75) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.position = 'none')

count_00_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'count_00_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2.75) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RR - joint intervention') +
  theme(legend.position = 'none')

count_01_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'count_01_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2.75) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[1])) +
  theme(legend.position = 'none')

count_10_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'count_10_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2.75) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[2])) +
  theme(legend.position = 'none')

red_00_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'red_00_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RR - joint intervention') +
  theme(legend.position = 'none')

red_01_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'red_01_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[1])) +
  theme(legend.position = 'none')

red_10_tc_rr_width <- ggplot(data = subset(tc_rr_df, quantity == 'red_10_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[2])) +
  theme(legend.position = 'none')

overall_legend <- get_legend(base_plot)

tc_rr_width <- ggarrange(nat_tc_rr_width,
                        count_00_tc_rr_width, count_01_tc_rr_width, count_10_tc_rr_width,
                        red_00_tc_rr_width, red_01_tc_rr_width, red_10_tc_rr_width, overall_legend,
                        nrow = 2, ncol = 4)  


tc_rr_width <- annotate_figure(tc_rr_width,
                              left = text_grob("Average interval width", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))


tc_rr_width
ggsave('interval_width_tc_rr.tiff', dpi = 600,
       width = 15, height = 8, bg = 'white')
ggsave('interval_width_tc_rr.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')

# Plot bias for RD, TC

nat_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'natural_diff'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RD') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)

count_00_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'count_diff_00'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RD - joint intervention') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)+
  geom_hline(yintercept = -10, linetype = 2)

count_01_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'count_diff_01'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RD - intervention on M'[1])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)+
  geom_hline(yintercept = -10, linetype = 2)

count_10_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'count_diff_10'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RD - intervention on M'[2])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)+
  geom_hline(yintercept = -10, linetype = 2)

red_00_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'red_00'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RD - joint intervention') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)+
  geom_hline(yintercept = -10, linetype = 2)

red_01_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'red_01'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RD - intervention on M'[1])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)+
  geom_hline(yintercept = -10, linetype = 2)

red_10_tc_rd_bias <- ggplot(data = subset(tc_rd_df, quantity == 'red_10'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-20, 20) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RD - intervention on M'[2])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = 10, linetype = 2)+
  geom_hline(yintercept = -10, linetype = 2)

tc_rd_bias <- ggarrange(nat_tc_rd_bias,
                        count_00_tc_rd_bias, count_01_tc_rd_bias, count_10_tc_rd_bias,
                        red_00_tc_rd_bias, red_01_tc_rd_bias, red_10_tc_rd_bias, overall_legend,
                        nrow = 2, ncol = 4)  

tc_rd_bias <- annotate_figure(tc_rd_bias,
                              left = text_grob("Percent bias", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))

tc_rd_bias
ggsave('bias_tc_rd.tiff', dpi = 600,
       width = 15, height = 8, bg = 'white')
ggsave('bias_tc_rd.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')

########## OC #############################################

# Plot bias for RR, OC

nat_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 15) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

count_00_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'count_00_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 15) +   
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RR - joint intervention') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

count_01_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'count_01_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 15) +   
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[1])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

count_10_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'count_10_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 15) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[2])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

red_00_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'red_00_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 15) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RR - joint intervention') +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

red_01_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'red_01_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(-2.5, 15) +   
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[1])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

red_10_oc_rr_bias <- ggplot(data = subset(oc_rr_df, quantity == 'red_10_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 15) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[2])) +
  theme(legend.position = 'none')+
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

oc_rr_bias <- ggarrange(nat_oc_rr_bias,
                        count_00_oc_rr_bias, count_01_oc_rr_bias, count_10_oc_rr_bias,
                        red_00_oc_rr_bias, red_01_oc_rr_bias, red_10_oc_rr_bias, overall_legend,
                        nrow = 2, ncol = 4)  

oc_rr_bias <- annotate_figure(oc_rr_bias,
                              left = text_grob("Percent bias", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))

oc_rr_bias
ggsave('bias_oc_rr.tiff', dpi = 600,
       width = 15, height = 8, bg = 'white')
ggsave('bias_oc_rr.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')

# Plot average interval width for RR, OC

nat_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.position = 'none')

count_00_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'count_00_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RR - joint intervention') +
  theme(legend.position = 'none')

count_01_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'count_01_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[1])) +
  theme(legend.position = 'none')

count_10_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'count_10_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 2) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[2])) +
  theme(legend.position = 'none')

red_00_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'red_00_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RR - joint intervention') +
  theme(legend.position = 'none')

red_01_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'red_01_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(0, 1) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[1])) +
  theme(legend.position = 'none')

red_10_oc_rr_width <- ggplot(data = subset(oc_rr_df, quantity == 'red_10_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[2])) +
  theme(legend.position = 'none')

overall_legend <- get_legend(base_plot)

oc_rr_width <- ggarrange(nat_oc_rr_width,
                         count_00_oc_rr_width, count_01_oc_rr_width, count_10_oc_rr_width,
                         red_00_oc_rr_width, red_01_oc_rr_width, red_10_oc_rr_width, overall_legend,
                         nrow = 2, ncol = 4)  

oc_rr_width <- annotate_figure(oc_rr_width,
                               left = text_grob("Average interval width", rot = 90, size = 16),
                               bottom = text_grob("Simulation scenario", size = 16))

oc_rr_width
ggsave('interval_width_oc_rr.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')


# Plot bias for RD, oc

nat_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'natural_diff'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RD') +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

count_00_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'count_diff_00'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RD - joint intervention') +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

count_01_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'count_diff_01'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RD - intervention on M'[1])) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

count_10_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'count_diff_10'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RD - intervention on M'[2])) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

red_00_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'red_00'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RD - joint intervention') +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

red_01_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'red_01'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RD - intervention on M'[1])) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

red_10_oc_rd_bias <- ggplot(data = subset(oc_rd_df, quantity == 'red_10'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-15, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RD - intervention on M'[2])) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = -10, linetype = 2) +
  geom_hline(yintercept = 10, linetype = 2)

oc_rd_bias <- ggarrange(nat_oc_rd_bias,
                        count_00_oc_rd_bias, count_01_oc_rd_bias, count_10_oc_rd_bias,
                        red_00_oc_rd_bias, red_01_oc_rd_bias, red_10_oc_rd_bias, overall_legend,
                        nrow = 2, ncol = 4)  

oc_rd_bias <- annotate_figure(oc_rd_bias,
                              left = text_grob("Percent bias", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))

oc_rd_bias
ggsave('bias_oc_rd.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')

########## TB #############################################

# Plot bias for RR, TB


nat_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 10, linetype = 2)

count_00_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'count_00_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RR - joint intervention') +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2)

count_01_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'count_01_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[1])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2)

count_10_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'count_10_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[2])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2)

red_00_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'red_00_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RR - joint intervention') +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2)

red_01_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'red_01_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[1])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2)

red_10_tb_rr_bias <- ggplot(data = subset(tb_rr_df, quantity == 'red_10_rr'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-2.5, 10) +  
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[2])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2)

tb_rr_bias <- ggarrange(nat_tb_rr_bias,
                        count_00_tb_rr_bias, count_01_tb_rr_bias, count_10_tb_rr_bias,
                        red_00_tb_rr_bias, red_01_tb_rr_bias, red_10_tb_rr_bias, overall_legend,
                        nrow = 2, ncol = 4)  

tb_rr_bias <- annotate_figure(tb_rr_bias,
                              left = text_grob("Percent bias", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))

tb_rr_bias
ggsave('bias_tb_rr.tiff', dpi = 600,
       width = 15, height = 8, bg = 'white')
ggsave('bias_tb_rr.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')

# Plot average interval width for RR, TB

nat_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'natural_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RR') +
  theme(legend.position = 'none')

count_00_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'count_00_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RR - joint intervention') +
  theme(legend.position = 'none')

count_01_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'count_01_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[1])) +
  theme(legend.position = 'none')

count_10_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'count_10_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 1.25) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RR - intervention on M'[2])) +
  theme(legend.position = 'none')

red_00_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'red_00_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 0.5) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RR - joint intervention') +
  theme(legend.position = 'none')

red_01_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'red_01_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(0, 0.5) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[1])) +
  theme(legend.position = 'none')

red_10_tb_rr_width <- ggplot(data = subset(tb_rr_df, quantity == 'red_10_rr'), aes(x = scenario, y = avg_int_width, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(0, 0.5) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RR - intervention on M'[2])) +
  theme(legend.position = 'none')

overall_legend <- get_legend(base_plot)

tb_rr_width <- ggarrange(nat_tb_rr_width,
                         count_00_tb_rr_width, count_01_tb_rr_width, count_10_tb_rr_width,
                         red_00_tb_rr_width, red_01_tb_rr_width, red_10_tb_rr_width, overall_legend,
                         nrow = 2, ncol = 4)  

tb_rr_width <- annotate_figure(tb_rr_width,
                               left = text_grob("Average interval width", rot = 90, size = 16),
                               bottom = text_grob("Simulation scenario", size = 16))

tb_rr_width
ggsave('interval_width_tb_rr.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')


# Plot bias for RD, tb

nat_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'natural_diff'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Natural-course RD') +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

count_00_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'count_diff_00'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Counterfactual RD - joint intervention') +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

count_01_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'count_diff_01'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RD - intervention on M'[1])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

count_10_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'count_diff_10'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle(expression('Counterfactual RD - intervention on M'[2])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

red_00_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'red_00'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) + 
  ggtitle('Reduction in RD - joint intervention') +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

red_01_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'red_01'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RD - intervention on M'[1])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

red_10_tb_rd_bias <- ggplot(data = subset(tb_rd_df, quantity == 'red_10'), aes(x = scenario, y = percent_bias, fill = Method)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ylim(-10, 10) +
  xlab('') +
  ylab('') +
  scale_fill_manual(values = c('tomato1', 'blueviolet')) +
  ggtitle(expression('Reduction in RD - intervention on M'[2])) +
  theme(legend.position = 'none')  +
  geom_hline(yintercept = 10, linetype = 2) +
  geom_hline(yintercept = -10, linetype = 2)  

tb_rd_bias <- ggarrange(nat_tb_rd_bias,
                        count_00_tb_rd_bias, count_01_tb_rd_bias, count_10_tb_rd_bias,
                        red_00_tb_rd_bias, red_01_tb_rd_bias, red_10_tb_rd_bias, overall_legend,
                        nrow = 2, ncol = 4)  

tb_rd_bias <- annotate_figure(tb_rd_bias,
                              left = text_grob("Percent bias", rot = 90, size = 16),
                              bottom = text_grob("Simulation scenario", size = 16))

tb_rd_bias
ggsave('bias_tb_rd.jpg', dpi = 600,
       width = 15, height = 8, bg = 'white')

# Make three tables with other metrics on RR scale: true estimate, percent bias, average interval width, coverage

tc_rr_df %>% filter(quantity == 'natural_rr' & Method == 'Proposed')
oc_rr_df %>% filter(quantity == 'natural_rr' & Method == 'Proposed')
tb_rr_df %>% filter(quantity == 'natural_rr' & Method == 'Proposed')

tc_rr_df %>% filter(quantity == 'count_00_rr' & Method == 'Proposed')
oc_rr_df %>% filter(quantity == 'count_00_rr' & Method == 'Proposed')
tb_rr_df %>% filter(quantity == 'count_00_rr' & Method == 'Proposed')

tc_rr_df %>% filter(quantity == 'red_00_rr' & Method == 'Existing')
tc_rr_df %>% filter(quantity == 'red_01_rr' & Method == 'Existing')
tc_rr_df %>% filter(quantity == 'red_10_rr' & Method == 'Existing')

tc_rr_df %>% filter(quantity == 'red_00_rr' & Method == 'Proposed')
tc_rr_df %>% filter(quantity == 'red_01_rr' & Method == 'Proposed')
tc_rr_df %>% filter(quantity == 'red_10_rr' & Method == 'Proposed')

tb_rr_df %>% filter(quantity == 'red_00_rr' & Method == 'Existing')
tb_rr_df %>% filter(quantity == 'red_01_rr' & Method == 'Existing')
tb_rr_df %>% filter(quantity == 'red_10_rr' & Method == 'Existing')

tb_rr_df %>% filter(quantity == 'red_00_rr' & Method == 'Proposed')
tb_rr_df %>% filter(quantity == 'red_01_rr' & Method == 'Proposed')
tb_rr_df %>% filter(quantity == 'red_10_rr' & Method == 'Proposed')



