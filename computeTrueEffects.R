# Load packages 

library(dplyr)

# Source generate data function

source('generateDecompData.R')

# Generate true values

set.seed(333)

# scenario 1: two continuous, not correlated, no interaction

true_s1 <- generateData(nSamp = 100000, u_coef_m1 = 0, u_coef_m2 = 0, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(F,F))

# scenario 2: two continuous, correlation ~ 0.3, no interaction

true_s2 <- generateData(nSamp = 100000, u_coef_m1 = 0.7, u_coef_m2 = 0.7, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(F,F))

# scenario 3: two continuous, correlation ~ 0.6, no interaction

true_s3 <- generateData(nSamp = 100000, u_coef_m1 = 1.25, u_coef_m2 = 1.25, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(F,F))

# scenario 4: two continuous, not correlated,interaction

true_s4 <- generateData(nSamp = 100000, u_coef_m1 = 0, u_coef_m2 = 0, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(F,F))

# scenario 5: two continuous, correlation ~ 0.3, interaction

true_s5 <- generateData(nSamp = 100000, u_coef_m1 = 0.7, u_coef_m2 = 0.7, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(F,F))

# scenario 6: two continuous, correlation ~ 0.6, interaction

true_s6 <- generateData(nSamp = 100000, u_coef_m1 = 1.25, u_coef_m2 = 1.25, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(F,F))

# scenario 7: one continuous, one binary, not correlated, no interaction

true_s7 <- generateData(nSamp = 100000, u_coef_m1 = 0, u_coef_m2 = 0, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,F))

# scenario 8: one continuous, one binary, correlation ~ 0.3, no interaction

true_s8 <- generateData(nSamp = 100000, u_coef_m1 = 0.7, u_coef_m2 = 0.7, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,F))

# scenario 9: one continuous, one binary, correlation ~ 0.6, no interaction

true_s9 <- generateData(nSamp = 100000, u_coef_m1 = 1.25, u_coef_m2 = 1.25, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,F))

# scenario 10: one continuous, one binary, not correlated, interaction

true_s10 <- generateData(nSamp = 100000, u_coef_m1 = 0, u_coef_m2 = 0, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,F))

# scenario 11: one continuous, one binary, correlation ~ 0.3, interaction

true_s11 <- generateData(nSamp = 100000, u_coef_m1 = 0.7, u_coef_m2 = 0.7, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,F))

# scenario 12: one continuous, one binary, correlation ~ 0.6, interaction

true_s12 <- generateData(nSamp = 100000, u_coef_m1 = 1.25, u_coef_m2 = 1.25, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,F))

# scenario 13: two binary, not correlated, no interaction

true_s13 <- generateData(nSamp = 100000, u_coef_m1 = 0, u_coef_m2 = 0, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,T))

# scenario 14: two binary, correlation ~ 0.3, no interaction

true_s14 <- generateData(nSamp = 100000, u_coef_m1 = 0.7, u_coef_m2 = 0.7, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,T))

# scenario 15: two binary, correlation ~ 0.6, no interaction

true_s15 <- generateData(nSamp = 100000, u_coef_m1 = 1.25, u_coef_m2 = 1.25, int_coef = 0, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,T))

# scenario 16: two binary, not correlated,interaction

true_s16 <- generateData(nSamp = 100000, u_coef_m1 = 0, u_coef_m2 = 0, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,T))

# scenario 17: two binary, correlation ~ 0.3, interaction

true_s17 <- generateData(nSamp = 100000, u_coef_m1 = 0.7, u_coef_m2 = 0.7, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,T))

# scenario 18: two binary, correlation ~ 0.6, interaction

true_s18 <- generateData(nSamp = 100000, u_coef_m1 = 1.25, u_coef_m2 = 1.25, int_coef = 0.5, 
                        computeTrueEffects = T, reps = 100, binary_m = c(T,T))

# compile all true effects and save to file

all_true <- rbind(true_s1, true_s2, true_s3, true_s4, 
                  true_s5, true_s6, true_s7, true_s8,
                  true_s9, true_s10, true_s11, true_s12, 
                  true_s13, true_s14, true_s15, true_s16, 
                  true_s17, true_s18)

write.csv(all_true, 'true_values.csv', row.names = F)

