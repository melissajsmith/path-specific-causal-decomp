
library(mvtnorm)
library(dplyr)

# Function to simulate dataset with 2 mediators

generateData <- function(nSamp,
                         u_coef_m1 = 0, # controls residual correlation -- no residual correlation by default
                         u_coef_m2 = 0, # controls residual correlation -- no residual correlation by default
                         int_coef = 0, # controls mediator-mediator interaction between mediators 1 and 2
                         resid_var_cont = 1, # controls the variance of the model error terms for continuous mediators
                         binary_m = c(F, F), # options: binary_m = c(F,F), c(T,F), c(T,T)
                         computeTrueEffects = F,
                         reps = NULL){
  
  # Set coefficient values for regression models
  gamma0 <- 0 # intercept, l model
  gamma1 <- 2 # coefficient for a, l model
  
  eta0 <- 0 # intercept, a model
  eta1 <- 2 # coefficient for u2, a model
  
  alpha0_m1 <- -1 # intercept, m1 model
  alpha1_m1 <- 0.5 # coefficient for a, m1 model
  alpha2_m1 <- 0.5 # coefficient for c, m1 model
  
  alpha0_m2 <- -1 # intercept, m2 model
  alpha1_m2 <- 0.8 # coefficient for a, m2 model
  alpha2_m2 <- 0.5 # coefficient for c, m2 model

  beta0 <- -2  # intercept, outcome model
  beta1 <- 0.5 # coefficient for u2, outcome model
  beta2 <- 0.5 # coefficient for m1, outcome model
  beta3 <- 0.5 # coefficient for m2, outcome model
  beta4 <- 0.5 # coefficient for c, outcome model
  
  # Confounders
  c <- rbinom(nSamp, size = 1, prob = 0.5)
  
  # Unmeasured confounder that induces correlation between exposure and outcome
  u2 <- rbinom(nSamp, size = 1, prob = 0.5)

  # Exposure
  lp_a <- eta0 + eta1*u2
  a <- rbinom(nSamp, size = 1, prob = exp(lp_a) / (1 + exp(lp_a)))
  
  # Unmeasured confounder that induces correlation in mediators
  u <- rnorm(nSamp, sd = 1)
  
  # Generate mediator data from a model with covariance matrix sigma
  lp <- matrix(NA, nrow = nSamp, ncol = 2)
  m <- matrix(NA, nrow = nSamp, ncol = 2)
  
  lp[,1] <- alpha0_m1 + alpha1_m1*a + alpha2_m1*c + u_coef_m1*u
  lp[,2] <- alpha0_m2 + alpha1_m2*a + alpha2_m2*c + u_coef_m2*u 
  
  
  if(sum(binary_m == c(T, F)) == 2){
    
    eps <- rmvnorm(nSamp, mean = c(0,0), sigma = matrix(c(1, 0, 0, resid_var_cont), nrow = 2, byrow = T))
    
    m[,1] <- ifelse(lp[,1] + eps[,1] > 0, 1, 0)
    m[,2] <- lp[,2] + eps[,2]
    
  } else if(sum(binary_m == c(T,T)) == 2){
    
    eps <- rmvnorm(nSamp, mean = c(0,0), sigma = diag(2))
    
    m[,1] <- ifelse(lp[,1] + eps[,1] > 0, 1, 0)
    m[,2] <- ifelse(lp[,2] + eps[,2] > 0, 1, 0)
  } else{
    
    eps <- rmvnorm(nSamp, mean = c(0,0), sigma = resid_var_cont*diag(2))
    
    m[,1] <- lp[,1] + eps[,1]
    m[,2] <- lp[,2] + eps[,2]
    
  }
  
  residual_cor <- cor(u_coef_m1*u + eps[,1], u_coef_m2*u + eps[,2])
  
  # Generate outcome
  
  lpOut <- beta0 + beta1*u2 + beta2*m[,1] + beta3*m[,2] + int_coef*m[,1]*m[,2] + beta4*c
  y <- rbinom(nSamp, size = 1, prob = exp(lpOut) / (1 + exp(lpOut)))

  to_return <- data.frame(y = y, a = a, m1 = m[,1], m2 = m[,2], c = c)
  
  # Compute true effects using numerical integration
  
  if(computeTrueEffects == T){
    
    # Quantities of interest:
    
    # E(Y(1, M ~ f(1,1))) - quantity 1
    # E(Y(1, M ~ f(0,0))) - quantity 2
    # E(Y(1, M ~ f(0,1))) - quantity 3
    # E(Y(1, M ~ f(1,0))) - quantity 4
    # E(Y(0, M ~ f(0,0))) - quantity 5
    
    integrand <- function(quantity, binary_m = binary_m){
      # binary_m = F if m1 is continuous and true if m1 is discrete
      if(quantity == 1){
        a = 1; a1 = 1; a2 = 1
      } else if(quantity == 2){
        a = 1; a1 = 0; a2 = 0
      } else if(quantity == 3){
        a = 1; a1 = 0; a2 = 1
      } else if(quantity == 4){
        a = 1; a1 = 1; a2 = 0
      } else if(quantity == 5){
        a = 0; a1 = 0; a2 = 0
      }
      
      # Generate from p(c)
      c <- rbinom(nSamp, size = 1, prob = 0.5)
      
      # Unmeasured confounder that induces correlation between exposure and outcome
      u2_orig <- rbinom(nSamp, size = 1, prob = 0.5)
      
      # Exposure
      lp_a <- eta0 + eta1*u2_orig
      a_orig <- rbinom(nSamp, size = 1, prob = exp(lp_a) / (1 + exp(lp_a)))
      
      u2_a_df <- data.frame(u2_orig = u2_orig, a_orig = a_orig)
      
      # Generate from p(u2 | a)
      probs <- aggregate(u2_orig ~ a_orig, u2_a_df, mean)
      if(a == 0){
        u2 <- rbinom(nSamp, size = 1, prob = probs[1,2])
      }
      if(a == 1){
        u2 <- rbinom(nSamp, size = 1, prob = probs[2,2])
      }
      
      # Generate mediator data from a model with covariance matrix sigma
      lp <- matrix(NA, nrow = nSamp, ncol = 2)
      m <- matrix(NA, nrow = nSamp, ncol = 2)
      
      # Generate from p(m1, m2 | a1, a2, c, u)
      lp[,1] <- alpha0_m1 + alpha1_m1*a1 + alpha2_m1*c + u_coef_m1*u
      lp[,2] <- alpha0_m2 + alpha1_m2*a2 + alpha2_m2*c + u_coef_m2*u
      
      
      if(sum(binary_m == c(T, F)) == 2){
        
        eps <- rmvnorm(nSamp, mean = c(0,0), sigma = matrix(c(1, 0, 0, resid_var_cont), nrow = 2, byrow = T))
        
        m[,1] <- ifelse(lp[,1] + eps[,1] > 0, 1, 0)
        m[,2] <- lp[,2] + eps[,2]
        
      } else if(sum(binary_m == c(T,T)) == 2){
        eps <- rmvnorm(nSamp, mean = c(0,0), sigma = diag(2))
        
        m[,1] <- ifelse(lp[,1] + eps[,1] > 0, 1, 0)
        m[,2] <- ifelse(lp[,2] + eps[,2] > 0, 1, 0)
      } else{
        
        eps <- rmvnorm(nSamp, mean = c(0,0), sigma = resid_var_cont*diag(2))
        
        m[,1] <- lp[,1] + eps[,1]
        m[,2] <- lp[,2] + eps[,2]
      }
      
      # Compute E(y | u2, m1, m2, c)
      
      lpOut <- beta0 + beta1*u2 + beta2*m[,1] + beta3*m[,2] + int_coef*m[,1]*m[,2] + beta4*c
      ey <- mean(exp(lpOut) / (1 + exp(lpOut)))
      
      return(ey)
      
    }
    
    ey_111 <- rep(NA, reps)
    ey_100 <- rep(NA, reps)
    ey_101 <- rep(NA, reps)
    ey_110 <- rep(NA, reps)
    ey_000 <- rep(NA, reps)
    
    for(i in 1:reps){
      
      ey_111[i] <- integrand(quantity = 1, binary_m = binary_m)
      ey_100[i] <- integrand(quantity = 2, binary_m = binary_m)
      ey_101[i] <- integrand(quantity = 3, binary_m = binary_m)
      ey_110[i] <- integrand(quantity = 4, binary_m = binary_m)
      ey_000[i] <- integrand(quantity = 5, binary_m = binary_m)
      
    }
    
    ey_111 <- mean(ey_111)
    ey_100 <- mean(ey_100)
    ey_101 <- mean(ey_101)
    ey_110 <- mean(ey_110)
    ey_000 <- mean(ey_000)

    
    natural_diff <- ey_111 - ey_000
    count_00_diff <- ey_100 - ey_000
    count_01_diff <- ey_101 - ey_000
    count_10_diff <- ey_110 - ey_000
    red_00 <- natural_diff - count_00_diff
    red_01 <- natural_diff - count_01_diff
    red_10 <- natural_diff - count_10_diff
    
    natural_rr <- ey_111 / ey_000
    count_00_rr <- ey_100 / ey_000
    count_01_rr <- ey_101 / ey_000
    count_10_rr <- ey_110 / ey_000
    red_00_rr <- natural_rr - count_00_rr
    red_01_rr <- natural_rr - count_01_rr
    red_10_rr <- natural_rr - count_10_rr
    
    to_return <- c(natural_diff, count_00_diff, count_01_diff, count_10_diff, red_00, red_01, red_10, 
                   ey_111, ey_100, ey_101, ey_110, ey_000, residual_cor, 
                   natural_rr, count_00_rr, count_01_rr, count_10_rr,
                   red_00_rr, red_01_rr, red_10_rr)
    
    names(to_return) <- c('natural_diff', 'count_diff_00', 'count_diff_01', 'count_diff_10',
                          'red_00', 'red_01', 'red_10', 'ey_111', 'ey_100', 'ey_101', 'ey_110', 'ey_000', 'residual_cor',
                          'natural_rr', 'count_00_rr', 'count_01_rr', 'count_10_rr', 
                          'red_00_rr', 'red_01_rr', 'red_10_rr')
    
  }
    
  
  return(to_return)

  
}
