

runExisting <- function(data, reps = 1000, int = F, binary_m = c(F,F)){
  
  # Fit mediator models with observed data
  
  if(sum(binary_m == c(F,F)) == 2){
    family1 = 'gaussian'
    family2 = 'gaussian'
  }
  if(sum(binary_m == c(T,F)) == 2){
    family1 = binomial(link = "probit")
    family2 = 'gaussian'
  }
  if(sum(binary_m == c(T,T)) == 2){
    family1 = binomial(link = "probit")
    family2 = binomial(link = "probit")
  }
  
  data$a1 <- data$a
  data$a2 <- data$a
  
  med1_model <- glm(m1 ~ a1 + c, data = data, family = family1)
  med2_model <- glm(m2 ~ a2 + c, data = data, family = family2)
  
  # Fit outcome model with observed data
  
  if(int == T){ # if there is a mediator-mediator interaction
    
    out_model <- glm(y ~ a + m1 + m2 + m1*m2 + c, family = binomial, data = data)
    
  } else{
    
    out_model <- glm(y ~ a + m1 + m2 + c, family = binomial, data = data)
    
  }
  
  
  # Divide dataset by group
  
  data_0 <- filter(data, a == 0)
  data_1 <- filter(data, a == 1)
  
  data_1_idx <- which(data$a == 1)
  
  # Obtain natural-course predictions from the mediator model
  
  nat0_vec <- rep(NA, reps)
  nat1_vec <- rep(NA, reps)
  
  m1_nat <- matrix(NA, nrow = nrow(data_1), ncol = reps)
  m2_nat <- matrix(NA, nrow = nrow(data_1), ncol = reps)
  
  for(i in 1:reps){
    
    if(sum(binary_m == c(F,F)) == 2){
      
      sigma1 <- sqrt(summary(med1_model)$dispersion)
      sigma2 <- sqrt(summary(med2_model)$dispersion)
      
      # Generate error terms for each group's natural outcomes
      
      eps_nat1 <- rnorm(nrow(data), mean = 0, sd = sigma1)
      eps_nat2 <- rnorm(nrow(data), mean = 0, sd = sigma2)
      
      m1_pred <- predict(med1_model) + eps_nat1
      m2_pred <- predict(med2_model) + eps_nat2 
      
    } else if(sum(binary_m == c(T,F)) == 2){
      
      sigma1 <- 1
      sigma2 <- sqrt(summary(med2_model)$dispersion)
      
      # Generate error terms for each group's natural outcomes
      
      eps_nat1 <- rnorm(nrow(data), mean = 0, sd = sigma1)
      eps_nat2 <- rnorm(nrow(data), mean = 0, sd = sigma2)
      
      m1_pred <- ifelse(predict(med1_model) + eps_nat1 > 0, 1, 0)
      m2_pred <- predict(med2_model) + eps_nat2
      
    } else{
      
      sigma1 <- 1
      sigma2 <- 1
      
      # Generate error terms for each group's natural outcomes
      
      eps_nat1 <- rnorm(nrow(data), mean = 0, sd = sigma1)
      eps_nat2 <- rnorm(nrow(data), mean = 0, sd = sigma2)
      
      m1_pred <- ifelse(predict(med1_model) + eps_nat1 > 0, 1, 0)
      m2_pred <- ifelse(predict(med2_model) + eps_nat2 > 0, 1, 0)
      
    }
    
    predicted_data <- data.frame(y = NA, a = data$a, m1 = m1_pred, m2 = m2_pred, c = data$c)
    
    nat_prob <- predict(out_model, newdata = predicted_data, type = 'response')
    
    y_pred <- rbinom(nrow(data), 1, prob = nat_prob)
    
    predicted_data$y <- y_pred
    
    nat0_vec[i] <- aggregate(y ~ a, predicted_data, mean)[1,2]
    nat1_vec[i] <- aggregate(y ~ a, predicted_data, mean)[2,2]
    
    m1_nat[,i] <- m1_pred[data_1_idx]
    m2_nat[,i] <- m2_pred[data_1_idx]
    
  }
  
  nat_diff <- mean(nat1_vec) - mean(nat0_vec)
  nat_0 <- mean(nat0_vec)
  nat_1 <- mean(nat1_vec)
  
  natural_rr <- nat_1 / nat_0
  
  # Obtain counterfactual predictions from the mediator model 
  # Compute mediator values after changing those with a = (1,1) to a = (0,0)
  # Compute mediator values after changing those with a = (1,1) to a = (0,1)
  # Compute mediator values after changing those with a = (1,1) to a = (1,0)
  
  count_1_00_vec <- rep(NA, reps)
  count_1_01_vec <- rep(NA, reps)
  count_1_10_vec <- rep(NA, reps)
  
  
  for(i in 1:reps){
    
    sigma_mat <- matrix(c(sigma1^2, 0, 0, sigma2^2), byrow = T, nrow = 2)
    
    eps_count_00 <- rmvnorm(nrow(data_1), mean = c(0,0), sigma = sigma_mat)
    eps_count_10 <- rmvnorm(nrow(data_1), mean = c(0,0), sigma = sigma_mat)
    eps_count_01 <- rmvnorm(nrow(data_1), mean = c(0,0), sigma = sigma_mat)
    
    # Counterfactual (all)
    
    newdata_00 <- data.frame(y = NA, a1 = 0, a2 = 0, m1 = NA, m2 = NA, c = data_1$c)
    
    if(sum(binary_m == c(F,F)) == 2){
      m1_pred00 <- predict(med1_model, newdata = newdata_00) + eps_count_00[,1]
    } else{
      m1_pred00 <- ifelse(predict(med1_model, newdata = newdata_00) + eps_count_00[,1] > 0, 1, 0)
    }
    
    if((sum(binary_m == c(F,F)) == 2) | (sum(binary_m == c(T,F)) == 2)){
      m2_pred00 <- predict(med2_model, newdata = newdata_00) + eps_count_00[,2]
    } else{
      m2_pred00 <- ifelse(predict(med2_model, newdata = newdata_00) + eps_count_00[,2] > 0, 1, 0)
    }
    
    
    predicted_data00 <- data.frame(y = NA, a = data_1$a, m1 = m1_pred00, m2 = m2_pred00, c = data_1$c)
    count_00_prob <- predict(out_model, newdata = predicted_data00, type = 'response')
    y_00_pred <- rbinom(nrow(data_1), 1, prob = count_00_prob)
    predicted_data00$y <- y_00_pred
    
    count_1_00_vec[i] <- mean(predicted_data00$y)
    
    # Counterfactual (changing one mediator at a time)
    
    newdata_01 <- data.frame(y = NA, a1 = 0, a2 = 1, m1 = NA, m2 = NA, c = data_1$c)
    newdata_10 <- data.frame(y = NA, a1 = 1, a2 = 0, m1 = NA, m2 = NA, c = data_1$c)
    
    if(sum(binary_m == c(F,F)) == 2){
      m1_pred_01 <- predict(med1_model, newdata = newdata_01) + rnorm(nrow(newdata_01), 0, sd = sigma1)
      m1_pred_10 <- predict(med1_model, newdata = newdata_10) + rnorm(nrow(newdata_10), 0, sd = sigma1)
    } else{
      m1_pred_01 <- ifelse(predict(med1_model, newdata = newdata_01) + rnorm(nrow(newdata_01), 0, sd = sigma1) > 0, 1, 0)
      m1_pred_10 <- ifelse(predict(med1_model, newdata = newdata_10) + rnorm(nrow(newdata_10), 0, sd = sigma1) > 0, 1, 0)
    }
    
    if((sum(binary_m == c(F,F)) == 2) | (sum(binary_m == c(T,F)) == 2)){
      m2_pred_01 <- predict(med2_model, newdata = newdata_01) + rnorm(nrow(newdata_01), 0, sd = sigma2)
      m2_pred_10 <- predict(med2_model, newdata = newdata_10) + rnorm(nrow(newdata_10), 0, sd = sigma2)
    } else{
      m2_pred_01 <- ifelse(predict(med2_model, newdata = newdata_01) + rnorm(nrow(newdata_01), 0, sd = sigma2) > 0, 1, 0)
      m2_pred_10 <- ifelse(predict(med2_model, newdata = newdata_10) + rnorm(nrow(newdata_10), 0, sd = sigma2) > 0, 1, 0)
      }

    predicted_data01 <- data.frame(y = NA, a = data_1$a, m1 = m1_pred_01, m2 = m2_pred_01, c = data_1$c)
    predicted_data10 <- data.frame(y = NA, a = data_1$a, m1 = m1_pred_10, m2 = m2_pred_10, c = data_1$c)
    
    count_01_prob <- predict(out_model, newdata = predicted_data01, type = 'response')
    y_01_pred <- rbinom(nrow(data_1), 1, prob = count_01_prob)
    
    count_10_prob <- predict(out_model, newdata = predicted_data10, type = 'response')
    y_10_pred <- rbinom(nrow(data_1), 1, prob = count_10_prob)
    
    count_1_10_vec[i] <- mean(y_10_pred)
    count_1_01_vec[i] <- mean(y_01_pred)
    
  }
  
  # Joint pathway
  
  count_00_diff <- mean(count_1_00_vec) - nat_0
  count_1_00 <- mean(count_1_00_vec)
  red_00 <- nat_diff - count_00_diff
  
  count_00_rr <- mean(count_1_00_vec) / nat_0
  red_00_rr <- natural_rr - count_00_rr
  
  # Only intervene on M1
  
  count_01_diff <- mean(count_1_01_vec) - nat_0
  count_1_01 <- mean(count_1_01_vec) 
  red_01 <- nat_diff - count_01_diff
  
  count_01_rr <- mean(count_1_01_vec) / nat_0
  red_01_rr <- natural_rr - count_01_rr
  
  # Only intervene on M2
  
  count_10_diff <- mean(count_1_10_vec) - nat_0
  count_1_10 <- mean(count_1_10_vec)
  red_10 <- nat_diff - count_10_diff
  
  count_10_rr <- mean(count_1_10_vec) / nat_0
  red_10_rr <- natural_rr - count_10_rr
  
  to_return <- c(nat_diff, count_00_diff, count_10_diff, count_01_diff,
                 red_00, red_10, red_01,
                 nat_1, nat_0, count_1_00, count_1_01, count_1_10,
                 natural_rr, count_00_rr, count_01_rr, count_10_rr,
                 red_00_rr, red_01_rr, red_10_rr)
  
  names(to_return) <- c('nat_diff', 'count_00_diff', 'count_10_diff', 'count_01_diff',
                        'red_00', 'red_10', 'red_01',
                        'nat_1', 'nat_0', 'count_1_00', 'count_1_01', 'count_1_10',
                        'natural_rr', 'count_00_rr', 'count_01_rr', 'count_10_rr',
                        'red_00_rr', 'red_01_rr', 'red_10_rr')
  
  return(to_return)
  
}
