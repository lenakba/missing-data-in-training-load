# this script has all the functions for
# calculating performance parameters
# in all scripts used in the missing data study

#--------------------------------Functions for calculating performance measures
# functions for easily calculating certain performance parameters
# needs vectors of estimates and target value

# amount of bias toward a target estimate
raw_bias = function(estimate, target){
  mean(abs(estimate - target))
}

percent_bias = function(estimate, target){
  mean(100*((estimate - target)/target))
}

abs_percent_bias = function(estimate, target){
  mean(100*(abs((estimate - target)/target)))
}

# Root-mean-squared-error
rmse = function(estimate, target){
  sqrt(mean((estimate - target)^2)) 
}

# monte carlo standard error also requires the number of simulations (runs, permutations)
mcse_bias = function(estimate, target, nsim){
  
  d_se = bind_cols(data.frame(estimate), data.frame(target)) 
  d_est = data.frame(numeric(nrow(d_se)))
  colnames(d_est) = "bias_j"
  for(i in 1:nrow(d_se)){
    d_temp = d_se[-i,]
    rb = raw_bias(d_temp$estimate, d_temp$target)
    d_est[i,1] = rb
  }
  
  bias_j = d_est$bias_j
  main_rb = raw_bias(estimate, target)
  mcse = sqrt(sum((bias_j-main_rb)^2)/(nsim*(nsim-1)))
  mcse
}

mcse_rmse = function(estimate, target, nsim){
  
  d_se = bind_cols(data.frame(estimate), data.frame(target)) 
  d_est = data.frame(numeric(nrow(d_se)))
  colnames(d_est) = "rmse_j"
  for(i in 1:nrow(d_se)){
    d_temp = d_se[-i,]
    rmse = rmse(d_temp$estimate, d_temp$target)
    d_est[i,1] = rmse
  }
  
  rmse_j = d_est$rmse_j
  main_rmse = rmse(estimate, target)
  mcse = sqrt(sum((rmse_j-main_rmse)^2)/(nsim*(nsim-1)))
  mcse
}

# coverage = % confidence intervals that overlap with the target estimate
# coverage requires vectors of low and high confidence intervals and a vector with the target coefficient
# the denominator is usually the number of values in the vectors
coverage = function(ci_low, ci_high, target, denominator){
  is_covered = ifelse((ci_low < target) & (target < ci_high), 1, 0)
  cr = 100*(sum(is_covered == 1, na.rm = TRUE)/denominator)
  cr
}

mcse_coverage = function(ci_low, ci_high, target, denominator, nsim){
  is_covered = ifelse((ci_low < target) & (target < ci_high), 1, 0)
  cr = 100*(sum(is_covered == 1, na.rm = TRUE)/denominator)
  mcse = sqrt(abs(((95-cr)*(5-cr)))/nsim)
  mcse
}

# average width of confidence intervals
average_width = function(ci_low, ci_high){
  mean(ci_high-ci_low)
}

# % power to detect a significant relationship
power = function(p, denominator){
  is_sig = ifelse(p <= 0.05, 1, 0)
  power = 100*(sum(is_sig == 1, na.rm = TRUE)/denominator)
  power
}
