
# script for reading data produced in simulation-srpe.R
# and calculating performance measures

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors
options(scipen = 17, 
        stringsAsFactors = FALSE)

# loading packages
library(tidyverse) # for datawrangling
library(mice) # multiple imputation package

#--------------------------------Functions for calculating performance measures
# functions for easily calculating certain performance parameters
# needs vectors of estimates and target value
raw_bias = function(estimate, target){
  mean(estimate - target)
}

percent_bias = function(estimate, target){
  mean(100*((estimate - target)/target))
}

rmse = function(estimate, target){
  sqrt(mean((estimate - target)^2)) 
}

# monte carlo standard error also requires the number of simulations (runs, permutations)
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

average_width = function(ci_low, ci_high){
  mean(ci_high-ci_low)
}


#--------------------------------Read data and calculate performance measures

base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_da_fits = paste0(base_folder, "derived_var_fits\\")
folder_da_imps = paste0(base_folder, "derived_var_imps\\")

# reading the simulated results from fits
files_da_fits = list.files(path = folder_da_fits)
runs = length(files_da_fits)
d_fit_estimates = data.frame()
for(i in 1:runs){
  temp_data = readRDS(paste0(folder_da_fits, i,"_d_derived_var_fits.rds"))
  d_fit_estimates = rbind(d_fit_estimates, temp_data)
}