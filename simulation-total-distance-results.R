# script for reading data produced in simulation-total-distance.R
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


#--------------------------------Read data and calculate performance measures on model fits

base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_da_fits = paste0(base_folder, "td_fits\\")

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8)
missing_prop_mar = c("light", "medium", "strong")

# reading the simulated results from fits
files_da_fits = list.files(path = folder_da_fits)
n_sim = length(files_da_fits)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_da_fits, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_da_fits, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates = rbind(d_fit_estimates, temp_data_mcar, temp_data_mar)
}

# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.003
d_fit_estimates_td = d_fit_estimates %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td")

perf_estimates_targetcoef = d_fit_estimates_td %>% 
  group_by(method, missing_type, missing_amount) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
  arrange(missing_type, missing_amount, rb) %>% ungroup()

# save to csv
# save results
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
write_excel_csv(perf_estimates_targetcoef, "simulation_results_fits_td.csv", delim = ";", na = "")

#--------------------------------Read data and calculate performance measures on the raw data

# where the imputed datasets are saved
folder_da_imps = paste0(base_folder, "td_imps\\")
n_sim= 5
# we assume it is the same number of simulations for both simulations
# reading the simulated imputation datasets
files_da_imps = list.files(path = folder_da_imps)
d_imp = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_da_imps, i,"_d_td_imps_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_da_imps, i,"_d_td_imps_mar_",.,".rds"))) %>% bind_rows()
  d_imp = rbind(d_imp, temp_data_mcar, temp_data_mar)
}

perf_esimates_impvalues = d_imp %>% filter(method != "Complete Case Analysis", imp_place == 1) %>% 
  group_by(missing_type, missing_amount, method) %>% 
  summarise(rb = raw_bias(td, target),
            pb = percent_bias(td, target),
            rmse = rmse(td, target),
            mcse_rmse = mcse_rmse(td, target, n_sim)) %>% 
  arrange(missing_type, missing_amount, rmse)

## TODO visualize imputations
# densityplot_itt = densityplot(x=imp.itt, data = ~td)
# densityplot_jav = densityplot(x=imp.jav, data = ~td)
# densityplot_pas = densityplot(x=imp.pas, data = ~td)
# densityplot_id = densityplot(x=imp.id, data = ~td)

# xyplot(imp.itt, injury ~ td)

# nested-loop-plot?