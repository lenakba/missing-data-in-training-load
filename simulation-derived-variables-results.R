# script for reading data produced in simulation-derived-variables.R
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
  mcse = sqrt(((95-cr)*(5-cr))/nsim)
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

# between simulation variation estimates
perf_estimates_simvar = d_fit_estimates %>% 
  group_by(method) %>% 
  summarise(rb = raw_bias(estimate, 1),
            pb = percent_bias(estimate, 1),
            rmse = rmse(estimate, 1),
            coverage = coverage(CI_low, CI_high, 1, n()),
            average_width = average_width(CI_low, CI_high),
            mcse_rmse = mcse_rmse(estimate, rep(1, n()), runs),
            mcse_coverage = mcse_coverage(CI_low, CI_high, 1, n(), runs)) %>% ungroup()

# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target param as ideal
target_est = d_fit_estimates %>% filter(term == "srpe", method == "No imputation") %>% distinct(estimate) %>% pull()
d_fit_estimates_srpe = d_fit_estimates %>% mutate(target_est = target_est) %>% filter(term == "srpe")
perf_estimates_targetcoef = d_fit_estimates_srpe %>% 
  filter(method != "No imputation") %>% 
  group_by(method) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            mcse_rmse = mcse_rmse(estimate, target_est, runs),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, runs))

## TODO evaluate imputation points by themselves

# we assume it is the same number of runs for both simulations
# reading the simulated imputation datasets
files_da_imps = list.files(path = folder_da_imps)
d_imp = data.frame()
for(i in 1:runs){
  temp_data = readRDS(paste0(folder_da_imps, i,"_d_derived_var_imps.rds"))
  d_imp = rbind(d_imp, temp_data)
}

# l_imputed1 = mice::complete(imp.itt, "all") 
# l_imputed2 = mice::complete(imp.jav, "all", include = TRUE)
# l_imputed3 = mice::complete(imp.pas, "all")
# l_imputed4 = mice::complete(imp.id, "all") 

# how accurately do these methods impute?
# imp_rows = which(is.na(d_missing$rpe) | is.na(d_missing$duration))
# calc_imp_params = function(d, method){
# d = d %>% rownames_to_column() %>% 
#   mutate(imp_place = ifelse(rowname %in% imp_rows, 1, 0), 
#          target_srpe = target_srpe,
#          method = method) %>% 
#   dplyr::select(-rowname) %>% 
#   filter(imp_place == 1) %>% 
#   mutate(raw_bias = srpe-target_srpe,
#          perc_bias = round(100*((srpe-target_srpe)/target_srpe)),
#          rmse = sqrt((srpe-target_srpe)^2)) %>% 
#   summarise(raw_bias = mean(raw_bias, na.rm = TRUE),
#             perc_bias = mean(perc_bias, na.rm = TRUE),
#             rmse = mean(rmse, na.rm = TRUE))
#   d
# }
# 
# l_imputed1 %>% map(. %>% calc_imp_params(., method = "ITT"))
# l_imputed2 %>% map(. %>% calc_imp_params(., method = "JAV"))
# l_imputed3 %>% map(. %>% calc_imp_params(., method = "PAS"))
# l_imputed4 %>% map(. %>% calc_imp_params(., method = "ID"))

## TODO visualize imputations
# densityplot_itt = densityplot(x=imp.itt, data = ~srpe)
# densityplot_jav = densityplot(x=imp.jav, data = ~srpe)
# densityplot_pas = densityplot(x=imp.pas, data = ~srpe)
# densityplot_id = densityplot(x=imp.id, data = ~srpe)

# xyplot(imp.itt, injury ~ srpe)