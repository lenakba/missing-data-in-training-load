# script for reading data produced in simulation-derived-variables.R
# and calculating performance measures

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors
options(scipen = 17, 
        stringsAsFactors = FALSE)

# loading packages
library(tidyverse) # for datawrangling
library(mice) # multiple imputation package

# fetching functions for performance parameters
# we assume working directory is the same location as this script
source("performance-measure-functions.R", encoding = "UTF-8")

#--------------------------------Read data and calculate performance measures

base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_fits = paste0(base_folder, "substudy_derived_var_fits\\")
folder_imps = paste0(base_folder, "substudy_derived_var_imps\\")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
runs = length(files_fits)
d_fit_estimates = data.frame()
for(i in 1:runs){
  temp_data = readRDS(paste0(folder_fits, i,"_d_derived_var_fits.rds"))
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
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.003
d_fit_estimates_srpe = d_fit_estimates %>% 
  filter(method != "No imputation") %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "srpe")

perf_estimates_targetcoef = d_fit_estimates_srpe %>% 
  group_by(method) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            mcse_rmse = mcse_rmse(estimate, target_est, runs),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), runs)) %>% 
  arrange(desc(rmse))

## TODO evaluate imputation points by themselves

# we assume it is the same number of runs for both simulations
# reading the simulated imputation datasets
files_imps = list.files(path = folder_imps)
d_imp = data.frame()
for(i in 1:runs){
  temp_data = readRDS(paste0(folder_imps, i,"_d_derived_var_imps.rds"))
  d_imp = rbind(d_imp, temp_data)
}

perf_esimates_impvalues = d_imp %>% filter(imp_place == 1) %>% 
  group_by(method) %>% 
  summarise(rb = raw_bias(srpe, target),
            pb = percent_bias(srpe, target),
            rmse = rmse(srpe, target),
            mcse_rmse = mcse_rmse(srpe, target, runs)) %>% 
  arrange(rmse)

## TODO visualize imputations
# densityplot_itt = densityplot(x=imp.itt, data = ~srpe)
# densityplot_jav = densityplot(x=imp.jav, data = ~srpe)
# densityplot_pas = densityplot(x=imp.pas, data = ~srpe)
# densityplot_id = densityplot(x=imp.id, data = ~srpe)

# xyplot(imp.itt, injury ~ srpe)