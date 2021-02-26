# script for reading data produced in simulation-substudy-total-distance.R
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

#--------------------------------Read data and calculate performance measures on model fits

base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_fits = paste0(base_folder, "substudy_td_fits\\")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
n_sim = length(files_fits)/2
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data = readRDS(paste0(folder_fits, i,"_d_td_fits.rds"))
  d_fit_estimates = rbind(d_fit_estimates, temp_data)
}

d_fit_estimates_nogps = data.frame()
for(i in 1:n_sim){
  temp_data = readRDS(paste0(folder_fits, i,"_d_td_nogps_fits.rds"))
  d_fit_estimates_nogps = rbind(d_fit_estimates_nogps, temp_data)
}

# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.0003
d_fit_estimates_td = d_fit_estimates %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td", method != "No Imputation")

d_fit_estimates_td_nogps = d_fit_estimates_nogps %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td", method != "No Imputation")

# calc performance measures for the two different datasets

perf_estimates_targetcoef = d_fit_estimates_td %>% 
  group_by(method) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            power = power(p, n()),
            mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
  arrange(desc(rmse)) %>% ungroup()

perf_estimates_targetcoef_nogps = d_fit_estimates_td_nogps %>% 
  group_by(method) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            power = power(p, n()),
            mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
  arrange(desc(rmse)) %>% ungroup()
