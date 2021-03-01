
# script for reading data produced in simulation-srpe.R
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
folder_fits = paste0(base_folder, "srpe_fits\\")

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
missing_prop_mar = c("light", "medium", "strong")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
n_sim = length(files_fits)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_srpe_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits, i,"_d_srpe_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates = rbind(d_fit_estimates, temp_data_mcar, temp_data_mar)
}

# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.003
d_fit_estimates_srpe = d_fit_estimates %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "srpe", method != "No Imputation")

perf_estimates_targetcoef = d_fit_estimates_srpe %>% 
  group_by(method, missing_type, missing_amount) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            power = power(p, n()),
            mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
  arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup()

# save to csv
# save results
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
write_excel_csv(perf_estimates_targetcoef, "simulation_results_fits_srpe.csv", delim = ";", na = "")

#------------------- figures
d_fig = perf_estimates_targetcoef %>% select(method, missing_type, missing_amount, pb, rmse) %>% mutate(pb = pb/100)

d_fig_mcar = d_fig %>% filter(missing_type == "mcar")
d_fig_mar = d_fig %>% filter(missing_type == "mar") %>% mutate(missing_amount = case_when(missing_amount == "light" ~ "Light MAR",
                                                                                          missing_amount == "medium" ~ "Medium MAR",
                                                                                          missing_amount == "strong" ~ "Strong MAR"))

library(lmisc) # ggplot2 themes
ggplot(d_fig_mcar, aes(x = as.numeric(missing_amount), y = pb, group = method, color = method)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 2, alpha = 0.3) +
  geom_point(size = 2) +
  theme_line() +
  scale_y_continuous(labels = axis_percent) +
  ylab("Percent\nBias") + 
  xlab("% Missing under MCAR") + 
  scale_x_continuous(labels = axis_percent, breaks = scales::breaks_width(0.1, 0))  +
  theme(legend.position="bottom",
        legend.title=element_blank())

ggplot(d_fig_mcar, aes(x = as.numeric(missing_amount), y = rmse, group = method, color = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_line() +
  ylab("RMSE") + 
  xlab("% Missing under MCAR") +
  scale_x_continuous(labels = axis_percent, breaks = scales::breaks_width(0.1, 0)) +
  theme(legend.position="bottom",
        legend.title=element_blank())


ggplot(d_fig_mar, aes(x = rmse, y = method)) + 
  facet_wrap(~missing_amount) +
  ggstance::geom_barh(stat = "identity", fill = bjsm_blue) + 
  theme_barh() +
  xlab("Root-Mean-Squared Error (RMSE)") +
  ylab(NULL)

ggplot(d_fig_mar, aes(x = pb, y = method)) + 
  facet_wrap(~missing_amount) +
  ggstance::geom_barh(stat = "identity", fill = bjsm_blue) + 
  theme_barh() +
  xlab("Percent Bias") +
  ylab(NULL) +
  scale_x_continuous(labels = axis_percent)

#--------------------------------Read data and calculate performance measures on the raw data

# where the imputed datasets are saved
folder_imps = paste0(base_folder, "srpe_imps\\")
n_sim= 5
# we assume it is the same number of simulations for both simulations
# reading the simulated imputation datasets
files_imps = list.files(path = folder_imps)
d_imp = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_imps, i,"_d_srpe_imps_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_imps, i,"_d_srpe_imps_mar_",.,".rds"))) %>% bind_rows()
  d_imp = rbind(d_imp, temp_data_mcar, temp_data_mar)
}

perf_esimates_impvalues = d_imp %>% filter(method != "Complete Case Analysis", imp_place == 1) %>% 
  group_by(missing_type, missing_amount, method) %>% 
  summarise(rb = raw_bias(srpe, target),
            pb = percent_bias(srpe, target),
            rmse = rmse(srpe, target),
            mcse_rmse = mcse_rmse(srpe, target, n_sim)) %>% 
  arrange(missing_type, missing_amount, rmse)

## TODO visualize imputations
# densityplot_itt = densityplot(x=imp.itt, data = ~srpe)
# densityplot_jav = densityplot(x=imp.jav, data = ~srpe)
# densityplot_pas = densityplot(x=imp.pas, data = ~srpe)
# densityplot_id = densityplot(x=imp.id, data = ~srpe)

# xyplot(imp.itt, injury ~ srpe)

# nested-loop-plot?
