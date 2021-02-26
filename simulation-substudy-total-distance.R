# The aim of this script
# is to determine whether the method that comes up on top as the "best"
# is different in a scenario where only total distance is missing
# from a scenario where, if total distance is missing, so are all the other GPS measures

library(tidyverse) # for datawrangling
library(mice) # for imputation methods

# so we don't have to deal with scientific notations
# and strings aren't automaticcaly read as factors
options(scipen = 17, 
        stringsAsFactors = FALSE)

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")

# note that the Total Distance data is per day
d_td_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_td_anon.csv"), delim = ";")

# remove missing
# select vars we need in the simulation, key variables we think are correlated with the level of total distance
keyvars = c("p_id", "training_date", "mc_day", "week_nr")
d_td = na.omit(d_td_full) %>% 
  select(all_of(keyvars), td = total_distance_daily, v4 = v4_distance_daily, v5 = v5_distance_daily, pl = player_load_daily)

# adding a variable for match
# under the assumption that this is very predictive of total distance
d_td = d_td %>% mutate(match = ifelse(mc_day == "M", 1, 0))

#------------------------------------------Simulation
# we have all functions needed for performing the simulation in this section

# logistic function
log_reg = function(tl_coef){
  res = 1 / (1 + exp(-tl_coef))
  res
}

# linear logistic regression function
# where the level of total distance effects injury probability
inj_probability_td = function(td){
  y = log_reg(-2 + 0.0003*td) 
  y
}

# Create missing completely at random
add_mcar_td = function(d, missing_prop, missing_gps = FALSE){
  n_values = nrow(d)
  d = d %>% rownames_to_column()
  random_spots_td = sample(1:n_values, round(missing_prop*n_values))
  if(missing_gps){
  d = d %>% mutate_at(vars(td, v4, v5, pl), ~ifelse(rowname %in% random_spots_td, NA, .))
  } else {
  d = d %>% mutate(td = ifelse(rowname %in% random_spots_td, NA, td)) %>% dplyr::select(-rowname)
  }
  d
}


# impute mean function which calculates the mean by a chosen grouping variable
impute_mean = function(d, group_var){
  group_var = enquo(group_var)
  
  # calc mean
  d_imp = d %>% 
    group_by(!!group_var) %>% 
    mutate(m_td = mean(td, na.rm = TRUE)) %>% 
    ungroup()
  
  # impute with the mean
  d_imp = d_imp %>% mutate(td = ifelse(is.na(td), m_td, td)) %>% dplyr::select(-m_td)
  d_imp
}

# fit our models
fit_glm = function(d){
  fit = glm(injury ~ td, family = "binomial", data = d)
  fit
}

# function for obtaining parameters from any model fit
# specify method for a column with the model-type
# and whether its based on a mids object or not
get_params = function(fit, method, pool = FALSE){
  if(pool){
    d_params = summary(mice::pool(fit), "all", conf.int = TRUE) %>% 
      dplyr::select(term, estimate, CI_low = "2.5 %", CI_high = "97.5 %", p = p.value) %>% 
      as_tibble() %>% 
      mutate(term =  as.character(term))
  } else {
    d_params = parameters::parameters(fit) %>% tibble() %>% 
      dplyr::select(term = Parameter, estimate = Coefficient, CI_low, CI_high, p)
  }
  d_params = d_params %>% mutate(method = method) 
  d_params
}

# The final, helper function that performs all the imputations at once, given a dataset with missing
# then, it fits a logistic regression model on the data
# and outputs the needed model parameters for the validation of the imputation models
sim_impfit = function(d_missing, target_param, rep = 1){
  
  #-----------------impute using all the different methods
  # Mean imputation by the mean per player
  d.mean.p_id = impute_mean(d_missing, p_id)
  
  # Mean imputation by the mean per week
  d.mean.week = impute_mean(d_missing, week_nr)
  
  # Multiple Imputation - Regression imputation
  mids.reg = mice(d_missing, method = "norm.predict", seed = 1234, m = 5, print = FALSE)
  
  # Multiple Imputation - Predicted Mean Matching
  mids.pmm = mice(d_missing, seed = 1234, m = 5, print = FALSE)
  
  # complete case analysis
  d.cc = na.omit(d_missing)
  
  # fit our models
  fit.mean.p_id = fit_glm(d.mean.p_id)
  fit.mean.week = fit_glm(d.mean.week)
  fit.reg =  with(mids.reg, glm(injury ~ td, family = binomial))
  fit.pmm =  with(mids.pmm, glm(injury ~ td, family = binomial))
  fit.cc =  fit_glm(d.cc)
  
  # fetch model parameters
  tab1 = get_params(fit.mean.p_id, "Mean Imputation - Mean per player")  
  tab2 = get_params(fit.mean.week, "Mean Imputation - Mean per week")  
  tab3 = get_params(fit.reg, "MI - Regression Imputation", pool = TRUE)  
  tab4 = get_params(fit.pmm, "MI - PMM", pool = TRUE)  
  tab5 = get_params(fit.cc, "Complete Case Analysis")  
  
  d_fits = bind_rows(target_param, tab1, tab2, tab3, tab4, tab5)
  d_fits = d_fits %>% mutate(rep = rep)
  d_fits
}

# The first helper function outputs model fit estimated parameters
# we also want the imputed values as is

# function for adding the target total distance values
# to an imputed dataset
add_target_imp = function(d, imp_rows_pos, target, method){
  d = d %>% rownames_to_column() %>%
    mutate(imp_place = ifelse(rowname %in% imp_rows_pos, 1, 0),
           target = target,
           method = method) %>%
    dplyr::select(method, imp_place, td, target)
  d
}

# helper function that imputes using all methods and outputs the
# data with the target srpe we wish to compare
# in theory, it would be more computationally efficient to have 1 function
# that imputes data, 1 function that runs the fits on data from the first function,
# and 1 function that outputs the imputed data from the first function.
# instead, we have 2 functions that both perform step 1, i.e., we are running it double.
# this is because more computationally power-efficient code
# would require more complicated programming. We would have to run fits on lists of data instead of
# a single dataset, and pooling might have to be manually implemented according to Ruben's rules.
# The simulation is, experienced from our derived-variable substudy, not that computationally heavy,
# and so I think this solution is fine, although it breaks the Do-not-Repeat-Yourself (DRY) Principle.
sim_imp = function(d_missing, target, run = 1){
  
  # find which rows have missing and need imputation
  imp_rows_pos = which(is.na(d_missing$td))
  
  #-----------------impute using all the different methods
  # Mean imputation by the mean per player
  d.mean.p_id = impute_mean(d_missing, p_id)
  
  # Mean imputation by the mean per week
  d.mean.week = impute_mean(d_missing, week_nr)
  
  # Multiple Imputation - Regression imputation
  mids.reg = mice(d_missing, method = "norm.predict", seed = 1234, m = 5, print = FALSE)
  
  # Multiple Imputation - Predicted Mean Matching
  mids.pmm = mice(d_missing, seed = 1234, m = 5, print = FALSE)
  
  # complete case analysis
  d.cc = na.omit(d_missing)
  
  # add column of which row was imputed
  d.mean.p_id = d.mean.p_id %>% add_target_imp(., imp_rows_pos, target = target, method = "Mean Imputation - Mean per player")
  d.mean.week = d.mean.week %>% add_target_imp(., imp_rows_pos, target = target, method = "Mean Imputation - Mean per week")
  
  # since multiple imputation has 5 datasets, the column is added to a list of datasets
  d.reg = mice::complete(mids.reg, "all") %>%
    map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "MI - Regression Imputation")) %>%
    imap(., ~mutate(., dataset_n = .y)) %>% # a column for which dataset number, as multiple imputation imputes multiple
    bind_rows()
  
  d.pmm = mice::complete(mids.pmm, "all") %>%
    map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "MI - PMM")) %>%
    imap(., ~mutate(., dataset_n = .y)) %>% # a column for which dataset number, as multiple imputation imputes multiple
    bind_rows()
  
  # target column can't be added to complete case analysis, because they've been listwise deleted
  d.cc = d.cc %>% mutate(method = "Complete Case Analysis") %>% dplyr::select(method, td)
  
  # combine to 1 dataset
  d_imp = bind_rows(d.mean.p_id, d.mean.week, d.reg, d.pmm, d.cc) %>% tibble()
  d_imp
}

###############Data Preparation

# create fake injuries
d_td = d_td %>% 
  mutate(inj_prop = inj_probability_td(td), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# logistic regression for comparison
fit.target = glm(injury ~ td, family = "binomial", data = d_td)
target_param = get_params(fit.target, "No Imputation")  

# remove variables we in theory wouldn't know about
# in a real life situation
d_exdata_td = d_td %>% dplyr::select(-inj_prop)

# fetch our original sRPE column, which is our original, true value, and we aim to target it
target_col = d_td$td

#####################For-loop simulation

# performing simulations with n runs
# the warnings are caused by collinearity between the variables
# which is expected
base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_fits = paste0(base_folder, "substudy_td_fits\\")
folder_imps = paste0(base_folder, "substudy_td_imps\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  d_missing = add_mcar(d_exdata, 0.25)
  d_sim_fits = sim_impfit_derivedvar(d_missing, run = i)
  d_sim_imps = sim_imp_derivedvar(d_missing, target_srpe, run = i)
  saveRDS(d_sim_fits, file=paste0(folder_fits, i,"_d_td_fits.rds"))
  saveRDS(d_sim_imps, file=paste0(folder_imps, i,"_d_td_imps.rds"))
  
  d_missing_nogps = add_mcar(d_exdata, 0.25, TRUE)
  d_sim_fits = sim_impfit_derivedvar(d_missing_nogps, run = i)
  d_sim_imps = sim_imp_derivedvar(d_missing_nogps, target_srpe, run = i)
  saveRDS(d_sim_fits, file=paste0(folder_fits, i,"_d_td_nogps_fits.rds"))
  saveRDS(d_sim_imps, file=paste0(folder_imps, i,"_d_td_nogps_imps.rds"))
}
options(warn=0)