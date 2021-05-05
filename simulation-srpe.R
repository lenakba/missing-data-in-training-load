# this script is for simulating missing data
# and compare different methods of imputation
library(tidyverse) # for datawrangling
library(mice) # for imputation methods

# so we don't have to deal with scientific notations
# and strings aren't automaticcaly read as factors
options(scipen = 17, 
        stringsAsFactors = FALSE)

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")

# note that the RPE data is per session (which there can be multiple of per day)
d_rpe_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_rpe_anon.csv"), delim = ";")

# remove missing
# select vars we need in the simulation, key variables we think are correlated with the level of sRPE
keyvars = c("p_id", "training_date", "mc_day", "week_nr")
d_srpe = d_rpe_full %>% filter(!is.na(rpe) & !is.na(duration)) %>% select(all_of(keyvars), rpe, duration) %>% mutate(srpe = rpe*duration)

# adding a variable for match
# under the assumption that this is very predictive of sRPE
d_srpe = d_srpe %>% mutate(match = ifelse(mc_day == "M", 1, 0))

#------------------------------------------Simulation
# we have all functions needed for performing the simulation in this section
# if you want to look at each step in the simulation, please start at the section Step 1

# All the functions we need are listed first

# logistic function
log_reg = function(tl_coef){
  res = 1 / (1 + exp(-tl_coef))
  res
}

# linear logistic regression function
# where the level of sRPE effects injury probability
inj_probability_srpe = function(srpe){
  y = log_reg(-2 + 0.003*srpe) 
  y
}

# Create missing completely at random
add_mcar_rpe = function(d, missing_prop){
  n_values = nrow(d)
  d = d %>% rownames_to_column()
  random_spots_rpe = sample(1:n_values, round(missing_prop*n_values))
  random_spots_min = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(rpe = ifelse(rowname %in% random_spots_rpe, NA, rpe),
                   duration = ifelse(rowname %in% random_spots_min, NA, duration)) %>% dplyr::select(-rowname)
  d
}

# Create missing with Missing probability based on other variables (Missing at Random)
# linear logistic regression function
# for the relationship between different variables and the probability of missing
mar_function = function(d, corr){
  if(corr == "light"){
    y = log_reg(-2 + (0.03*d$age) + (0.02*d$sex) + (0.3*d$freeday))
  } else if(corr == "medium"){
    y = log_reg(-2 + (0.08*d$age) + (0.04*d$sex) + (0.8*d$freeday))    
  }  else if(corr == "strong"){
    y = log_reg(-2 + (0.13*d$age) + (0.1*d$sex) + (1.8*d$freeday) + (1.8*d$match))     
  }
  y
}

# adding missing at random to a dataset
add_mar_rpe = function(d, corr){
  d = d %>% mutate(na_prop = mar_function(., corr),
                   na_spot_rpe = rbinom(length(na_prop), 1, prob = na_prop),
                   na_spot_duration = rbinom(length(na_prop), 1, prob = na_prop),
                   rpe = ifelse(na_spot_rpe == 1, NA, rpe),
                   duration = ifelse(na_spot_duration == 1, NA, duration))
  d %>% dplyr::select(-starts_with("na"))
}

# impute mean function which calculates the mean by a chosen grouping variable
impute_mean = function(d, group_var){
  group_var = enquo(group_var)
  
  # calc mean
  d_imp = d %>% 
    group_by(!!group_var) %>% 
    mutate(m_rpe = mean(rpe, na.rm = TRUE),
           m_duration = mean(duration, na.rm = TRUE)) %>% 
    ungroup()
  
  # impute with the mean
  d_imp = d_imp %>% mutate(rpe = ifelse(is.na(rpe), m_rpe, rpe),
                           duration = ifelse(is.na(duration), m_duration, duration),
                           srpe = rpe*duration) %>% 
    dplyr::select(-m_rpe, -m_duration)
  d_imp
}

# fit our models
fit_glm = function(d){
  fit = glm(injury ~ srpe, family = "binomial", data = d)
  fit
}

# function for obtaining parameters from any model fit
# specify method for a column with the model-type
# and whether its based on a mids object or not
get_params = function(fit, method, pool = FALSE){
  if(pool){
    d_params = summary(mice::pool(fit), "all", conf.int = TRUE) %>% 
      dplyr::select(term, std.error, estimate, CI_low = "2.5 %", CI_high = "97.5 %", p = p.value) %>% 
      as_tibble() %>% 
      mutate(term =  as.character(term))
  } else {
    d_params = parameters::parameters(fit) %>% tibble() %>% 
      dplyr::select(term = Parameter, std.error = SE, estimate = Coefficient, CI_low, CI_high, p)
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
  
  # Mulitple Imputation - Regression imputation
  mids.reg = mice(d_missing, method = "norm.predict", seed = 1234, m = 1, print = FALSE)
  imp.reg = mice::complete(mids.reg, "long", include = TRUE)
  imp.reg$srpe = with(imp.reg, rpe*duration)
  mids.reg = as.mids(imp.reg)
  
  # Multiple imputation - predicted mean matching
  mids.pmm = mice(d_missing, print = FALSE, seed = 1234)
  imp.pmm = mice::complete(mids.pmm, "long", include = TRUE)
  imp.pmm$srpe = with(imp.pmm, rpe*duration)
  mids.itt.pmm = as.mids(imp.pmm)
  
  # complete case analysis
  d.cc = na.omit(d_missing) %>% mutate(srpe = rpe*duration)
  
  # fit our models
  fit.mean.p_id = fit_glm(d.mean.p_id)
  fit.mean.week = fit_glm(d.mean.week)
  fit.reg =  with(mids.reg, glm(injury ~ srpe, family = binomial))
  fit.pmm =  with(mids.itt.pmm, glm(injury ~ srpe, family = binomial))
  fit.cc =  fit_glm(d.cc)
  
  # fetch model parameters
  tab1 = get_params(fit.mean.p_id, "Mean Imputation - Mean per player")  
  tab2 = get_params(fit.mean.week, "Mean Imputation - Mean per week")  
  tab3 = summary(fit.reg, conf.int = TRUE) %>% 
    dplyr::select(term, std.error, estimate, CI_low = "conf.low", CI_high = "conf.high", p = p.value) %>% 
    as_tibble() %>% 
    mutate(term =  as.character(term),
           method = "Regression Imputation")
  tab4 = get_params(fit.pmm, "MI - PMM", pool = TRUE)  
  tab5 = get_params(fit.cc, "Complete Case Analysis")  
  
  d_fits = bind_rows(target_param, tab1, tab2, tab3, tab4, tab5)
  d_fits = d_fits %>% mutate(rep = rep)
  d_fits
}

# The first helper function outputs model fit estimated parameters
# we also want the imputed values as is

# function for adding the target srpe values
# to an imputed dataset
add_target_imp = function(d, imp_rows_pos, target, method){
  d = d %>% rownames_to_column() %>%
    mutate(imp_place = ifelse(rowname %in% imp_rows_pos, 1, 0),
           target = target,
           method = method) %>%
    dplyr::select(method, imp_place, rpe, duration, srpe, target)
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
  imp_rows_pos = which(is.na(d_missing$rpe) | is.na(d_missing$duration))
  
  #-----------------impute using all the different methods
  # Mean imputation by the mean per player
  d.mean.p_id = impute_mean(d_missing, p_id)
  
  # Mean imputation by the mean per week
  d.mean.week = impute_mean(d_missing, week_nr)
  
  # Multiple Imputation - Regression imputation
  mids.reg = mice(d_missing, method = "norm.predict", seed = 1234, m = 1, print = FALSE)
  imp.reg = mice::complete(mids.reg, "long", include = TRUE)
  imp.reg$srpe = with(imp.reg, rpe*duration)
  mids.reg = as.mids(imp.reg)
  
  # Multiple Imputation - Predicted Mean Matching
  mids.pmm = mice(d_missing, print = FALSE, seed = 1234)
  imp.pmm = mice::complete(mids.pmm, "long", include = TRUE)
  imp.pmm$srpe = with(imp.pmm, rpe*duration)
  mids.itt.pmm = as.mids(imp.pmm)
  
  # complete case analysis
  d.cc = na.omit(d_missing) %>% mutate(srpe = rpe*duration)
  
  # add column of which row was imputed
  d.mean.p_id = d.mean.p_id %>% add_target_imp(., imp_rows_pos, target = target, method = "Mean Imputation - Mean per player")
  d.mean.week = d.mean.week %>% add_target_imp(., imp_rows_pos, target = target, method = "Mean Imputation - Mean per week")

  # since multiple imputation has 5 datasets, the column is added to a list of datasets
  d.reg = mice::complete(mids.reg, "all") %>%
    map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "Regression Imputation")) %>%
    imap(., ~mutate(., dataset_n = .y)) %>% # a column for which dataset number, as multiple imputation imputes multiple
    bind_rows() %>% filter(dataset_n != 0) # unimputed dataset is included in the ITT method, we remove this
  
  d.pmm = mice::complete(mids.itt.pmm, "all") %>%
          map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "MI - PMM")) %>%
          imap(., ~mutate(., dataset_n = .y)) %>% # a column for which dataset number, as multiple imputation imputes multiple
          bind_rows() %>% filter(dataset_n != 0) # unimputed dataset is included in the ITT method, we remove this
 
   # target column can't be added to complete case analysis, because they've been listwise deleted
   d.cc = d.cc %>% mutate(method = "Complete Case Analysis") %>% dplyr::select(method, rpe, duration, srpe)
  
  # combine to 1 dataset
  d_imp = bind_rows(d.mean.p_id, d.mean.week, d.reg, d.pmm, d.cc) %>% tibble()
  d_imp
}

###############Data Preparation

# create fake injuries
d_srpe = d_srpe %>% 
  mutate(inj_prop = inj_probability_srpe(srpe), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# logistic regression for comparison
fit.target = glm(injury ~ srpe, family = "binomial", data = d_srpe)
target_param = get_params(fit.target, "No Imputation")  

# remove variables we in theory wouldn't know about
# in a real life situation
d_exdata_srpe = d_srpe %>% dplyr::select(-inj_prop, -srpe)

# for missing at random, we create fake variables with correlation to the amount of missing
# we add fake age and sex, and we use the day of the week to determine weekend
# we'll use these variables to create Missing at Random
srpe_p_id = d_srpe %>% distinct(p_id) 
srpe_id_base = srpe_p_id %>% mutate(age = sample(18:30, length(srpe_p_id$p_id), replace = TRUE),
                                    sex = sample(0:1, length(srpe_p_id$p_id), replace = TRUE))
d_exdata_mar = d_exdata_srpe %>% 
  left_join(srpe_id_base, by = "p_id") %>% 
  mutate(freeday = ifelse(mc_day == "M+1" | mc_day == "M+2", 1, 0)) %>% 
  select(-sex, -age, -freeday)

# fetch our original sRPE column, which is our original, true value, and we aim to target it
target_col = d_srpe$srpe

#####################For-loop simulation

# performing simulations with n runs
# the warnings are caused by collinearity between the variables
# which is expected
base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_fits = paste0(base_folder, "srpe_fits\\")
folder_imps = paste0(base_folder, "srpe_imps\\")

# helper function for performing all needed simulations
# given a missing type (missing), either "mcar" or "mar"
# and amount of missing, a proportion for mcar or "light"/"medium"/"strong" for mar
# not that the datasets have to be ready beforehand, this is NOT a general function
sim_impute = function(missing, missing_amount, rep){
  
  if(missing == "mcar"){
  d_mcar = add_mcar_rpe(d_exdata_srpe, missing_amount)
  d_sim_fits_mcar = sim_impfit(d_mcar, target_param, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
  saveRDS(d_sim_fits_mcar, file=paste0(folder_fits, rep,"_d_srpe_fits_", missing, "_", missing_amount,".rds"))  
  d_sim_imps_mcar = sim_imp(d_mcar, target_col, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
  saveRDS(d_sim_imps_mcar, file=paste0(folder_imps, rep,"_d_srpe_imps_", missing, "_", missing_amount,".rds"))
  
  } else if(missing == "mar"){
  d_mar = add_mar_rpe(d_exdata_mar, missing_amount)
  d_sim_fits_mar = sim_impfit(d_mar, target_param, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
  saveRDS(d_sim_fits_mar, file=paste0(folder_fits, rep,"_d_srpe_fits_", missing, "_", missing_amount,".rds")) 
  d_sim_imps_mar = sim_imp(d_mar, target_col, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
  saveRDS(d_sim_imps_mar, file=paste0(folder_imps, rep,"_d_srpe_imps_", missing, "_", missing_amount,".rds"))
  }
}

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
missing_prop_mar = c("light", "medium", "strong")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  # walk will run the function for each missing proportion in the vector
  # without attempting to spit out a list (in comparison to map(), which will create a list or die trying)
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., rep = i))
}
options(warn=0)

#------------------------------------------------to check if having position changes the results
folder_fits = paste0(base_folder, "srpe_fits_pos\\")
folder_imps = paste0(base_folder, "srpe_imps_pos\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  # walk will run the function for each missing proportion in the vector
  # without attempting to spit out a list (in comparison to map(), which will create a list or die trying)
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., rep = i))
}
options(warn=0)
