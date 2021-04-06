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

# note that the Total Distance data is per day
d_td_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_td_anon.csv"), delim = ";")
d_td_full = d_td_full %>% rename(gps_td = total_distance_daily, gps_v4 = v4_distance_daily, gps_v5 = v5_distance_daily, gps_pl = player_load_daily)

# remove missing
# this has to be done before choosing variables so that the variables will be comparable
# i.e. the results of having or not having sRPE and position in the dataset cannot be confused with
# differing sample sizes due to unequal levels of missing.
d_td = na.omit(d_td_full)

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
inj_probability_td = function(gps_td){
  y = log_reg(-2 + 0.0003*gps_td) 
  y
}

# Create missing completely at random
add_mcar_td = function(d, missing_prop){
  n_values = nrow(d)
  d = d %>% rownames_to_column()
  random_spots_td = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(gps_td = ifelse(rowname %in% random_spots_td, NA, gps_td)) %>% dplyr::select(-rowname)
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
add_mar_td = function(d, corr){
  d = d %>% mutate(na_prop = mar_function(., corr),
                   na_spot_td = rbinom(length(na_prop), 1, prob = na_prop),
                   gps_td = ifelse(na_spot_td == 1, NA, gps_td))
  d %>% dplyr::select(-starts_with("na"))
}

# impute mean function which calculates the mean by a chosen grouping variable
impute_mean = function(d, group_var){
  group_var = enquo(group_var)
  
  # calc mean
  d_imp = d %>% 
    group_by(!!group_var) %>% 
    mutate(m_td = mean(gps_td, na.rm = TRUE)) %>% 
    ungroup()
  
  # impute with the mean
  d_imp = d_imp %>% mutate(gps_td = ifelse(is.na(gps_td), m_td, gps_td)) %>% dplyr::select(-m_td)
  d_imp
}

# fit our models
fit_glm = function(d){
  fit = glm(injury ~ gps_td, family = "binomial", data = d)
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
  fit.reg =  with(mids.reg, glm(injury ~ gps_td, family = binomial))
  fit.pmm =  with(mids.pmm, glm(injury ~ gps_td, family = binomial))
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
    dplyr::select(method, imp_place, gps_td, target)
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
  imp_rows_pos = which(is.na(d_missing$gps_td))
  
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
  d.cc = d.cc %>% mutate(method = "Complete Case Analysis") %>% dplyr::select(method, gps_td)
  
  # combine to 1 dataset
  d_imp = bind_rows(d.mean.p_id, d.mean.week, d.reg, d.pmm, d.cc) %>% tibble()
  d_imp
}

###############Data Preparation

# create fake injuries
d_td = d_td %>% 
  mutate(inj_prop = inj_probability_td(gps_td), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# logistic regression for comparison
fit.target = glm(injury ~ gps_td, family = "binomial", data = d_td)
target_param = get_params(fit.target, "No Imputation")  

# remove variables we in theory wouldn't know about
# in a real life situation
# select vars we need in the simulation, key variables we think are correlated with the level of total distance
keyvars = c("p_id", "training_date", "mc_day", "week_nr")
d_exdata = d_td %>% select(all_of(keyvars), match, injury, starts_with("gps"), position, srpe)

# for missing at random, we create fake variables with correlation to the amount of missing
# we add fake age and sex, and we use the day of the week to determine weekend
# we'll use these variables to create Missing at Random
td_p_id = d_td %>% distinct(p_id) 
td_id_base = td_p_id %>% mutate(age = sample(18:30, length(td_p_id$p_id), replace = TRUE),
                                    sex = sample(0:1, length(td_p_id$p_id), replace = TRUE))

d_exdata_mar = d_exdata %>% 
  left_join(td_id_base, by = "p_id") %>% 
  mutate(freeday = ifelse(mc_day == "M+1" | mc_day == "M+2", 1, 0))

# fetch our original sRPE column, which is our original, true value, and we aim to target it
target_col = d_td$gps_td

# 3 datasets with varying levels of information
# no extra variables available
d_exdata_td_noextra = d_exdata %>% select(-position, -srpe)
# position available as extra variable
d_exdata_td_pos = d_exdata %>% select(-srpe)
# sRPE and position available as extra variable
d_exdata_td_srpe_pos = d_exdata

# same for the MAR dataset
d_exdata_mar_noextra = d_exdata_mar %>% select(-position, -srpe)
d_exdata_mar_pos = d_exdata_mar %>% select(-srpe)
d_exdata_mar_srpe_pos = d_exdata_mar

#####################For-loop simulation

# helper function for performing all needed simulations
# given a missing type (missing), either "mcar" or "mar"
# and amount of missing, a proportion for mcar or "light"/"medium"/"strong" for mar
# not that the datasets have to be ready beforehand, this is NOT a general function
sim_impute = function(missing, missing_amount, d, folder_fits, folder_imps, rep){
  
  if(missing == "mcar"){
    d_mcar = add_mcar_td(d, missing_amount)
    d_sim_fits_mcar = sim_impfit(d_mcar, target_param, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
    saveRDS(d_sim_fits_mcar, file=paste0(folder_fits, rep,"_d_td_fits_", missing, "_", missing_amount,".rds"))  
    d_sim_imps_mcar = sim_imp(d_mcar, target_col, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
    saveRDS(d_sim_imps_mcar, file=paste0(folder_imps, rep,"_d_td_imps_", missing, "_", missing_amount,".rds"))
    
  } else if(missing == "mar"){
    d_mar = add_mar_td(d, missing_amount)
    d_sim_fits_mar = sim_impfit(d_mar, target_param, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
    saveRDS(d_sim_fits_mar, file=paste0(folder_fits, rep,"_d_td_fits_", missing, "_", missing_amount,".rds")) 
    d_sim_imps_mar = sim_imp(d_mar, target_col, rep) %>% mutate(missing_type = missing, missing_amount = missing_amount)
    saveRDS(d_sim_imps_mar, file=paste0(folder_imps, rep,"_d_td_imps_", missing, "_", missing_amount,".rds"))
  }
}

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
missing_prop_mar = c("light", "medium", "strong")

# base folder where all the folders for simulations are to be saved
base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"

#------------------------First variant with no extra variables
folder_fits_noextra = paste0(base_folder, "td_fits\\")
folder_imps_noextra = paste0(base_folder, "td_imps\\")

# performing simulations with n runs
# the warnings are caused by collinearity between the variables
# which is expected
options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  # walk will run the function for each missing proportion in the vector
  # without attempting to spit out a list (in comparison to map(), which will create a list or die trying)
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_noextra, folder_fits_noextra, folder_imps_noextra, rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., d_exdata_mar_noextra, folder_fits_noextra, folder_imps_noextra, rep = i))
}
options(warn=0)

#-------------- position available
folder_fits_pos = paste0(base_folder, "td_fits_pos\\")
folder_imps_pos = paste0(base_folder, "td_imps_pos\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_pos, folder_fits_pos, folder_imps_pos, rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., d_exdata_mar_pos, folder_fits_pos, folder_imps_pos, rep = i))
}
options(warn=0)

#-------------- player position and srpe

# performing simulations with n runs
# the warnings are caused by collinearity between the variables
# which is expected
folder_fits_srpe_pos = paste0(base_folder, "td_fits_srpe_pos\\")
folder_imps_srpe_pos = paste0(base_folder, "td_imps_srpe_pos\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_srpe_pos, folder_fits_srpe_pos, folder_imps_srpe_pos, rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., d_exdata_mar_srpe_pos, folder_fits_srpe_pos, folder_imps_srpe_pos, rep = i))
}
options(warn=0)

#----------------------------------------------To check if results are different whether all GPS variables have missing or not

# we need to change the functions that add missing to something
# that adds missing to all the gps variables at once
# Create missing completely at random
add_mcar_td = function(d, missing_prop){
  n_values = nrow(d)
  d = d %>% rownames_to_column()
  random_spots_td = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate_at(vars(gps_td, gps_v4, gps_v5, gps_pl), ~ifelse(rowname %in% random_spots_td, NA, .)) %>% dplyr::select(-rowname)
  d
}

# adding missing at random to a dataset
add_mar_td = function(d, corr){
  d = d %>% mutate(na_prop = mar_function(., corr),
                   na_spot_td = rbinom(length(na_prop), 1, prob = na_prop),
                   gps_td = ifelse(na_spot_td == 1, NA, gps_td),
                   gps_v4 = ifelse(na_spot_td == 1, NA, gps_v4),
                   gps_v5 = ifelse(na_spot_td == 1, NA, gps_v5),
                   gps_pl = ifelse(na_spot_td == 1, NA, gps_pl))
  d %>% dplyr::select(-starts_with("na"))
}


#----------- First for the variant with no extra variables
folder_fits_nogps_noextra = paste0(base_folder, "td_fits_nogps\\")
folder_imps_nogps_noextra = paste0(base_folder, "td_imps_nogps\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  # walk will run the function for each missing proportion in the vector
  # without attempting to spit out a list (in comparison to map(), which will create a list or die trying)
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_noextra, folder_fits_nogps_noextra, folder_imps_nogps_noextra, rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., d_exdata_mar_noextra, folder_fits_nogps_noextra, folder_imps_nogps_noextra, rep = i))
}
options(warn=0)

#------------ if all gPS are missing, but position is available
folder_fits_nogps_pos = paste0(base_folder, "td_fits_nogps_pos\\")
folder_imps_nogps_pos = paste0(base_folder, "td_imps_nogps_pos\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  # walk will run the function for each missing proportion in the vector
  # without attempting to spit out a list (in comparison to map(), which will create a list or die trying)
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_pos, folder_fits_nogps_pos, folder_imps_nogps_pos, rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., d_exdata_mar_pos, folder_fits_nogps_pos, folder_imps_nogps_pos, rep = i))
}
options(warn=0)


#----------- finally, if all gPS are missing, but position AND sRPE is available
folder_fits_nogps_srpe_pos = paste0(base_folder, "td_fits_nogps_srpe_pos\\")
folder_imps_nogps_srpe_pos = paste0(base_folder, "td_imps_nogps_srpe_pos\\")

options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1:n_sim){
  # walk will run the function for each missing proportion in the vector
  # without attempting to spit out a list (in comparison to map(), which will create a list or die trying)
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_srpe_pos, folder_fits_nogps_srpe_pos, folder_imps_nogps_srpe_pos, rep = i))
  missing_prop_mar %>% walk(~sim_impute("mar", ., d_exdata_mar_srpe_pos, folder_fits_nogps_srpe_pos, folder_imps_nogps_srpe_pos, rep = i))
}
options(warn=0)

#----------------------Sub analysis - does result change under single imputation of regression and pmm?-----------------------------------------

# Create missing completely at random
add_mcar_td = function(d, missing_prop){
  n_values = nrow(d)
  d = d %>% rownames_to_column()
  random_spots_td = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(gps_td = ifelse(rowname %in% random_spots_td, NA, gps_td)) %>% dplyr::select(-rowname)
  d
}

# adding missing at random to a dataset
add_mar_td = function(d, corr){
  d = d %>% mutate(na_prop = mar_function(., corr),
                   na_spot_td = rbinom(length(na_prop), 1, prob = na_prop),
                   gps_td = ifelse(na_spot_td == 1, NA, gps_td))
  d %>% dplyr::select(-starts_with("na"))
}

# we need to change the functions that add missing to something
# that adds missing to all the gps variables at once
# Create missing completely at random
add_mcar_td_allgps = function(d, missing_prop){
  n_values = nrow(d)
  d = d %>% rownames_to_column()
  random_spots_td = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate_at(vars(gps_td, gps_v4, gps_v5, gps_pl), ~ifelse(rowname %in% random_spots_td, NA, .)) %>% dplyr::select(-rowname)
  d
}

# adding missing at random to a dataset
add_mar_td_allgps = function(d, corr){
  d = d %>% mutate(na_prop = mar_function(., corr),
                   na_spot_td = rbinom(length(na_prop), 1, prob = na_prop),
                   gps_td = ifelse(na_spot_td == 1, NA, gps_td),
                   gps_v4 = ifelse(na_spot_td == 1, NA, gps_v4),
                   gps_v5 = ifelse(na_spot_td == 1, NA, gps_v5),
                   gps_pl = ifelse(na_spot_td == 1, NA, gps_pl))
  d %>% dplyr::select(-starts_with("na"))
}

get_params = function(fit, method, pool = FALSE){
  if(pool){
    d_params = summary(mice::pool(fit), "all", conf.int = TRUE) %>% 
      dplyr::select(term, std.error, estimate, CI_low = "2.5 %", CI_high = "97.5 %", p = p.value) %>% 
      as_tibble() %>% 
      mutate(term =  as.character(term))
  } else {
    d_params = summary(fit, conf.int = TRUE) %>% 
      dplyr::select(term, std.error, estimate, CI_low = "conf.low", CI_high = "conf.high", p = p.value) %>% 
      as_tibble() %>% 
      mutate(term =  as.character(term))
  }
  d_params = d_params %>% mutate(method = method) 
  d_params
}

# The final, helper function that performs all the imputations at once, given a dataset with missing
# then, it fits a logistic regression model on the data
# and outputs the needed model parameters for the validation of the imputation models
sim_impfit_singleimp = function(d_missing, rep = 1){
  
  #-----------------impute using all the different methods
  # Multiple Imputation - Regression imputation
  mids.reg.1 = mice(d_missing, method = "norm.predict", seed = 1234, m = 1, print = FALSE)
  mids.reg.5 = mice(d_missing, method = "norm.predict", seed = 1234, m = 10, print = FALSE)
  
  # Multiple Imputation - Predicted Mean Matching
  mids.pmm.1 = mice(d_missing, seed = 1234, m = 1, print = FALSE)
  mids.pmm.5 = mice(d_missing, seed = 1234, m = 10, print = FALSE)
  
  # fit our models
  fit.reg.1 =  with(mids.reg.1, glm(injury ~ gps_td, family = binomial))
  fit.pmm.1 =  with(mids.pmm.1, glm(injury ~ gps_td, family = binomial))
  fit.reg.5 =  with(mids.reg.5, glm(injury ~ gps_td, family = binomial))
  fit.pmm.5 =  with(mids.pmm.5, glm(injury ~ gps_td, family = binomial))
  
  # fetch model parameters
  tab1 = get_params(fit.reg.1, "SI - Regression Imputation", pool = FALSE)  
  tab2 = get_params(fit.pmm.1, "SI - PMM", pool = FALSE)  
  tab3 = get_params(fit.reg.5, "MI - Regression Imputation", pool = TRUE)  
  tab4 = get_params(fit.pmm.5, "MI - PMM", pool = TRUE)  
  
  d_fits = bind_rows(tab1, tab2, tab3, tab4)
  d_fits = d_fits %>% mutate(rep = rep)
  d_fits
}

sim_impute = function(missing, missing_amount, d, folder_fits, rep){
  name = as.character(substitute(d))
  
  if(missing == "mcar"){
    d_mcar = add_mcar_td(d, missing_amount)
    d_sim_fits_mcar = sim_impfit_singleimp(d_mcar, rep) %>% 
      mutate(missing_type = missing, 
             missing_amount = missing_amount,
             data_type = name)
    saveRDS(d_sim_fits_mcar, file=paste0(folder_fits, rep,"_d_td_fits_", name, "_", missing, "_", missing_amount,".rds"))  
  } else if(missing == "mar"){
    d_mar = add_mar_td(d, missing_amount)
    d_sim_fits_mar = sim_impfit_singleimp(d_mar, rep) %>% 
                     mutate(missing_type = missing, 
                            missing_amount = missing_amount,
                            data_type = name)
    saveRDS(d_sim_fits_mar, file=paste0(folder_fits, rep,"_d_td_fits_", name, "_", missing, "_", missing_amount,".rds"))
  }
}

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# base folder where all the folders for simulations are to be saved
base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_fits_singleimp = paste0(base_folder, "td_fits_singleimp\\")

# performing simulations with n runs
# the warnings are caused by collinearity between the variables
# which is expected
options(warn=-1)
set.seed = 1234
n_sim = 1900
for(i in 1793:n_sim){
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_noextra, folder_fits_singleimp, rep = i))
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_pos, folder_fits_singleimp, rep = i))
  missing_prop_mcar %>% walk(~sim_impute("mcar", ., d_exdata_td_srpe_pos, folder_fits_singleimp, rep = i))
}
options(warn=0)
