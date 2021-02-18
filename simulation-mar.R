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

# note that the Total Distance data is per day, the RPE data is per session (which there can be multiple of per day)
d_td_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_td_anon.csv"), delim = ";")
d_rpe_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_rpe_anon.csv"), delim = ";")

# remove missing
# select vars we need in the simulation, key variables we think are correlated with the level of sRPE and total distance
keyvars = c("p_id", "training_date", "mc_day")
d_td = d_td_full %>% filter(!is.na(total_distance_daily)) %>% select(all_of(keyvars), td = total_distance_daily, srpe) 
d_srpe = d_rpe_full %>% filter(!is.na(rpe) & !is.na(duration)) %>% select(all_of(keyvars), rpe, duration) %>% mutate(srpe = rpe*duration)

#------------------------------------------Step 1 add fake injuries with known relationship with load variable

# logistic function
log_reg = function(tl_coef){
  res = 1 / (1 + exp(-tl_coef))
  res
}

# linear logistic regression function
inj_probability_srpe = function(srpe){
  y = log_reg(-2 + 0.003*srpe) 
  y
}

# we create fake injuries
d_srpe = d_srpe %>% 
  mutate(inj_prop = inj_probability_srpe(srpe), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# logistic regression for comparison
fit.target = glm(injury ~ srpe, family = "binomial", data = d_srpe)

#-----------------------------------------Step 2 Create data with missing

# remove variables we in theory wouldn't know about
# in a real life situation
d_exdata_srpe = d_srpe %>% dplyr::select(-inj_prop, -srpe)


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


d_missing_srpe5 = add_mcar_rpe(d_exdata_srpe, 0.05) 
d_missing_srpe20 = add_mcar_rpe(d_exdata_srpe, 0.2) 
d_missing_srpe80 = add_mcar_rpe(d_exdata_srpe, 0.8) 

# Missing probability is based on other variables (Missing at Random)

#----------------------------------------Step 3 Handle missing data in x number of ways

impute_median = function(var){
  med = median(var, na.rm = TRUE) 
  var_imp = ifelse(is.na(var), med, var)
  var_imp
}

# Mean imputation
mids.mean = mice(d_missing_srpe20, method = "mean", m = 1, maxit = 1)
d.mean = mice::complete(mids.mean, include = FALSE) %>% mutate(srpe = rpe*duration)

# Regression imputation
mids.reg = mice(d_missing_srpe20, method = "norm.predict", seed = 1234, m = 1, print = FALSE)
d.reg = mice::complete(mids.reg, "long", include = FALSE) %>% mutate(srpe = rpe*duration)

# Multiple imputation with predicted mean matching, and the Impute, then transform strategy
mids.pmm = mice(d_missing_srpe20, print = FALSE, seed = 1234)
imp.pmm = mice::complete(mids.pmm, "long", include = TRUE)
imp.pmm$srpe = with(imp.pmm, rpe*duration)
mids.itt.pmm = as.mids(imp.pmm)

# the next methods don't have MICE implementation
# Median imputation
d.median = d_missing_srpe20 %>% mutate(rpe = impute_median(rpe),
                                       duration = impute_median(duration),
                                       srpe = rpe*duration)
# complete case analysis
d.cc = na.omit(d_missing_srpe20) %>% mutate(srpe = rpe*duration)

#-----------------------------------------------------Step 4 fit our models

# fit our models
fit_glm = function(d){
  fit = glm(injury ~ srpe, family = "binomial", data = d)
  fit
}

fit.rdm =  fit_glm(d.rdm)
fit.mean = fit_glm(d.mean)
fit.reg =  fit_glm(d.reg)
fit.pmm =  with(mids.itt.pmm, glm(injury ~ srpe, family = binomial))
fit.median =  fit_glm(d.median)
fit.cc =  fit_glm(d.cc)

#----------------------------------------------------Step 6 fetch parameters

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

target_param = get_params(fit.target, "No Imputation")  

tab1 = get_params(fit.rdm, "Random Sampling Imputation")  
tab2 = get_params(fit.mean, "Mean Imputation")  
tab3 = get_params(fit.reg, "Regression Imputation")  
tab4 = get_params(fit.pmm, "Multiple Imputation", pool = TRUE)  
tab5 = get_params(fit.median, "Median Imputation")  
tab6 = get_params(fit.cc, "Complete Case Analysis")  

d_fits = bind_rows(target_param, tab1, tab2, tab3, tab4, tab5, tab6)
d_fits = d_fits %>% mutate(rep = run)
d_fits


#---------------------------------------------Step 7 create function that performs step 3 to 6 a user-chosen number of times
sim_impfit = function(d_missing, target_param, run = 1){

  # Imputing by random sampling
  # so we can compare methods to this as a baseline
  mids.rdm = mice(d_missing, method = "sample", m = 1, maxit = 1, print = FALSE)
  d.rdm = mice::complete(mids.rdm, include = FALSE) %>% mutate(srpe = rpe*duration)
  
  # Mean imputation
  mids.mean = mice(d_missing, method = "mean", m = 1, maxit = 1, print = FALSE)
  d.mean = mice::complete(mids.mean, include = FALSE) %>% mutate(srpe = rpe*duration)
  
  # Regression imputation
  mids.reg = mice(d_missing, method = "norm.predict", seed = 1234, m = 1, print = FALSE)
  d.reg = mice::complete(mids.reg, "long", include = FALSE) %>% mutate(srpe = rpe*duration)
  
  # Multiple imputation with predicted mean matching, and the Impute, then transform strategy
  mids.pmm = mice(d_missing, print = FALSE, seed = 1234)
  imp.pmm = mice::complete(mids.pmm, "long", include = TRUE)
  imp.pmm$srpe = with(imp.pmm, rpe*duration)
  mids.itt.pmm = as.mids(imp.pmm)
  
  # the next methods don't have MICE implementation
  # Median imputation
  d.median = d_missing %>% mutate(rpe = impute_median(rpe),
                                         duration = impute_median(duration),
                                         srpe = rpe*duration)
  # complete case analysis
  d.cc = na.omit(d_missing) %>% mutate(srpe = rpe*duration)
  
  # fit our models
  fit.rdm =  fit_glm(d.rdm)
  fit.mean = fit_glm(d.mean)
  fit.reg =  fit_glm(d.reg)
  fit.pmm =  with(mids.itt.pmm, glm(injury ~ srpe, family = binomial))
  fit.median =  fit_glm(d.median)
  fit.cc =  fit_glm(d.cc)
  
  # fetch model parameters
  tab1 = get_params(fit.rdm, "Random Sampling Imputation")  
  tab2 = get_params(fit.mean, "Mean Imputation")  
  tab3 = get_params(fit.reg, "Regression Imputation")  
  tab4 = get_params(fit.pmm, "Multiple Imputation", pool = TRUE)  
  tab5 = get_params(fit.median, "Median Imputation")  
  tab6 = get_params(fit.cc, "Complete Case Analysis")  
  
  d_fits = bind_rows(target_param, tab1, tab2, tab3, tab4, tab5, tab6)
  d_fits = d_fits %>% mutate(rep = run)
  d_fits
}

# we create fake injuries
d_srpe = d_srpe %>% 
  mutate(inj_prop = inj_probability_srpe(srpe), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# logistic regression for comparison
fit.target = glm(injury ~ srpe, family = "binomial", data = d_srpe)
target_param = get_params(fit.target, "No Imputation")  

# remove variables we in theory wouldn't know about
# in a real life situation
d_exdata_srpe = d_srpe %>% dplyr::select(-inj_prop, -srpe)

# this is what will go in the for-loop:
d_missing_srpe20 = add_mcar_rpe(d_exdata_srpe, 0.8)
sim_impfit(d_missing_srpe20, target_param, 1)


#----------------------------------------Same for Total distance


# linear logistic regression function
inj_probability_td = function(td){
  y = log_reg(-2 + 0.0003*td) 
  y
}




d_td = d_td %>% 
  mutate(inj_prop = inj_probability_td(td), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

d_exdata_td = d_td %>% dplyr::select(-inj_prop)








# vector of chosen missing proportions
missing_prop_v = c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8)

#-------------------------MCAR

# list with multiple datasets we add missing to
l_td = list(d_td, d_td, d_td, d_td, d_td, d_td)
l_srpe = list(d_srpe, d_srpe, d_srpe, d_srpe, d_srpe, d_srpe)

# function for adding missing completely at random with user's choice of proportion missing
set.seed(123)
add_mcar = function(d, missing_prop){
  n_values = nrow(d)
  random_spots = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(load = ifelse(rowname %in% random_spots, NA, load),
                   missing_type = "MCAR",
                   missing_amount = missing_prop)
  d
}

# use map2 to map each dataset with each element in vector missing_prop_v
l_td = l_td %>% map2(.x =., .y = missing_prop_v, ~add_mcar(.x, .y))
d_srpe = d_srpe %>% map2(.x =., .y = missing_prop_v, ~add_mcar(.x, .y))

#------------------------MAR

# logistic function
log_reg = function(tl_coef){
  res = 1 / (1 + exp(-tl_coef))
  res
}

# linear logistic regression function
mar_function = function(age, sex, weekend){
  y = log_reg(0.005 + (0.03*age) + (0.02*sex) + (0.3*weekend))
  y
}

# we add fake age and sex, and we use the day of the week to determine weekend
# we'll use these variables to create Missing at Random, but we'll remove
# the weekend variable later on, so that all we have is training data
# that the imputation method can use
td_p_id = d_td %>% distinct(p_id) 
td_p_id_base = td_p_id %>% mutate(age = sample(18:30, length(td_p_id$p_id), replace = TRUE),
                                  sex = sample(0:1, length(td_p_id$p_id), replace = TRUE))
d_td = d_td %>% 
       left_join(td_p_id_base, by = "p_id") %>% 
       mutate(weekend = ifelse(day_of_week == "Saturday" | day_of_week == "Sunday", 1, 0)) %>% select(-day_of_week)

d_td = d_td %>% mutate(coefs_mar = mar_function(age, sex, weekend))

# function that simulates injuries based on simulated longitudinal correlations
sim_long_corr = function(d, clsize, formula){# Simulation of correlated binary responses
  formula = enquo(formula)
  
  # Define the marginal risk [Log-odds scale]
  logit = function(x) log(x/(1-x))
  d_formula = d %>% mutate(formula_logit = logit(!!formula))
  FUN = d_formula %>% pull(formula_logit)
  
  # function for creating a covariance matrix with autoregressive correlation
  # https://i.stack.imgur.com/I3uwR.jpg 
  # values closer in time are more correlated than those further away in time
  ar1_cor = function(n, rho = 0.8) {
    exponent = abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                     (1:n - 1))
    rho^exponent
  }
  
  #autoregressive covariance matrix
  matrix = ar1_cor(clsize)
  
  d_sim_long = SimCorMultRes::rbin(
    # Number of repeated obs. per athlete
    clsize = clsize,
    # Formula for the marginal risk model
    xformula = ~FUN, 
    # Intercept for marginal risk
    intercepts = 0,
    # Coefficents for marginal risk
    betas = 1,
    # Correlation matrix for response
    cor.matrix = matrix,
    # Link function
    link = "logit"
  )
  d_sim = d_sim_long$simdata %>% tibble() %>% dplyr::select(y)
  d_sim
}

n_athletes_td = 38
clsize_td = nrow(d_td)/n_athletes_td

d_td = d_td %>% mutate(missing_spot = sim_long_corr(d_td, clsize_td, coefs_mar)$y) 


