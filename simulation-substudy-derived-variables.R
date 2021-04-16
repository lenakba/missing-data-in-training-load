# one question that is unsolved is how to impute so-called 
# derived variables
# here, we try to find out the best method for sRPE

# may be relevant: https://journals.sagepub.com/doi/full/10.1177/0962280214521348
# definitely relevant: https://stefvanbuuren.name/fimd/sec-knowledge.html 

# loading packages
library(tidyverse) # for datawrangling
library(DBI) # for database extraction with SQL
library(chron) # for calculating duration
library(mice) # multiple imputation package
library(readxl) # reading excel files

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


#---------------------------Testing Imputation: creating fake injuries and see which method has the least bias etc

# Evaluation of parameter for whr with 25% MCAR missing in hgt 
# and 25% MCAR missing in wgt using four imputation strategies (nsim = 200).

# let's try everything with our srpe dataset

# logistic function
log_reg = function(tl_coef){
  res = 1 / (1 + exp(-tl_coef))
  res
}

# linear logistic regression function
inj_probability = function(srpe){
  y = log_reg(-2 + 0.003*srpe) 
  y
}

#-------------------------------1 permutation

# we fetch our MCAR function from the main simulation
add_mcar = function(d, missing_prop){
  n_values = nrow(d)
  random_spots_rpe = sample(1:n_values, round(missing_prop*n_values))
  random_spots_min = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(rpe = ifelse(rowname %in% random_spots_rpe, NA, rpe),
                   duration = ifelse(rowname %in% random_spots_min, NA, duration)) %>% dplyr::select(-rowname)
  d
}

# d_missing = add_mcar(d_exdata, 0.25) %>% dplyr::select(-inj_prop)
# 
# # Method 1 Impute, then transform
# imp1 = mice(d_missing, print = FALSE, seed = 1234)
# long1 = mice::complete(imp1, "long", include = TRUE)
# long1$srpe = with(long1, rpe*duration)
# imp.itt = as.mids(long1)
# 
# # Method 2 Transform, then impute
# d_missing_srpe = d_missing %>% mutate(srpe = rpe * duration)
# # We may prevent automatic removal by setting the relevant entries 
# # in the predictorMatrix to zero.
# # This is a little faster (5-10%) and cleans out the warning.
# pred = make.predictorMatrix(d_missing_srpe)
# pred[c("rpe", "srpe"), c("rpe", "srpe")] = 0
# pred[c("duration", "srpe"), c("duration", "srpe")] = 0
# meth_pmm = make.method(d_missing_srpe)
# imp.jav = mice(d_missing_srpe, meth = meth_pmm, pred = pred, seed = 1234, print = TRUE)
# 
# # Method 3 Passive Imputation
# meth = make.method(d_missing_srpe)
# meth["srpe"] = "~I(rpe*duration)" # we add the calculation for sRPE among the methods
# pred2 = make.predictorMatrix(d_missing_srpe)
# pred2[c("rpe", "duration"), "srpe"] = 0
# imp.pas = mice(d_missing_srpe, meth = meth, pred = pred2, print = FALSE, seed = 1234)
# 
# # Method 4 Impute derived without informants
# d_missing_srpe_only = d_missing_srpe %>% dplyr::select(-rpe, -duration)
# imp.id = mice(d_missing_srpe_only, seed = 1234, print = FALSE) 
# 
# # fit our models
# fit_target = glm(injury ~ srpe + age, family = "binomial", data = d_sim_inj)
# fit1 = with(imp.itt, glm(injury ~ srpe + age, family = binomial))
# fit2 = with(imp.jav, glm(injury ~ srpe + age, family = binomial))
# fit3 = with(imp.pas, glm(injury ~ srpe + age, family = binomial))
# fit4 = with(imp.id, glm(injury ~ srpe + age, family = binomial))

# function for obtaining parameters from any model fit
# specify method for a column with the model-type
# and whether its a fit on a list of imputed data or not
get_params = function(fit, method, imp = TRUE){
  if(imp){
  d_params = summary(mice::pool(fit), "all", conf.int = TRUE) %>% 
          dplyr::select(term, estimate, CI_low = "2.5 %", CI_high = "97.5 %") %>% 
          as_tibble() %>% 
          mutate(term =  as.character(term))
  } else {
  d_params = parameters::parameters(fit) %>% tibble() %>% 
          dplyr::select(term = Parameter, estimate = Coefficient, CI_low, CI_high)
  }
  d_params = d_params %>% mutate(method = method) 
  d_params
}

# tab_target = get_params(fit_target, "No imputation", imp = FALSE)  
# tab1 = get_params(fit1, "Impute then transform")  
# tab2 = get_params(fit2, "Transform then impute")  
# tab3 = get_params(fit3, "Passive Imputation")  
# tab4 = get_params(fit4, "Impute transformed alone")  
# d_fits = bind_rows(tab_target, tab1, tab2, tab3, tab4)

# create function based on all this
sim_impfit_derivedvar = function(d_missing, run = 1){
    target_srpe = d_missing$srpe

    # Method 1 Impute, then transform
    imp1 = mice(d_missing, print = FALSE, seed = 1234)
    long1 = mice::complete(imp1, "long", include = TRUE)
    long1$srpe = with(long1, rpe*duration)
    imp.itt = as.mids(long1)

    # Method 2 Transform, then impute
    d_missing_srpe = d_missing %>% mutate(srpe = rpe * duration)
    # We may prevent automatic removal by setting the relevant entries 
    # in the predictorMatrix to zero.
    # This is a little faster (5-10%) and cleans out the warning.
    pred = make.predictorMatrix(d_missing_srpe)
    pred[c("rpe", "srpe"), c("rpe", "srpe")] = 0
    pred[c("duration", "srpe"), c("duration", "srpe")] = 0
    meth_pmm = make.method(d_missing_srpe)
    imp.jav = mice(d_missing_srpe, meth = meth_pmm, pred = pred, seed = 1234, print = FALSE)

    # Method 3 Passive Imputation
    meth = make.method(d_missing_srpe)
    meth["srpe"] = "~I(rpe*duration)" # we add the calculation for sRPE among the methods
    pred2 = make.predictorMatrix(d_missing_srpe)
    pred2[c("rpe", "duration"), "srpe"] = 0
    imp.pas = mice(d_missing_srpe, meth = meth, pred = pred2, print = FALSE, seed = 1234)

    # Method 4 Impute derived without informants
    d_missing_srpe_only = d_missing_srpe %>% dplyr::select(-rpe, -duration)
    imp.id = mice(d_missing_srpe_only, seed = 1234, print = FALSE) 

    # fit our models
    fit_target = glm(injury ~ srpe, family = "binomial", data = d_sim_inj)
    fit1 = with(imp.itt, glm(injury ~ srpe, family = binomial))
    fit2 = with(imp.jav, glm(injury ~ srpe, family = binomial))
    fit3 = with(imp.pas, glm(injury ~ srpe, family = binomial))
    fit4 = with(imp.id, glm(injury ~ srpe, family = binomial))

    tab_target = get_params(fit_target, "No imputation", imp = FALSE)  
    tab1 = get_params(fit1, "Impute then transform")  
    tab2 = get_params(fit2, "Transform then impute")  
    tab3 = get_params(fit3, "Passive imputation")  
    tab4 = get_params(fit4, "Impute transformed alone")  

    d_fits = bind_rows(tab_target, tab1, tab2, tab3, tab4)
    d_fits = d_fits %>% mutate(rep = run)
    d_fits
}

# d_missing = add_mcar(d_exdata, 0.25) %>% dplyr::select(-inj_prop)
# test1 = sim_impfit_derivedvar(d_missing)

# function for adding the target srpe values
# to an imputed dataset
add_target_imp = function(d, imp_rows_pos, target, method){
  d = d %>% rownames_to_column() %>%
    mutate(imp_place = ifelse(rowname %in% imp_rows_pos, 1, 0),
           target = target,
           method = method) %>%
    dplyr::select(-rowname)
  d
}

# create function based on all this
sim_imp_derivedvar = function(d_missing, target, run = 1){
  
  # Method 1 Impute, then transform
  imp1 = mice(d_missing, print = FALSE, seed = 1234)
  long1 = mice::complete(imp1, "long", include = TRUE)
  long1$srpe = with(long1, rpe*duration)
  imp.itt = as.mids(long1)
  
  # Method 2 Transform, then impute
  d_missing_srpe = d_missing %>% mutate(srpe = rpe * duration)
  # We may prevent automatic removal by setting the relevant entries 
  # in the predictorMatrix to zero.
  # This is a little faster (5-10%) and cleans out the warning.
  pred = make.predictorMatrix(d_missing_srpe)
  pred[c("rpe", "srpe"), c("rpe", "srpe")] = 0
  pred[c("duration", "srpe"), c("duration", "srpe")] = 0
  meth_pmm = make.method(d_missing_srpe)
  imp.jav = mice(d_missing_srpe, meth = meth_pmm, pred = pred, seed = 1234, print = FALSE)
  
  # Method 3 Passive Imputation
  meth = make.method(d_missing_srpe)
  meth["srpe"] = "~I(rpe*duration)" # we add the calculation for sRPE among the methods
  pred2 = make.predictorMatrix(d_missing_srpe)
  pred2[c("rpe", "duration"), "srpe"] = 0
  imp.pas = mice(d_missing_srpe, meth = meth, pred = pred2, print = FALSE, seed = 1234)
  
  # Method 4 Impute derived without informants
  d_missing_srpe_only = d_missing_srpe %>% dplyr::select(-rpe, -duration)
  imp.id = mice(d_missing_srpe_only, seed = 1234, print = FALSE) 
  
  # create imputed datasets
  imp_rows_pos = which(is.na(d_missing$rpe) | is.na(d_missing$duration))
  d_imputed1 = mice::complete(imp.itt, "all") %>% 
               map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "Impute then transform")) %>% 
               imap(., ~mutate(., dataset_n = .y)) %>% 
               bind_rows() %>% filter(dataset_n != 0) #unimputed datasetis included in the ITT method
  d_imputed2 = mice::complete(imp.jav, "all") %>% 
               map(. %>% add_target_imp(., imp_rows_pos, target = target, method =  "Transform then impute")) %>% 
               imap(., ~mutate(., dataset_n = .y)) %>% 
               bind_rows()
  d_imputed3 = mice::complete(imp.pas, "all") %>% 
               map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "Passive imputation")) %>% 
               imap(., ~mutate(., dataset_n = .y)) %>% 
               bind_rows()
  d_imputed4 = mice::complete(imp.id, "all") %>% 
               map(. %>% add_target_imp(., imp_rows_pos, target = target, method = "Impute transformed alone")) %>% 
               imap(., ~mutate(., dataset_n = .y)) %>% 
               bind_rows()
  d_imp = bind_rows(d_imputed1, d_imputed2, d_imputed3, d_imputed4) %>% tibble()
  d_imp = d_imp %>% mutate(rep = run)
  d_imp 
}

# target_srpe = d_sim_inj$srpe
# test = sim_imp_derivedvar(d_missing, target_srpe)

# performing simulations with n runs
# the warnings are caused by collinearity between the variables
# which is expected
base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"
folder_fits = paste0(base_folder, "substudy_derived_var_fits\\")
folder_imps = paste0(base_folder, "substudy_derived_var_imps\\")

# we create fake injuries
d_sim_inj = d_srpe %>% 
  mutate(inj_prop = inj_probability(srpe), 
         injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# now we make out example data that we'll remove data from
d_exdata = d_sim_inj %>% rownames_to_column() %>% dplyr::select(-srpe, -inj_prop)
target_srpe = d_sim_inj$srpe

options(warn=-1)
set.seed = 1234
runs = 1900
target_srpe = d_sim_inj$srpe
for(i in 1:runs) {
  d_missing = add_mcar(d_exdata, 0.25)
  d_sim_fits = sim_impfit_derivedvar(d_missing, run = i)
  d_sim_imps = sim_imp_derivedvar(d_missing, target_srpe, run = i)
  saveRDS(d_sim_fits, file=paste0(folder_fits, i,"_d_derived_var_fits.rds"))
  saveRDS(d_sim_imps, file=paste0(folder_imps, i,"_d_derived_var_imps.rds"))
}
options(warn=0)

#-------------------------------------------------imputation methods in Van Buuren (for reference)

# The easiest way to deal with the problem is to leave any derived data outside the imputation process. 
# The approach is known as Impute, then transform (Von Hippel 2009). https://journals.sagepub.com/doi/abs/10.1111/j.1467-9531.2009.01215.x
# It is simple to do that in mice by
data = boys[, c("age", "hgt", "wgt", "hc", "reg")]
imp = mice(data, print = FALSE, seed = 71712)
long = mice::complete(imp, "long", include = TRUE)
long$whr = with(long, 100 * wgt / hgt)
imp.itt = as.mids(long)

# Another possibility is to create whr before imputation, 
# and impute whr as just another variable, 
# known as JAV (White, Royston, and Wood 2011), https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.4067,
# or under the name Transform, then impute (Von Hippel 2009), https://journals.sagepub.com/doi/abs/10.1111/j.1467-9531.2009.01215.x
# This is easy to do, as follows:
data$whr = 100 * data$wgt / data$hgt
imp.jav1 = mice(data, seed = 32093, print = FALSE)

# We may prevent automatic removal by setting the relevant entries 
# in the predictorMatrix to zero.
# This is a little faster (5-10%) and cleans out the warning.
pred = make.predictorMatrix(data)
pred[c("wgt", "whr"), c("wgt", "whr")] = 0
pred[c("hgt", "whr"), c("hgt", "whr")] = 0
imp.jav2 = mice(data, pred = pred, seed = 32093, print = FALSE)

# A third approach is Passive imputation, 
# where the transformation is done on-the-fly within the imputation algorithm. 
# Since the transformed variable is available for imputation, 
# the hope is that passive imputation removes the bias of the 
# Impute, then transform methods, 
# while restoring consistency among the imputations that was broken in JAV. 
# In mice passive imputation is invoked by specifying the tilde symbol as the first character of the imputation method. 
# This provides a simple method for specifying dependencies among the variables, 
# such as transformed variables, recodes, interactions, sum scores and so on. 
# In the above example, we invoke passive imputation by
data = boys[, c("age", "hgt", "wgt", "hc", "reg")]
data$whr = 100 * data$wgt / data$hgt
meth = make.method(data)
meth["whr"] = "~I(100 * wgt / hgt)"
pred = make.predictorMatrix(data)
pred[c("wgt", "hgt"), "whr"] = 0
imp.pas = mice(data, meth = meth, pred = pred, print = FALSE, seed = 32093)

# Bartlett et al. (2015) proposed a novel rejection sampling method 
# that creates imputations that are congenial in the sense of Meng (1994) 
# with the substantive (complete-data) model.
# The method has been implemented in the smcfcs package. 
# The imputation method requires a specification of the complete-data model, 
# as arguments smtype and smformula. 
# An example of how to generate imputations, fit models, and pool the results is:
# library(smcfcs)
# data = pop
# data[sample(nrow(data), size = 100), "wgt"] = NA
# data[sample(nrow(data), size = 100), "hgt"] = NA
# data$whr = 100 * data$wgt / data$hgt
# meth = c("", "norm", "norm", "", "", "norm")
# imps = smcfcs(originaldata = data, meth = meth, smtype = "lm",
#                smformula = "hc ~ age + hgt + wgt + whr")
# fit = lapply(imps$impDatasets, lm,
#               formula = hc ~ age + hgt + wgt + whr)
# summary(pool(fit))

# As the population data, take the 681 complete records of variables age, hgt, wgt, hc and reg, 
# and create a model for predicting height circumference from hc from more easily measured variables, 
# including whr.
pop <- na.omit(boys[, c("age", "hgt", "wgt", "hc", "reg")])
pop$whr <- with(pop, 100 * wgt / hgt)
broom::tidy(lm(hc ~ age + hgt + wgt + whr, data = pop))

# This is a simple linear model, 
# but the proportion of explained variance is very high, about 0.9. 
# The ratio variable whr explains about 5% of the variance on top of the other variables. 
# Let us randomly delete 25% of hgt and 25% of wgt, apply each of the three methods 200 times using 
# m = 5, and evaluate the parameter for whr.