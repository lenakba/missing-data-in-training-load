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

# Connecting to database
db = 'stromsgodset'  #provide the name of your db
db_port = '5432'  # or any other port specified by the DBA
db_user = "postgres" 
db_password = "postgresql"
db_stromsgodset = dbConnect(RPostgreSQL::PostgreSQL(), dbname = db, port=db_port, user=db_user, password=db_password) 

# get sRPE data from database
d_srpe_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.temp_training_log")) %>% as_tibble() 
d_srpe_selected = d_srpe_full %>% dplyr::select(player_id, training_date = planned_date, activity, duration, rpe = difficulty, srpe = load)

# some players have reported the activity "training" with a duration higher than 0, 
# yet rpe is set to 0, which means no training was performed
# we will assume these had a rpe of 1
# in addition, if activity = training, but duration = 0, we assume rpe and srpe to be 0 too
# we use the chron package to create duration into a numeric variable
d_srpe_fixed = d_srpe_selected %>% 
  mutate(times_tz = chron::times(duration), 
         minutes = chron::minutes(times_tz), 
         hours = chron::hours(times_tz),
         hours_in_minutes = hours*60,
         minutes_sum = hours_in_minutes+minutes,
         duration_min = minutes_sum,
         rpe = ifelse(activity == "Training" & rpe == 0 & duration_min > 0, 1, rpe),
         rpe = ifelse(activity == "Training" & duration_min == 0, 0, rpe),
         srpe = duration_min*rpe) %>% # we need to recalculate srpe thanks to that
  dplyr::select(player_id, training_date, activity, rpe, duration = duration_min, srpe)

# get baseline variables from excel sheet
folder = "O:\\Prosjekter\\Dalen-Lorentsen - Belastningsstyring - prosjekt 1 - metode\\Data\\stromsgodset\\raw_data\\"
d_player = read_excel(paste0(folder, "stromsgodset_players.xlsx")) %>% 
           mutate(age = as.numeric(difftime(min(d_srpe_fixed$training_date), birth_date, units = "days"))/365) %>% 
           dplyr::select(-name, -position_group, -birth_date)

d_srpe_baseline = d_srpe_fixed %>% left_join(d_player, by = c( "player_id" = "id"))


#-----------Adding days that were guaranteed free days (therefore not missing)
# We want to make sure that dates where the
# players didn't participate in any activity are included
# as these are implicitly sRPE = 0.
# unless any other information is given, 
# we will assume that the 2 days after a match, 
# given that the week has only 1 match, are free days with 0 TD for all players 

# obtaining information about dates
d_date_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.date_dimension")) %>% as_tibble()


# finding number of matches per week
# add an index with number of matches for each of the weeks
d_n_matches = d_date_full %>% group_by(week_nr) %>% summarise(n_matches = sum(match_indicator))
d_date_full = d_date_full %>% left_join(d_n_matches, by = "week_nr")

# Add categorical variable describing each day in relation to match days
# According to Torstein Dalen-Lorentsen the sequence:
# M
# M+1
# M+2
# m-4
# m-3
# m-2
# m-1
# M

# for weeks with 1 match
d_match_weeks = d_date_full %>% filter(n_matches == 1) 
d_match_weeks = d_match_weeks %>% mutate(mc_day = case_when(match_indicator ~ "M",
                                                            lag(match_indicator) ~ "M+1", 
                                                            lag(match_indicator, 2) ~ "M+2",
                                                            lead(match_indicator) ~ "M-1",
                                                            lead(match_indicator, 2) ~ "M-2",
                                                            lead(match_indicator, 3) ~ "M-3",
                                                            lead(match_indicator, 4) ~ "M-4")
)

# some days are missing because the week had the match placed on an untraditional day
# d_match_weeks %>% filter(is.na(mc_day))

# And for two matches per week or more:
# M
# m-2
# m-1
# M
# M-3
# m-2
# m-1
# M

d_match_weeks_dbl = d_date_full %>% filter(n_matches >= 2) 
d_match_weeks_dbl = d_match_weeks_dbl %>% mutate(mc_day = case_when(match_indicator ~ "M",
                                                                    lead(match_indicator) ~ "M-1",
                                                                    lead(match_indicator, 2) ~ "M-2",
                                                                    lead(match_indicator, 3) ~ "M-3",
                                                                    lead(match_indicator, 4) ~ "M-4",
                                                                    lead(match_indicator, 5) ~ "M-5")
)

# weeks without a match
d_free_weeks = d_date_full %>% filter(n_matches == 0) %>% mutate(mc_day = "Non-match week")

# combine data
d_weeks_full = bind_rows(d_free_weeks, d_match_weeks, d_match_weeks_dbl) %>% arrange(training_date)

# this is now the ready date dataset which we will combine later to get days without training
d_weeks = d_weeks_full %>% select(datekey, training_date, day_of_week, week_nr, match_indicator, n_matches, mc_day)

# adding match week day data to RPE data
d_srpe_dayinfo = d_srpe_baseline %>% full_join(d_weeks %>% dplyr::select(training_date, n_matches, mc_day), by = "training_date") 

# days that are M+1 or M+2 are rpe, duration and sRPE = 0
d_srpe_dayinfo  = d_srpe_dayinfo %>% mutate(rpe = ifelse(is.na(rpe) & (mc_day == "M+2" | mc_day == "M+1"), 0, rpe),
                            duration = ifelse(is.na(duration) & (mc_day == "M+2" | mc_day == "M+1"), 0, duration),
                            srpe = ifelse(is.na(srpe) & (mc_day == "M+2" | mc_day == "M+1"), 0, srpe))

# fill the baseline variables we know the answer to
d_srpe = d_srpe_dayinfo %>% arrange(player_id, training_date) %>% 
         group_by(player_id) %>% 
         fill(position, age, height) %>% ungroup() %>% filter(!is.na(player_id))


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
inj_probability = function(srpe, age){
  y = log_reg(-0.8 + 0.0003*srpe + (0.0003*age)) 
  y
}

# we create fake injuries, and the ideal logistic regression model
d_sim_inj = d_srpe %>% 
            mutate(inj_prop = inj_probability(srpe, age), 
                   injury = rbinom(length(inj_prop), 1, prob = inj_prop))

# now we make out example data that we'll remove data from
d_exdata = d_sim_inj %>% rownames_to_column() %>% dplyr::select(-srpe)
target_srpe = d_sim_inj$srpe

# we fetch our MCAR function from the main simulation
set.seed(123)
add_mcar = function(d, missing_prop){
  n_values = nrow(d)
  random_spots_rpe = sample(1:n_values, round(missing_prop*n_values))
  random_spots_min = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(rpe = ifelse(rowname %in% random_spots_rpe, NA, rpe),
                   duration = ifelse(rowname %in% random_spots_min, NA, duration)) %>% dplyr::select(-rowname)
  d
}

d_missing = add_mcar(d_exdata, 0.25) %>% dplyr::select(-inj_prop)

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
imp.jav = mice(d_missing_srpe, meth = meth_pmm, pred = pred, seed = 1234, print = TRUE)


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
fit_target = glm(injury ~ srpe + age, family = "binomial", data = d_sim_inj)
fit1 = with(imp.itt, glm(injury ~ srpe + age, family = binomial))
fit2 = with(imp.jav, glm(injury ~ srpe + age, family = binomial))
fit3 = with(imp.pas, glm(injury ~ srpe + age, family = binomial))
fit4 = with(imp.id, glm(injury ~ srpe + age, family = binomial))

# function for obtaining parameters from any model fit
# specify method for a column with the model-type
get_params = function(fit, method){
  d_params = parameters::parameters(fit) %>% tibble()
  d_params = d_params %>% mutate(method = method)
  d_params
}

tab_target = get_params(fit_target, "Target") %>% dplyr::select(estimate = Coefficient, CI_low, CI_high) %>% mutate(method = "No imputation")
tab1 = summary(mice::pool(fit1), "all", conf.int = TRUE) %>% dplyr::select(estimate, CI_low = "2.5 %", CI_high = "97.5 %") %>% mutate(method = "ITT")
tab2 = summary(mice::pool(fit2), "all", conf.int = TRUE) %>% dplyr::select(estimate, CI_low = "2.5 %", CI_high = "97.5 %") %>% mutate(method = "jav")
tab3 = summary(mice::pool(fit3), "all", conf.int = TRUE) %>% dplyr::select(estimate, CI_low = "2.5 %", CI_high = "97.5 %") %>% mutate(method = "pas")
tab4 = summary(mice::pool(fit4), "all", conf.int = TRUE) %>% dplyr::select(estimate, CI_low = "2.5 %", CI_high = "97.5 %") %>% mutate(method = "id")


## TODO Create function for estimating parameters

# true = 1
# RB = rowMeans(res1[,, "estimate"]) - true
# PB = 100 * abs((rowMeans(res[,, "estimate"]) - true)/ true)
# CR = rowMeans(res[,, "2.5 %"] < true & true < res[,, "97.5 %"])
# AW = rowMeans(res[,, "97.5 %"] - res[,, "2.5 %"])
# RMSE = sqrt(rowMeans((res[,, "estimate"] - true)^2))
# 
# data.frame(RB, PB, CR, AW, RMSE)

## TODO evaluate imputation points by themselves
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