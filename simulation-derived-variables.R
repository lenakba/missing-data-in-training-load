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

# Connecting to database
db = 'stromsgodset'  #provide the name of your db
db_port = '5432'  # or any other port specified by the DBA
db_user = "postgres" 
db_password = "postgresql"
db_stromsgodset = dbConnect(RPostgreSQL::PostgreSQL(), dbname = db, port=db_port, user=db_user, password=db_password) 

# get data from database
d_srpe_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.temp_training_log")) %>% as_tibble() 
d_srpe_selected = d_srpe_full %>% select(player_id, training_date = planned_date, activity, duration, difficulty, load)

# some players have reported the activity "training" with a duration higher than 0, 
# yet difficulty is set to 0, which means no training was performed
# we will assume these had a difficulty of 1
# in addition, if activity = training, but duration = 0, we assume difficulty and load to be 0 too
# we use the chron package to create duration into a numeric variable
d_srpe_fixed = d_srpe_selected %>% 
  mutate(times_tz = chron::times(duration), 
         minutes = chron::minutes(times_tz), 
         hours = chron::hours(times_tz),
         hours_in_minutes = hours*60,
         minutes_sum = hours_in_minutes+minutes,
         duration_min = minutes_sum,
         difficulty = ifelse(activity == "Training" & difficulty == 0 & duration_min > 0, 1, difficulty),
         difficulty = ifelse(activity == "Training" & duration_min == 0, 0, difficulty),
         load = duration_min*difficulty) %>% # we need to recalculate srpe thanks to that
  select(player_id, training_date, rpe = difficulty, duration = duration_min, srpe = load)

#--------------------------------------------------adding missing

# we fetch our MCAR function from the main simulation
set.seed(123)
add_mcar = function(d, missing_prop){
  n_values = nrow(d)
  random_spots = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(load = ifelse(rowname %in% random_spots, NA, load),
                   missing_type = "MCAR",
                   missing_amount = missing_prop)
  d
}



#-------------------------------------------------imputation methods in Van Buuren

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

#--------------------------------------------------------------Testing imputation

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

# Evaluation of parameter for whr with 25% MCAR missing in hgt 
# and 25% MCAR missing in wgt using four imputation strategies (nsim = 200).





