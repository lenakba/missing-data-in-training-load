# this script is for simulating missing data
# and compare different methods of imputation
library(tidyverse) # for datawrangling

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
d_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_anon.csv"), delim = ";")

# dividing into two datasets
# one where no one player and day is missing total distance
# one where no one player and day is missing srpe
# then we rename the td and srpe variable to the same name "load"
# because we want to use functions later on without having to differentiate between total distance and srpe later on
d_td_full = d_full %>% select(p_id, training_date, day_of_week, total_distance_daily) %>% rename(load = total_distance_daily)
d_srpe_full = d_full %>% select(p_id, training_date, day_of_week, load)

# now we remove missing until we have a dataset where we know 100% what the values are
d_td = d_td_full %>% filter(!is.na(load)) %>% rownames_to_column()
d_srpe = d_srpe_full %>% filter(!is.na(load)) %>% rownames_to_column()

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


