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

set.seed(123)
add_mcar = function(d, missing_prop){
  n_values = nrow(d)
  random_spots = sample(1:n_values, round(missing_prop*n_values))
  d = d %>% mutate(load = ifelse(rowname %in% random_spots, NA, load),
                   missing_type = "MCAR")
  d
}

d_td_005 = add_mcar(d_td, 0.05)
d_td_01 = add_mcar(d_td, 0.1)
d_td_08 = add_mcar(d_td, 0.8)

d_td_005 %>% summarise(missing = sum(is.na(load)), n = n(), prop = missing/n)
d_td_01 %>% summarise(missing = sum(is.na(load)), n = n(), prop = missing/n)
d_td_08 %>% summarise(missing = sum(is.na(load)), n = n(), prop = missing/n)

