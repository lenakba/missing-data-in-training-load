# this script is for simulating missing data
# and compare different methods of imputation

library(tidyverse) # for datawrangling

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
d_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_anon.csv"), delim = ";")

# dividing into two datasets
# one where no one player and day is missing total distance
# one where no one player and day  is missing srpe
# then we rename the td and srpe variable to the same name "load"
# because we want to use functions later on without having to differentiate between total distance and srpe later on
d_td = d_full %>% select(p_id, training_date, total_distance_daily) %>% rename(load = total_distance_daily)
d_srpe = d_full %>% select(p_id, training_date, load)

