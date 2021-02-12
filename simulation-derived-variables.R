# this script is for simulating missing data
# and compare different methods of imputation
library(tidyverse) # for datawrangling

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
d_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_anon.csv"), delim = ";")

# dividing into two datasets
# one where no one player and day is missing total distance
# one where no one player and day is missing srpe
d_srpe_full = d_full %>% select(p_id, training_date, day_of_week, rpe = difficulty, duration, srpe = load)
