# this script is for calculating the amount of missing data
# at different levels and variants

library(tidyverse) # for datawrangling

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
d_load_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_anon.csv"), delim = ";")

# missing can now be calculated
d_load_full %>% count(missing_td, missing_td_text)
d_load_full %>% count(missing_load, missing_load_text)