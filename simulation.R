# this script is for simulating missing data
# and compare different methods of imputation

library(tidyverse) # for datawrangling

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
read_delim(paste0(folder_data, "norwegian_premier_league_football_anon.csv"), delim = ";")

