# this script is for calculating the amount of missing data
# at different levels and variants

library(tidyverse) # for datawrangling
library(readxl) # for reading .xlsx files

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
d_tl_study_full = read_excel(paste0(folder_data, "studies_missing_reporting.xlsx"))
