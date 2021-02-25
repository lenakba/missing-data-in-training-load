# this script is for calculating the amount of missing data
# at different levels and variants

library(tidyverse) # for datawrangling
library(readxl) # for reading .xlsx files
library(lmisc) # ggplot2 themes etc.
library(devEMF) # to save emf files

# reading data
folder_data = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")
d_tl_study_full = read_excel(paste0(folder_data, "studies_missing_reporting.xlsx"))

# selecting the variables needed for these analyses
key_vars = c("study_id", "author_first", "year", "title")
base_vars = c("sport", "n_athletes", "population_sex", "population_mean_age", "n_injuries", "load_variable")
d_tl_study = d_tl_study_full %>% select(all_of(key_vars), all_of(base_vars), starts_with("missing"))

# percentage that reported missing in the training load variable
d_tl_study %>% summarise(n_report = sum(missing_reported == "Yes", na.rm = TRUE), n_studies = n(), prop = n_report/n_studies)

# mean amount of missing
missing_n_vars = c("missing_load_perc", "missing_injury_perc", "missing_players")
d_tl_study %>% summarise_at(vars(all_of(missing_n_vars)), ~mean(., na.rm = TRUE))
d_tl_study %>% summarise_at(vars(all_of(missing_n_vars)), ~sd(., na.rm = TRUE))

# how many report how much missing?
d_tl_study %>% summarise(sum(!is.na(missing_load_perc)))

# function for making a dot plot for a consistent style
fig_dots = function(d, x, y, title, percent = FALSE){
  x = enquo(x)
  y = enquo(y)
  
  p = ggplot(d, aes(x = !!x, y = !!y)) +
    geom_point(size = 3) + 
    theme_dot() + 
    ggtitle(title) +
    xlab(NULL) +
    ylab(NULL)
  
  if(percent){
    p = p + scale_x_continuous(labels=axis_percent)
  }
  p
}

# Massaging data for a figure showing what missing imputation methods publications choose
# Remove 1 publication that dutifully reported not having any missing data (they don't need methods to handle them then, do they?)
d_missing = d_tl_study %>% filter(missing_reported == "Yes", study_id != 8)

d_missing_methods = d_missing %>% 
  count(missing_method) %>% 
  mutate(denominator = sum(n), prop = n/denominator) %>% 
  arrange(prop) %>% 
  mutate(mmethod_fct = fct_inorder(missing_method))
n_missing = nrow(d_missing)

 emf("n_missing_methods.emf", width = 6, height = 3)
fig_dots(d_missing_methods, prop, mmethod_fct, paste0("Methods used for handling missing data (n = ",n_missing,")"), percent = TRUE)
 dev.off()



# function for making a line-chart with a consistent style. 
fig_line = function(d, x, y, title, percent = FALSE){
  x = enquo(x)
  y = enquo(y)
  
  p = ggplot(d, aes(x = !!x, y = !!y, group = 1)) +
    geom_line(color = nih_distinct[2], size = 1) +
    geom_point(color = nih_distinct[2], size = 2) +
    theme_line() +
    ylab(NULL) +
    ggtitle(title) +
    xlab(NULL) +
    scale_x_continuous(breaks = breaks_width(2, 1))
  
  if(percent){
    p = p + scale_y_continuous(labels = axis_percent)
    p
  }
  p
}

# percentage studies that reported missing and assumptions each year
fig_line(d_dquality, year, prop, "Percentage studies reported missing", TRUE)

d_study = d_study %>% 
  mutate(n_over_200 = ifelse(n_injuries >= 200, 1, 0))

prop_above_200 = d_study %>% 
  mutate(n_over_200 = ifelse(n_injuries >= 200, 1, 0)) %>% 
  summarise(n = sum(n_over_200 == 1, na.rm = TRUE), denom = sum(n_over_200 == 1, na.rm = TRUE) + sum(n_over_200 == 0, na.rm = TRUE), prop = n/denom)

emf("n_injury_distribution.emf", width = 8, height = 4)
text_size = 16
ggplot(d_study, aes(x = n_injuries_outlierfix)) + 
  geom_histogram(binwidth = 30, fill = nih_distinct[4]) +
  geom_vline(xintercept = 200, color = nih_distinct[1], size = 1.5, alpha = 0.5) + 
  scale_x_continuous(breaks = breaks_width(100, 0)) + 
  scale_y_continuous(breaks = breaks_width(4, 0), expand = expand_bar) + 
  theme_line() +
  xlab("Number of Injuries") +
  ylab("Number of\nStudies") +
  theme(axis.text = element_text(size=text_size),
        axis.title =  element_text(size=text_size))
dev.off()