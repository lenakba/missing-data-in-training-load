# this script is for calculating the amount of missing data
# at different levels and variants

library(tidyverse) # for datawrangling
library(readxl) # for reading .xlsx files
library(devEMF) # to save emf files
library(scales) # manipulating numeric scales

#-----------------------------------------------------Data preparation

# reading data
folder_data = paste0("my\\data\\location")
d_tl_study_full = read_excel(paste0(folder_data, "studies_missing_reporting.xlsx"))

# selecting the variables needed for these analyses
key_vars = c("study_id", "author_first", "year", "title")
base_vars = c("sport", "population_sex", "population_mean_age", "load_variable")
d_tl_study_selected = d_tl_study_full %>% select(all_of(key_vars), all_of(base_vars), starts_with("study_length"), starts_with("n_"), starts_with("missing"), starts_with("review_"))

# number of studies initially
nrow(d_tl_study_selected)

# study year >= 2010
d_tl_study = d_tl_study_selected %>% filter(year >= 2010)

#-------------------------------------------- Study characteristics
# percentage that reported the number of injuries in the study
d_tl_study %>% summarise(n_report = sum(!is.na(n_injuries_analyses), na.rm = TRUE), n_studies = n(), prop = n_report/n_studies)
d_tl_study %>% summarise(mean_age = mean(population_mean_age, na.rm = TRUE), sd_age = sd(population_mean_age, na.rm = TRUE))

# mean injuries
d_tl_study %>% filter(!is.na(n_injuries_analyses)) %>% summarise(mean = mean(n_injuries_analyses), 
                                                                 sd = sd(n_injuries_analyses),
                                                                 sum_target = sum(n_injuries_analyses >= 200),
                                                                 denom = n(),
                                                                 prop = sum_target/denom)

calc_perc = function(x, d = d_tl_study){
  x = enquo(x)
  d %>% count(!!x) %>% mutate(denominuator = sum(n), prop = n/denominuator) %>% arrange(desc(prop))
}

calc_perc(load_variable)
calc_perc(population_sex)
calc_perc(sport)

d_tl_study = d_tl_study %>% mutate(study_length_cat = ifelse(study_length_metric == "season" | study_length_metric == "year" | study_length_metric == "school year", "season/year/school year", "Other metrics"))
d_tl_study %>% group_by(study_length_cat) %>% calc_perc(study_length_n, .)
#-------------------------------------------- Missing data reporting

# percentage that reported missing in the training load variable
calc_perc(missing_reported_in_tl_variable)

# mean amount of missing
missing_n_vars = c("missing_load_perc", "missing_injury_perc", "missing_elligble_players_perc")
d_tl_study %>% filter(missing_reported_in_tl_variable == "Yes") %>% summarise_at(vars(all_of(missing_n_vars)), ~mean(., na.rm = TRUE))
d_tl_study %>% filter(missing_reported_in_tl_variable == "Yes") %>% summarise_at(vars(all_of(missing_n_vars)), ~sd(., na.rm = TRUE))

# how many report how much missing?
d_tl_study %>% summarise(n_how_much_missing = sum(!is.na(missing_load_perc)), n_studies = n(), prop = n_how_much_missing/n_studies)
d_tl_study %>% filter(missing_reported_in_tl_variable == "Yes") %>% summarise(n_how_much_missing_player = sum(!is.na(missing_elligble_players_perc)), n_studies = n(), prop = n_how_much_missing_player/n_studies)
d_tl_study %>% filter(missing_reported_in_tl_variable == "Yes") %>% summarise(n_how_much_missing_inj = sum(!is.na(missing_injury_perc)), n_studies = n(), prop = n_how_much_missing_inj/n_studies)


d_tl_study %>% filter(!is.na(missing_load_perc)) %>% count(load_variable)

# Massaging data for a figure showing what missing imputation methods publications choose
# Remove 1 publication that dutifully reported not having any missing data (they don't need methods to handle them then, do they?)
d_missing = d_tl_study %>% filter(missing_reported_in_tl_variable == "Yes", study_id != 8)

d_missing_methods = d_missing %>% 
  count(missing_method) %>% 
  mutate(denominator = sum(n), prop = n/denominator) %>% 
  arrange(prop) %>% 
  mutate(mmethod_fct = fct_inorder(missing_method))
n_missing = nrow(d_missing)

d_missing_methods %>% arrange(desc(prop))

#-----------------------------------figure - is %-reported missing stable the last few years?

d_tl_study_before_2021 = d_tl_study %>% filter(year <= 2020)
miss_per_year = d_tl_study_before_2021 %>% group_by(year) %>% 
  summarise(numerator = sum(missing_reported_in_tl_variable == "Yes"), denom = n(), prop = numerator/denom) %>% ungroup()
miss_per_year = miss_per_year %>% mutate(labels = paste0("(n = ", denom,")"))


plot_reporting_missing = ggplot(miss_per_year, aes(x = year, y = prop, label = labels)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  scale_y_continuous(labels = axis_percent, breaks = scales::breaks_width(0.1, 0)) + 
  scale_x_continuous(breaks = scales::breaks_width(2, 0)) +
  xlab(NULL) +
  ylab("% Studies") +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())


# Create a table plot
library(gridExtra)
# add 2011 to the denominators
d_2011 = tribble(~year, ~denom, 2011, 0)
miss_per_year_2011 = miss_per_year %>% bind_rows(., d_2011) %>% arrange(year)
labels = miss_per_year_2011 %>% select(denom) %>% rename('n studies' = denom)
labels_t = t(labels)

# make table to a figure with tableGrob
tbl <- tableGrob(labels_t, theme = ttheme_minimal(base_size = 14, base_family = "Trebuchet MS"), cols = NULL)
tbl$widths <- unit(rep(1/ncol(tbl), ncol(tbl)), "npc")

# save table and figure combined
emf("nstudy_missing_reporting_time.emf", width = 10, height = 5)
grid.arrange(plot_reporting_missing, 
             tbl,
             heights=c(3,1)
)
dev.off()
# ----------------------------------figure on number of injuries per study
d_tl_study = d_tl_study %>% 
  mutate(n_over_200 = ifelse(n_injuries_analyses >= 200, 1, 0))

prop_above_200 = d_tl_study %>%  
  summarise(n = sum(n_over_200 == 1, na.rm = TRUE), 
            denom = sum(n_over_200 == 1, na.rm = TRUE) + 
                    sum(n_over_200 == 0, na.rm = TRUE), 
            prop = n/denom)

d_tl_study = d_tl_study %>% mutate(n_injuries_outlierfix = ifelse(n_injuries_analyses >= 1000, 1000, n_injuries_analyses))


emf("n_injury_distribution.emf", width = 8, height = 4)
ggplot(d_tl_study, aes(x = n_injuries_outlierfix)) + 
  geom_histogram(binwidth = 30) +
  geom_vline(xintercept = 200, size = 1.5, alpha = 0.5) + 
  scale_x_continuous(breaks = breaks_width(100, 0)) + 
  scale_y_continuous(breaks = breaks_width(4, 0), expand = expand_bar) + 
  xlab("Number of injuries") +
  ylab("Number of studies") +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(family="Trebuchet MS", colour="black", face = "bold"))
dev.off()
