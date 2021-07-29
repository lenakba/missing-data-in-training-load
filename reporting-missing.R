# this script is for calculating the amount of missing data
# at different levels and variants
library(tidyverse) # for datawrangling
library(lmisc) # for ggplot2 themes
library(visdat) # for figures showing missing data pattern

# so we don't have to deal with scientific notations
# and strings aren't automaticcaly read as factors
options(scipen = 17, 
        stringsAsFactors = FALSE)

# reading data
folder_data = paste0("my\\data\\folder\\")

# note that the RPE data is per session (which there can be multiple of per day)
# and that the Total Distance data is per day
d_rpe_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_rpe_anon.csv"), delim = ";")
d_td_full = read_delim(paste0(folder_data, "norwegian_premier_league_football_td_anon.csv"), delim = ";")
d_td_full = d_td_full %>% rename(gps_td = total_distance_daily, gps_v4 = v4_distance_daily, gps_v5 = v5_distance_daily, gps_pl = player_load_daily)

# select vars we need in the simulation, key variables we think are correlated with the level of sRPE
keyvars = c("p_id", "training_date", "mc_day", "week_nr")
d_srpe = d_rpe_full %>% filter(!is.na(rpe) & !is.na(duration)) %>% select(all_of(keyvars), rpe, duration) %>% mutate(srpe = rpe*duration)

# calculating missing data
# number of players
n_distinct(d_td_full$p_id)

# number of players that did not have total distance data
d_td_full %>% distinct(p_id, .keep_all = TRUE) %>% summarise(missing_player = sum(missing_player == 1))
d_td_full %>% count(missing_td, missing_td_text)

# number of players that did not have RPE data
d_rpe_full %>% distinct(p_id, .keep_all = TRUE) %>% summarise(missing_player = sum(missing_player == 1))

# how many missing rpe?
d_rpe_full %>% count(missing_rpe, missing_rpe_text)
d_rpe_full %>% count(missing_duration, missing_duration_text)

# how many missing daily rpe?
d_td_full %>% summarise(missing_srpe = sum(is.na(srpe)))
d_td_full %>% count(missing_srpe, missing_srpe_text)

# how many missing daily total distance?
d_td_full %>% filter(!is.na(gps_day), missing_player == 0) %>% summarise(missing_td = sum(!is.na(gps_td)))

#-------------------------------------------------------- Figure on the pattern of missing data
# first remove days when GPS devices were not worn
d_td_gpsdays = d_td_full %>% filter(!is.na(gps_day))

# select relevant variables
d_td_gpsdays = d_td_gpsdays %>% select(p_id, training_date, week_nr, srpe, starts_with("gps"), mc_day, -gps_day)

# arrange by week number to showcase time effect
d_td_gpsdays = d_td_gpsdays %>% arrange(week_nr)

missing_pattern_plot = vis_dat(d_td_gpsdays) +
  scale_fill_manual(labels = c("Nominal", "Date", "Continuous", "Missing observation")) + 
  theme(text = element_text(size = 14, family="Trebuchet MS"),
        legend.key = element_rect(fill = "transparent", colour = "black")) +
  guides(fill=guide_legend(title="Variable type"))

# to save the figure
png("missing_pattern.png", width = 7.5, height = 6, units = "in", res = 600)
missing_pattern_plot
dev.off()

#--------------------------------------------------------Figures showing distribution of load values

# remove missing
# this has to be done before choosing variables so that the variables will be comparable
# i.e. the results of having or not having sRPE and position in the dataset cannot be confused with
# differing sample sizes due to unequal levels of missing.
d_td = na.omit(d_td_full)

# values used in simulation
srpe_values = d_srpe$srpe
td_values = d_td$gps_td

# distributions of load values
plot_hist_srpe = 
  ggplot(enframe(srpe_values), aes(x = srpe_values)) + 
  geom_histogram() +
  xlab("sRPE (AU)") + ylab("Count")  +
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.06), add = 0)) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())

plot_hist_td = 
  ggplot(enframe(td_values), aes(x = td_values)) + 
  geom_histogram() + 
  xlab("Total Distance (m)")  + ylab("Count") + 
  scale_y_continuous(expand = expansion(mult = c(0.0, 0.06), add = 0)) + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank())
