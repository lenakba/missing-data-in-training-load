# this script serves to prepare the football data for use in missin data simulation
# in this endeavor we will:
# Step 1: obtain GPS data and extract total distance
# Step 2: Remove ostensibly erroneous GPS data at the second level (this becomes part of missing data)
# Step 3: Remove ostensibly erroneous GPS data at the daily level
# Step 4: Obtain RPE data
# Step 5: Find implicit days where the players did not have any training (day after match, weekends etc.)
# Step 6: Combine RPE and GPS data at the daily level so that we have both in the same dataset
# Step 7: Add dates with neither sRPE data, nor GPS data, nor implicit free day data, to each player
# Step 8: Anonymize the ID so that the data used in simulations can later be uploaded as-is
# Step 9: save the final dataset to be used in simulations

# required packages to run this script
library(DBI) # for database extraction with SQL
library(tidyverse) # for data wrangling
library(rlang) # tidyverse function building tools

#optional packages
library(devEMF) # for saving emf files
library(lmisc) # for bjsm colors and ggplot themes


#-------------------------------------- Step 1: obtain GPS data and extract total distance

# Connecting to database
db = 'stromsgodset'  #provide the name of your db
db_port = '5432'  # or any other port specified by the DBA
db_user = "postgres" 
db_password = "postgresql"
db_stromsgodset = dbConnect(RPostgreSQL::PostgreSQL(), dbname = db, port=db_port, user=db_user, password=db_password) 

# Obtaining data
d_gps_full = dbGetQuery(db_stromsgodset, 
                        paste0("SELECT *
                  FROM training_data_2019.training_data")) %>% as_tibble()

d_td_full = d_gps_full %>% select(player_id, dt, datekey, session_id, total_distance, injury_id)
remove(d_gps_full)

#-------------------------------------------- Step 2: Remove ostensibly erroneous GPS data at the second level (this becomes part of missing data)

# From Garth's Master Thesis:
# Due to the dependence of these features upon values
# from previous training sessions, it is considered more harmful to
# exclude values than to replace them with smoothed averages. 
# For this reason, the approach of smoothing by bin means is adopted, 
# and training sessions with recorded values falling outside of a defined range are
# replaced by the mean values of all correct recordings from the same session.

# we chose to remove values above 30, assuming that the GPS was not working at that moment
# for values between 16 and 30, we set the value to 15, 
# which is 3 m/s faster than Hussein Bolt at his peak. The GPS is unreliable at times, and in doing so, we 
# assume the player was running at their fastest at that moment.

# the methods above are the same as in the soccer study on training load using the same data
remove_impossible = function(x){
  x = ifelse(x > 30, NA, x)
  x
}

adjust_outliers = function(x){
  x = ifelse(x > 15, 15, x)
  x
}

# perform removal and adjustment
d_td = d_td_full %>% mutate(total_distance = remove_impossible(total_distance))
d_td = d_td %>% mutate(total_distance = adjust_outliers(total_distance))


#to calculate the number of values removed and changed from this process
calc_n_change = function(x){
  d_remove = d_td_full %>% summarise(remove = sum(x > 30))
  d_change = d_td_full %>% summarise(change = sum(x > 15 & x <= 30))
  d_remove = d_remove %>% mutate(change = d_change$change, n_denominator = nrow(d_td_full), prop_remove = remove/n_denominator, prop_change = change/n_denominator)
  d_remove
}

d_n_cleaned = bind_rows(calc_n_change(d_td_full$total_distance)) %>% mutate(label = "Total Distance")

#--------------------------------------------------------- Step 3: Remove ostensibly erroneous GPS data at the daily level

# We will first calculate the sum of minutes in acitivity per person per day
d_duration = d_td %>% 
  group_by(player_id, datekey) %>% 
  mutate(sum_minutes = as.numeric(difftime(max(dt), min(dt), units = "mins"))) %>% 
  ungroup() 

# we will calculate the total distance per person per day
# then validate the TD by dividing it by the sum of minutes in activity 
vars = c("player_id", "datekey")
d_td_daily = d_duration  %>% select(all_of(vars), total_distance, sum_minutes) %>%  
  group_by(player_id, datekey) %>% 
  mutate(total_distance_daily = sum(total_distance, na.rm = TRUE)) %>% 
  distinct(player_id, datekey, .keep_all = TRUE) %>% 
  # if the player has 0 sum minutes, they have 0 TD
  mutate(total_distance_daily = ifelse(sum_minutes == 0, 0, total_distance_daily),
         total_distance_minute = total_distance_daily/sum_minutes) %>% ungroup() %>% select(-total_distance)

# looks pretty good:
d_td_daily %>% arrange(desc(total_distance_minute))

#---------------------------------------------------------------------Step 4: Obtain RPE data
d_srpe_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.temp_training_log")) %>% as_tibble() 
d_srpe = d_srpe_full %>% select(player_id, training_date = planned_date, activity, duration, difficulty, load)

# we sum multiple sessions on the same day per individual for a daily sRPE measure
d_srpe = d_srpe %>% group_by(player_id, training_date) %>% summarise(load = sum(load, na.rm = TRUE))

#--------------------------------------------------------------------- Step 5: Find implicit days where the players did not have any training (day after match, weekends etc.)

# We want to make sure that dates where the
# players didn't participate in any activity are included
# as these are implicitly sRPE and Total distance = 0.
# unless any other information is given, 
# we will assume that the 2 days after a match, 
# given that the week has only 1 match, are free days with 0 TD for all players 

# obtaining information about dates
d_date_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.date_dimension")) %>% as_tibble()


# finding number of matches per week
# add an index with number of matches for each of the weeks
d_n_matches = d_date_full %>% group_by(week_nr) %>% summarise(n_matches = sum(match_indicator))
d_date_full = d_date_full %>% left_join(d_n_matches, by = "week_nr")

# Add categorical variable describing each day in relation to match days
# According to Torstein Dalen-Lorentsen the sequence:
# M
# M+1
# M+2
# m-4
# m-3
# m-2
# m-1
# M

# for weeks with 1 match
d_match_weeks = d_date_full %>% filter(n_matches == 1) 
d_match_weeks = d_match_weeks %>% mutate(mc_day = case_when(match_indicator ~ "M",
                                                            lag(match_indicator) ~ "M+1", 
                                                            lag(match_indicator, 2) ~ "M+2",
                                                            lead(match_indicator) ~ "M-1",
                                                            lead(match_indicator, 2) ~ "M-2",
                                                            lead(match_indicator, 3) ~ "M-3",
                                                            lead(match_indicator, 4) ~ "M-4")
)

# some days are missing because the week had the match placed on an untraditional day
# d_match_weeks %>% filter(is.na(mc_day))

# And for two matches per week or more:
# M
# m-2
# m-1
# M
# M-3
# m-2
# m-1
# M

d_match_weeks_dbl = d_date_full %>% filter(n_matches >= 2) 
d_match_weeks_dbl = d_match_weeks_dbl %>% mutate(mc_day = case_when(match_indicator ~ "M",
                                                                    lead(match_indicator) ~ "M-1",
                                                                    lead(match_indicator, 2) ~ "M-2",
                                                                    lead(match_indicator, 3) ~ "M-3",
                                                                    lead(match_indicator, 4) ~ "M-4",
                                                                    lead(match_indicator, 5) ~ "M-5")
)

# weeks without a match
d_free_weeks = d_date_full %>% filter(n_matches == 0) %>% mutate(mc_day = "Non-match week")

# combine data
d_weeks_full = bind_rows(d_free_weeks, d_match_weeks, d_match_weeks_dbl) %>% arrange(training_date)

# this is now the ready date dataset which we will combine later to get days without training
d_weeks = d_weeks_full %>% select(datekey, training_date, week_nr, match_indicator, n_matches, mc_day)

#--------------------------------------------Step 6: Combine RPE and GPS data at the daily level so that we have both in the same dataset

# since player is the highest level in the ID-hierarchy (player, training date, session id)
# will will first combine to the player data so we can later calculate how many players have 
# missing data
d_player = dbGetQuery(db_stromsgodset, 
                           paste0("SELECT *
                  FROM training_data_2019.player")) %>% as_tibble() %>% select(player_id)

# add GPS data with leftjoin so we keep players without GPS values
d_player_gps = d_player %>% left_join(d_td_daily, by = "player_id")

# to combine with srpe data, we need the training date, which we gathered from date data
# so we combine date data first
d_player_gps_dt = d_player_gps %>% left_join(d_weeks, by = "datekey")
d_srpe_dt = d_srpe %>% left_join(d_weeks, by = "training_date")

# add sRPE data with fulljoin so we keep players without sRPE values
d_load = d_player_gps_dt %>% full_join(d_srpe_dt, by = c("player_id", "training_date", "datekey", "week_nr", "match_indicator", "n_matches", "mc_day"))

# days that are M+1 or M+2 are sRPE = 0 and total_distance = 0
d_load  = d_load %>% mutate(load = ifelse(is.na(load) & (mc_day == "M+2" | mc_day == "M+1"), 0, load),
                            total_distance_daily = ifelse(is.na(total_distance_daily) & (mc_day == "M+2" | mc_day == "M+1"), 0, total_distance_daily),
                            total_distance_minute = ifelse(is.na(total_distance_minute) & (mc_day == "M+2" | mc_day == "M+1"), 0, total_distance_minute))

# validation checks
# d_load should include:
# all players in d_player regardless of whether they have any total distance or sRPE data
n_distinct(d_player$player_id)
n_distinct(d_load$player_id)

# all players who have either TD data, sRPE data or both
d_load %>% filter(is.na(load))
d_load %>% filter(is.na(total_distance_daily))

# let's add a missing indicator we can use to calculate missing data

# finding players that have nethier GPS nor sRPE data
players_no_gps = setdiff(d_player$player_id, unique(d_td_daily$player_id))
players_no_srpe = setdiff(d_player$player_id, unique(d_srpe_dt$player_id))
players_no_gps_nor_srpe = intersect(players_no_srpe, players_no_gps)

# adding indicator for players who have no data
d_load = d_load %>% mutate(missing_player = ifelse(player_id %in% players_no_gps_nor_srpe, 1, 0))

# create indicator for missing values
# this is because we will later on add dates that are missing per player
# and we want to distinguish missing values where the player had answered something, but not the load
# from days that are missing entirely
d_load = d_load %>% mutate(missing_td = ifelse(is.na(total_distance_daily), 1, 0),
                           missing_td_text = ifelse(is.na(total_distance_daily), "Missing Explicitly", "Not Missing"),
                           missing_load = ifelse(is.na(load), 1, 0),
                           missing_load_text = ifelse(is.na(load), "Missing Explicitly", "Not Missing"))


d_load %>% View()

#--------------number of players
# player table
d_player_full = dbGetQuery(db_stromsgodset, 
                           paste0("SELECT *
                  FROM training_data_2019.player")) %>% as_tibble()

# number of players
nrow(d_player_id)

# number of players with gps data
n_distinct(d_gps$player_id)

#---------------------------------------------Missing data

n_measures_expected = nrow(d_gps)*365

match_week_dates = d_match_weeks %>% distinct(datekey)
d_gps_match_weeks = d_gps %>% filter(datekey %in% match_week_dates$datekey)

# number and percentage missing values
find_missing = function(x){
  x = enquo(x)
  d_gps %>% summarise(n_missing = sum(is.na(!!x)), n_nonmissing = sum(!is.na(!!x)), prop = n_missing/n_nonmissing)
}

find_missing(total_distance)

# number of missing match week days
gps_days = d_gps_match_weeks %>% distinct(datekey)
date_days = d_match_weeks %>% distinct(datekey)
nrow(gps_days)/nrow(date_days)









# Step 4: Obtain RPE data, add implicit days where the players did not have any training (day after match, weekends etc.)
d_date_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.date_dimension")) %>% as_tibble()

d_rpe_full = dbGetQuery(db_stromsgodset, 
                        paste0("SELECT *
                  FROM training_data_2019.temp_training_log")) %>% as_tibble()

d_inj_full = dbGetQuery(db_stromsgodset, 
                        paste0("SELECT *
                  FROM training_data_2019.temp_injury_illness")) %>% as_tibble()

folder = "O:\\Prosjekter\\Dalen-Lorentsen - Belastningsstyring - prosjekt 1 - metode\\Data\\stromsgodset\\raw_data\\"
player_cols = cols(
  name = col_character(),
  id = col_integer(),
  position = col_character(),
  position_group = col_character()
)
d_player = read_delim(paste0(folder, "player_mappings.csv"), delim = ";", col_types = player_cols) %>% select(-name)


