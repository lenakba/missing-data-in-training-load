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
library(chron) # for working with time variables

#optional packages
library(devEMF) # for saving emf files
library(lmisc) # for bjsm colors and ggplot themes
library(ostrc) # for anonymization function


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
d_srpe_selected = d_srpe_full %>% select(player_id, training_date = planned_date, activity, duration, difficulty, load)

# test that no one has 0 duration or 0 diffulty yet have more than 0 load or difficulty
d_srpe_selected %>% filter(duration == "00:00:00")
d_srpe_selected %>% filter(difficulty == 0 & activity == "Training") 

# some players have reported the activity "training" with a duration higher than 0, 
# yet difficulty is set to 0, which means no training was performed
# we will assume these had a difficulty of 1
# in addition, if activity = training, but duration = 0, we assume difficulty and load to be 0 too
# we use the chron package to create duration into a numeric variable
d_srpe_selected = d_srpe_selected %>% 
                  mutate(times_tz = chron::times(duration), 
                         minutes = chron::minutes(times_tz), 
                         hours = chron::hours(times_tz),
                         hours_in_minutes = hours*60,
                         minutes_sum = hours_in_minutes+minutes,
                         duration_min = minutes_sum,
                         difficulty = ifelse(activity == "Training" & difficulty == 0 & duration_min > 0, 1, difficulty),
                         difficulty = ifelse(activity == "Training" & duration_min == 0, 0, difficulty),
                         load = duration_min*difficulty) %>% # we need to recalculate srpe thanks to that
  select(player_id, training_date, difficulty, duration = duration_min, load)

# we sum multiple sessions on the same day per individual for a daily sRPE measure
d_srpe_daily = d_srpe_selected %>% group_by(player_id, training_date) %>% summarise(load = sum(load, na.rm = TRUE))

# in case we need them, we choose a random difficulty and duration row per player, per date and add it to the daily srpe data
# daily duration and difficulty (ddd)
# the duration and difficulty wont always multiply to the load because multiple sessions may have been part of the srpe and here we just chose one
d_ddd = d_srpe_selected %>% select(player_id, training_date, duration, difficulty) %>% distinct(player_id, training_date, .keep_all = TRUE) 
d_srpe = d_srpe_daily %>% left_join(d_ddd, by = c("player_id", "training_date"))

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
d_weeks = d_weeks_full %>% select(datekey, training_date, day_of_week, week_nr, match_indicator, n_matches, mc_day)

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
d_load = d_player_gps_dt %>% full_join(d_srpe_dt, by = c("player_id", "training_date", "day_of_week", "datekey", "week_nr", "match_indicator", "n_matches", "mc_day"))

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
d_load = d_load %>% mutate(missing_player = ifelse(player_id %in% players_no_gps_nor_srpe, 1, 0),
                           missing_player_text = ifelse(missing_player == 1, "Player missing all TL", "Player not missing all TL"))

# create indicator for missing values
# this is because we will later on add dates that are missing per player
# and we want to distinguish missing values where the player had answered something, but not the load
# from days that are missing entirely
d_load = d_load %>% mutate(missing_td = ifelse(is.na(total_distance_daily), 1, 0),
                           missing_td_text = ifelse(is.na(total_distance_daily), "Missing Explicitly", "Not Missing"),
                           missing_load = ifelse(is.na(load), 1, 0),
                           missing_load_text = ifelse(is.na(load), "Missing Explicitly", "Not Missing"))

#------------------------------------- Step 7: Add dates with neither sRPE data, nor GPS data, nor implicit free day data, to each player

#Missing Data - The Well of Lost Potential

# the idea is to calculate 
# number of responses / number of questionnaires sent

# if study startdate and study enddate is unknown, one can find the 
# first and last dates available in the entire dataset 
# here, we know the last date is 2019-12-03
# and we find the first date from the data
first_date = d_load %>% summarise(min(training_date, na.rm = TRUE)) %>% pull()
d_load_full = d_load %>% group_by(player_id) %>% 
         tidyr::complete(training_date = seq.Date(first_date, lubridate::as_date("2019-12-03"), by = "day")) %>% ungroup()

d_load_full = d_load_full %>% mutate(missing_td = ifelse(is.na(missing_td), 2, missing_td),
                           missing_td_text = ifelse(is.na(missing_td_text), "GPS use unknown", missing_td_text),
                           missing_load = ifelse(is.na(missing_load), 2, missing_load),
                           missing_load_text = ifelse(is.na(missing_load_text), "Missing Implicitly", missing_load_text))

# missing can now be calculated
d_load_full %>% count(missing_td, missing_td_text)
d_load_full %>% count(missing_load, missing_load_text)

# filling missing player variables
d_load_full = d_load_full %>% group_by(player_id) %>% fill(missing_player, missing_player_text, .direction = "downup") %>% ungroup()

#---------------------------------------- Step 8 Anonymize the ID so that the data used in simulations can later be uploaded as-is

set.seed(1234) # in case we need to run this script and create the data again
# use the anonymization function to easily anonymize the data
# the new ID has nothing to do with the old one
ano_func = make_anonymize_func(d_load_full$player_id)
d_load_anon = d_load_full %>% mutate(p_id = ano_func(player_id)) %>% select(-player_id) # remove old ID

#---------------------------------------- Step 9 save the final dataset to be used in simulations

# select wanted columns in the order that we want them
d_load_final = d_load_anon %>% select(p_id, training_date, day_of_week, load, duration, difficulty, total_distance_daily, missing_load, missing_load_text, missing_td, missing_td_text, missing_player, missing_player_text)

# where to place the saved data
folder_export = paste0("O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\")

# write .csv
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
write_excel_csv(d_load_final, paste0(folder_export, "norwegian_premier_league_football_anon.csv"), delim = ";", na = "")
