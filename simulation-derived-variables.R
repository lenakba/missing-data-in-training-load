# one question that is unsolved is how to impute so-called 
# derived variables
# here, we try to find out the best method for sRPE

# may be relevant: https://journals.sagepub.com/doi/full/10.1177/0962280214521348
# definitely relevant: https://stefvanbuuren.name/fimd/sec-knowledge.html 

# loading packages
library(tidyverse) # for datawrangling
library(DBI) # for database extraction with SQL
library(chron) # for calculating duration

# Connecting to database
db = 'stromsgodset'  #provide the name of your db
db_port = '5432'  # or any other port specified by the DBA
db_user = "postgres" 
db_password = "postgresql"
db_stromsgodset = dbConnect(RPostgreSQL::PostgreSQL(), dbname = db, port=db_port, user=db_user, password=db_password) 

# get data from database
d_srpe_full = dbGetQuery(db_stromsgodset, 
                         paste0("SELECT *
                  FROM training_data_2019.temp_training_log")) %>% as_tibble() 
d_srpe_selected = d_srpe_full %>% select(player_id, training_date = planned_date, activity, duration, difficulty, load)

# some players have reported the activity "training" with a duration higher than 0, 
# yet difficulty is set to 0, which means no training was performed
# we will assume these had a difficulty of 1
# in addition, if activity = training, but duration = 0, we assume difficulty and load to be 0 too
# we use the chron package to create duration into a numeric variable
d_srpe_fixed = d_srpe_selected %>% 
  mutate(times_tz = chron::times(duration), 
         minutes = chron::minutes(times_tz), 
         hours = chron::hours(times_tz),
         hours_in_minutes = hours*60,
         minutes_sum = hours_in_minutes+minutes,
         duration_min = minutes_sum,
         difficulty = ifelse(activity == "Training" & difficulty == 0 & duration_min > 0, 1, difficulty),
         difficulty = ifelse(activity == "Training" & duration_min == 0, 0, difficulty),
         load = duration_min*difficulty) %>% # we need to recalculate srpe thanks to that
  select(player_id, training_date, rpe = difficulty, duration = duration_min, srpe = load)

