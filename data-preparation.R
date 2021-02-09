# this script serves to prepare the football data for use in missin data simulation
# in this endeavor we will:
# Step 1: obtain GPS data and extract total distance
# Step 2: Remove ostensibly erroneous GPS data at the second level (this becomes part of missing data)
# Step 3: Remove ostensibly erroneous GPS data at the daily level
# Step 4: Obtain RPE data, add implicit days where the players did not have any training (day after match, weekends etc.)
# Step 5: and combine RPE and GPS data at the daily level so that we have both in the same dataset
# Step 6 *bonus*: obtain injury data and combine it with this data, in case we get to do logistic regression bonus analyses
# Step 7: anonymize the ID so that the data used in simulations can later be uploaded as-is
# Step 8: save the final dataset to be used in simulations

# required packages to run this script
library(DBI) # for database extraction with SQL
library(tidyverse) # for data wrangling
library(rlang) # tidyverse function building tools

#optional packages
library(devEMF) # for saving emf files
library(lmisc) # for bjsm colors and ggplot themes


# Step 1: obtain GPS data and extract total distance------------------------------------

#---------------------------------------Connecting to database
db = 'stromsgodset'  #provide the name of your db
db_port = '5432'  # or any other port specified by the DBA
db_user = "postgres" 
db_password = "postgresql"
db_stromsgodset = dbConnect(RPostgreSQL::PostgreSQL(), dbname = db, port=db_port, user=db_user, password=db_password) 

#--------------------------------------Obtaining data

d_gps_full = dbGetQuery(db_stromsgodset, 
                        paste0("SELECT *
                  FROM training_data_2019.training_data")) %>% as_tibble()

d_td_full = d_gps_full %>% select(player_id, dt, datekey, session_id, total_distance, injury_id)
remove(d_gps_full)

# Step 2: Remove ostensibly erroneous GPS data at the second level (this becomes part of missing data)

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

#----------------- Step 3: Remove ostensibly erroneous GPS data at the daily level


remove(d_td_full)
d_td









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


