# bdb 2021 cuse

# libraries
library(tidyverse)
library(dplyr)
library(Rfast)
library(data.table)
library(caret)
library(devtools)
library(doParallel)
library(scales)
library(mclust)
library(reshape)
library(nloptr)
library(keras)
library(tensorflow)
library(truncnorm)
library(ranger)

# options
memory.limit(size = 1e14)
options(scipen = 999)

# set seed
set.seed(123)

# parallel computing
cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

########################
### SOURCE FUNCTIONS ###
########################

# load all functions from funs folder - invisible hides output
invisible(sapply(paste0('./funs/', list.files('./funs')), source, .GlobalEnv))

####################
### READING DATA ###
####################

# merge coverages and targeted receiver and merge nflfastR and plays
df_plays <- bdb_read_plays()

# read in tracking data for all 17 weeks
df_tracking <- bdb_read_tracking(seq(1, 17))

##########################
### CLUSTERED POSITION ###
##########################

# run position clustering model
df_positions <- bdb_create_positions(df_plays, df_tracking)
mod_position <- bdb_run_position_model(df_positions)
df_tracking <- bdb_merge_positions_tracking(df_tracking, df_positions, mod_position)

###########################
### CALCULATE VARIABLES ###
###########################

# loop through weeks and calculate variables for coverage model
df_tracking_vars <- bdb_calculate_tracking_vars(df_tracking)

###############################################
### ADD INDIVIDUAL DEFENSIVE SPECIFICATIONS ###
###############################################

# add man-zone-blitz specifications
df_specs <- bdb_man_zone_specs(df_plays, df_tracking)

# merge tracking and specs + filter out blitzers and prevent zone
df_tracking_model <- bdb_merge_tracking_specs(df_tracking_vars, df_specs)

################################
### RUN NEURAL NETWORK MODEL ###
################################

# split train and test
df_train <- bdb_create_training_data(df_tracking_model)
df_test <- bdb_create_test_data(df_tracking_model)

# run model
mod_coverage <- bdb_run_coverage_model(df_train, 10, 5, 2, 10, 0.1, 0.5, 1000)

# train and test predictions
df_preds <- bdb_calculate_model_predictions(mod_coverage, df_train, df_test)

# evaluate predictions - confusion matrix
cm <- bdb_evaluate_model_predictions(mod_coverage, df_train)

###########################
### ADD TEAM FORMATIONS ###
###########################

# coverage rules
df_coverages_probability <- bdb_defensive_formation_probs(df_plays, df_tracking, df_preds)

#############################
### FIELD OWNERSHIP MODEL ###
#############################

# calculate field ownership model
mod_fo <- bdb_field_ownership_model(1000)

# run model
df_control <- bdb_calculate_field_ownership(mod_fo, df_tracking)

# run field ownership over expectation model
mod_fo_above_expected <- bdb_field_ownership_above_expected_model(df_control, df_tracking, df_plays, df_preds, df_coverages_probability)

# field ownership results
df_foae <- bdb_fo_above_expected_results(mod_fo_above_expected, df_control, df_tracking, df_plays, df_preds, df_coverages_probability)

# save data for plots
bdb_save_data_data(2018111810, 1888, 11, df_tracking, df_plays, df_tracking_vars, df_preds, df_foae, mod_fo)

# end parallel computing
stopCluster(cl)
rm(cl)

# save data
# save.image(file = 'session.RData')
# gc()
