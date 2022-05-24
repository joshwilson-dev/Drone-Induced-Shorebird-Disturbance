################
#### Header ####
################

# Title: Drone Induced Bird Disturbance
# Author: Josh Wilson
# Date: 23-05-2022

###############
#### Setup ####
###############

install.packages("mgcv")
install.packages("dplyr")
install.packages("readr")
install.packages("stringr")
install.packages("tidyr")

library(mgcv)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

##########################
#### Data Preparation ####
##########################

# import data
data <- read_csv("./shorebird-disturbance-clean.csv", guess_max = 1000000)

# prepare train and test data in ped format
prepare_data <- function(df) {
    data_clean <- df %>%
        # add migrating for migratory shorebirds
        mutate(migrating = case_when(
            (month_aest == 4 | month_aest == 5) &
            (common_name == "eastern_curlew" |
            common_name == "whimbrel" |
            common_name == "bar_tailed_godwit" |
            common_name == "great_knot") ~ TRUE,
            TRUE ~ FALSE)) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # approach ends if birds take flight
        group_by(test, flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        # one time interval is 0.1 s
        mutate(time = time * 10) %>%
        # degrade data to fit faster, but keep first sentinel flight and flight
        group_by(test, flight, behaviour, sentinel_flight) %>%
        mutate(keep = case_when(
            sentinel_flight == 1 & behaviour == 0 & row_number() <= 1 ~ 1,
            behaviour == 1 ~ 1,
            time %% 10 == 0 ~ 1,
            TRUE ~ 0)) %>%
        group_by(time) %>%
        mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            approach = as.factor(approach),
            flock = as.factor(flock),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            migrating = as.factor(migrating)
        )

    data_ped <- data_clean %>%
        group_by(test, flight, common_name) %>%
        mutate(
            ped_status = lead(behaviour),
            tstart = time,
            tend = lead(time),
            interval = tend - tstart,
            offset = log(interval)) %>%
        drop_na(ped_status) %>%
        ungroup() %>%
        droplevels()
    return(data_ped)
}

data_ped <- prepare_data(data)

##########################
#### Fit & Save Model ####
##########################

# fit model
system.time({
    fit <- gam(
        ped_status ~
        # drone
        drone +
        te(xy_disp_m, z_disp_m, by = common_name, k = 5) +
        s(xb_vel_ms, k = 5) +
        s(z_vel_ms, k = 5) +
        s(xyz_acc_mss, k = 5) +
        # environment
        s(tend, k = 5) +
        drone_obscured +
        migrating * common_name +
        # target
        # s(flock, by = common_name, bs = "re") +
        s(approach, bs = "re") +
        sentinel_flight,
        data = data_ped,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

# save model
saveRDS(fit, "model.rds")