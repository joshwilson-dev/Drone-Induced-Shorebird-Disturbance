################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Model Training
# Author: Josh Wilson
# Date: 13-06-2022
# References:
# https://cran.r-project.org/web/packages/mgcv/mgcv.pdf
# https://adibender.github.io/pammtools/

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Specify required packages
packages <- c("tidyr", "readr", "dplyr")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
    package_consent <- readline(
        prompt <- (paste("Install", new_packages, " y/n?\n")))
    if (tolower(package_consent) == "y") {
        install.packages(new_packages)
        }
    else print(paste("This code cannot be run without", new_packages))
}

# load packages, install if not available provided user approves
lapply(packages, require, character.only = TRUE)

# import and check data
data <- read_csv(unz("data/dibd_data.zip", "dibd_data.csv"))

######################
#### Prepare Data ####
######################

# prepare data in ped format
prepare_data <- function(df) {
    data_clean <- df %>%
        # remove sentinel_flights
        mutate(sentinel_flight = case_when(
            sentinel_flight == "null" ~ "a",
            TRUE ~ sentinel_flight
        )) %>%
        group_by(flight, common_name) %>%
        filter(max(sentinel_flight) == "a") %>%
        # approach ends if birds take flight
        group_by(flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        # degrade data to every second to fit faster
        # but keep first flight
        group_by(flight, behaviour, sentinel_flight) %>%
        mutate(keep = case_when(
            behaviour == 1 ~ 1,
            time_since_launch %% 5 == 0 ~ 1,
            TRUE ~ 0)) %>%
        group_by(time_since_launch) %>%
        mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1)

    # create ped parameters
    data_ped <- data_clean %>%
        group_by(flight, common_name) %>%
        mutate(
            ped_status = lead(behaviour),
            tstart = time_since_launch,
            tend = lead(time_since_launch),
            interval = tend - tstart,
            offset = log(interval)) %>%
        drop_na(ped_status) %>%
        ungroup() %>%
        droplevels()
    return(data_ped)
}

data_ped <- prepare_data(data)

# save ped data as new csv
write.csv(
    data_ped,
    "data/dibd_ped_data.csv",
    row.names = FALSE)