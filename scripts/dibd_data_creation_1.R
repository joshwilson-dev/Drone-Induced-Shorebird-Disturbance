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
data <- read_csv("data/dibd_data.csv")

######################
#### Prepare Data ####
######################

# prepare data in ped format
prepare_data <- function(df) {
    data_ped <- df %>%
        # add normalised count
        group_by(species) %>%
        mutate(normalised_count = count / max(count)) %>%
        ungroup() %>%
        # degrade data but keep first flight
        mutate(keep = case_when(
            response == 1 ~ 1,
            time_since_launch %% 5 == 0 ~ 1,
            TRUE ~ 0)) %>%
        group_by(time_since_launch) %>%
        mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1) %>%
        # remove any time points without more than one instance
        group_by(time_since_launch) %>%
        filter(n() > 1) %>%
        # create ped parameters
        group_by(flight, species) %>%
        mutate(
            ped_status = lead(response),
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
