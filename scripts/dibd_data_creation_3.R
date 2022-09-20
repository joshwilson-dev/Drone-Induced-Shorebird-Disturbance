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
packages <- c("tidyr", "readr", "dplyr", "ggplot2")
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
data <- read_csv("data/dibd_clean.csv")

######################
#### Prepare Data ####
######################

data_prep <- data %>%
    group_by(flight, species) %>%
    # create analysis parameters
    mutate(
        response = lead(response),
        tend = lead(time_since_launch),
        interval = tend - time_since_launch,
        offset = log(interval)) %>%
    # check if another species already took flight
    ungroup() %>%
    mutate(sum_response = rowSums(select(., contains("response_")))) %>%
    group_by(flight, species) %>%
    mutate(sum_response = lead(sum_response)) %>%
    drop_na(response) %>%
    droplevels()

# save ped data as new csv
write.csv(data_prep, "data/dibd_prep.csv", row.names = FALSE)
