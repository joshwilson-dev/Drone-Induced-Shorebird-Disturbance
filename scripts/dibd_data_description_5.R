################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Data Description
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
packages <- c("readr", "dplyr")
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

# import data
data_ped <- read_csv("data/dibd_ped_data.csv")

############################
#### General Statistics ####
############################

total_appraoches <- data_ped %>%
    group_by(flight) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(total_appraoches)

approaches_per_species <- data_ped %>%
    group_by(flight, common_name) %>%
    slice(1) %>%
    group_by(common_name) %>%
    summarise(count = n())

View(approaches_per_species)

approaches_per_site <-  data_ped %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(location) %>%
    summarise(count = n())

View(approaches_per_site)

approaches_per_drone <- data_ped %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(drone) %>%
    summarise(count = n())

View(approaches_per_drone)
