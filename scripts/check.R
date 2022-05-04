################
#### Header ####
################

# Title: Drone Induced Bird Disturbance - Time-Varying Effects
# Author: Josh Wilson
# Date: 23-03-2022
# Reference: https://cran.r-project.org/web/packages/mgcv/mgcv.pdf

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

packages <- c("tidyverse")
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

##########################
#### Data Preparation ####
##########################

# import data
data_new <- read_csv(choose.files(), guess_max = 1000000)

View(head(data_new))
summary(data_new)

check <- data_new %>%
    group_by(test, flight) %>%
    mutate(max_dist = max(xy_disp_m)) %>%
    slice(1) %>%
    select(max_dist, test, flight, location) %>%
    filter(location == "toorbul")
