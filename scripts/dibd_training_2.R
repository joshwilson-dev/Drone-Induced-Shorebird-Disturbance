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
packages <- c("readr", "dplyr", "mgcv", "pammtools")
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
data_ped <- read_csv("data/dibd_ped_data.csv") %>%
    # specify factors
    mutate(
        flight = as.factor(flight),
        flock = as.factor(flock),
        species = as.factor(species)) %>%
    # normalie count
    group_by(species) %>%
    mutate(normalised_count = count / max(count))

##############################
#### Train and Save Model ####
##############################

# fit model (took ~ 5hrs on Intel(R) Core(TM) i9-10900X CPU @ 3.70GHz 3.70 GHz)
system.time({
    fit <- gam(
        ped_status ~
        # stimulus
        specification +
        s(distance_x, by = species, k = 5) +
        s(distance_z, by = species, k = 5) +
        s(velocity_x, k = 5) +
        s(velocity_y, k = 5) +
        s(velocity_z, k = 5) +
        s(acceleration, k = 5) +
        # environment
        s(tend, k = 5) +
        obscuring +
        s(wind_speed, k = 5) +
        s(cloud_cover, k = 5) +
        s(high_tide, k = 5) +
        s(temperature, k = 5) +
        location +
        # target
        species +
        s(normalised_count, k = 5) +
        s(flight, bs = "re") +
        s(flock, bs = "re"),
        data = data_ped,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

# save model
save_prefix <- "models/dibd-model-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))