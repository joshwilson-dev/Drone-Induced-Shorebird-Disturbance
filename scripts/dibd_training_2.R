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
        drone = as.factor(drone),
        location = as.factor(location),
        flight = as.factor(flight),
        flock = as.factor(flock),
        common_name = as.factor(common_name),
        drone_obscured = as.factor(drone_obscured),
        sentinel_flight = as.factor(sentinel_flight),
        eastern_curlew = as.factor(eastern_curlew),
        migrating = as.factor(migrating))

summary(data_ped)

##############################
#### Train and Save Model ####
##############################

# fit model (took ~ 5hrs on Intel(R) Core(TM) i9-10900X CPU @ 3.70GHz 3.70 GHz)
system.time({
    fit <- gam(
        ped_status ~
        # stimulus
        s(drone, bs = "fs") +
        te(xy_disp_m, z_disp_m, by = eastern_curlew, k = 3) +
        s(xb_vel_ms, k = 3) +
        s(z_vel_ms, k = 3) +
        s(xyz_acc_mss, k = 3) +
        # environment
        s(tend, k = 3) +
        s(drone_obscured, bs = "fs") +
        s(wind_speed_ms, k = 3) +
        s(cloud_cover_p, k = 3) +
        s(hrs_from_high, k = 3) +
        s(temperature_dc, k = 3) +
        s(location, bs = "fs") +
        # target
        s(migrating, bs = "fs") +
        s(common_name, bs = "fs") +
        s(normalised_count, k = 3) +
        s(sentinel_flight, bs = "fs") +
        s(flight, bs = "re"),
        data = data_ped,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

# save model
save_prefix <- "models/dibd-model-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
