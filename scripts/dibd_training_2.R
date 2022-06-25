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
    mutate(target_flock = as.factor(target_flock))

summary(data_ped)

##############################
#### Train and Save Model ####
##############################

# fit model (took ~ 5hrs on Intel(R) Core(TM) i9-10900X CPU @ 3.70GHz 3.70 GHz)
system.time({
    fit <- gam(
        ped_status ~
        # stimulus
        stimulus_specification +
        s(stimulus_dxy_m, k = 5) +
        s(stimulus_dz_m, k = 5) +
        s(stimulus_vxy_ms, k = 5) +
        s(stimulus_vz_ms, k = 5) +
        # environment
        s(tend, k = 5) +
        environment_obscured +
        s(environment_wind_ms, k = 5) +
        s(environment_cloud_p, k = 5) +
        s(environment_peaktide_hrs, k = 5) +
        s(environment_temperature_dc, k = 5) +
        environment_location +
        # target
        s(count_eastern_curlew, k = 5) +
        s(target_flock, bs = "re"),
        data = data_ped,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

# save model
save_prefix <- "models/dibd-model-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
