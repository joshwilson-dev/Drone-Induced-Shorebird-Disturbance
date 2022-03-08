################
#### Header ####
################

# Title: Shorebird Disturbance Analysis: FID
# Author: Josh Wilson
# Date: 08-08-2021

###############
#### Setup ####
###############

# Install Packages
packages <- c("tidyverse", "mgcv", "visreg")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
    package_consent <- readline(
        prompt <- (paste("Install", new_packages, " y/n?\n")))
    if (tolower(package_consent) == "y") {
        install.packages(new_packages)
        }
    else print(paste("This code cannot be run without", new_packages))
}

# Import Packages
lapply(packages, require, character.only = TRUE)

# Clear Environment
rm(list = ls())

# Import Data
data_clean <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

data_fid <- data_clean %>%
    # keep only first instance of flight for each approach and species
    filter(behaviour == 1) %>%
    group_by(test, flight, species) %>%
    slice(1)

#################
#### Fit gam ####
#################

# Main Effects

# species:
# does the fid vary with species

# altitude:
# does the fid vary with altitude?

# drone:
# does the fid vary with drone type?

# approach type:
# does the fid vary with approach type?

# Random Effects:

# flock number
# each flock is a random sample of the total bird population

# Interactions:

gam_fid <- gam(
    dronebirddistance ~
    s(height_above_takeoff.meters.),
    data = data_fid,
    family = "gaussian",
    method = "REML")

summary(gam_fid)

###########################
#### Creating New Data ####
###########################

# Creating new data

altitude_fid <- seq(30, 110, by = 1)

new_data_fid <- expand.grid(height_above_takeoff.meters. = altitude_fid)

pred_fid <- predict.gam(
    gam_fid,
    new_data_fid,
    trans = binomial()$linkinv,
    type = "response",
    se.fit = TRUE)

results_fid <- new_data_fid %>%
    mutate(prediction = pred_fid$fit) %>%
    mutate(upper = pred_fid$fit + (2 * pred_fid$se.fit)) %>%
    mutate(lower = pred_fid$fit - (2 * pred_fid$se.fit))

#############################
#### Visualising Results ####
#############################

# effect of altitude for mavic 2 data
ggplot() +
    theme_set(theme_bw()) +
    geom_ribbon(
        data = results_fid,
        aes(
            height_above_takeoff.meters.,
            ymin = lower,
            ymax = upper,
            colour = "orange",
            fill = "orange"),
        alpha = 0.1) +
    geom_line(
        data = results_fid,
        aes(
            height_above_takeoff.meters.,
            prediction,
            colour = "orange"),
        size = 1.2) +
    geom_point(
        data = filter(data_fid, drone == "mavic 2 pro"),
        aes(height_above_takeoff.meters.,
            dronebirddistance,
            colour = "orange")) +
    coord_flip(ylim = c(0, 260), xlim = c(0, 120)) +
    labs(x = "Drone Altitude (m)", y = "Flight Initiation Distance (m)") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none",
        aspect.ratio = 0.45) +
    scale_x_continuous(
        minor_breaks = round(seq(0, 140, 20)),
        breaks = round(seq(0, 140, by = 20), 1),
        expand = c(0, 0)) +
    scale_y_continuous(
        minor_breaks = round(seq(-400, 400, 20)),
        breaks = round(seq(-400, 400, by = 20), 1),
        expand = c(0, 0))
