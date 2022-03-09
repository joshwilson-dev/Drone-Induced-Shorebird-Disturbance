################
#### Header ####
################

# Title: Shorebird Disturbance Analysis: FIA
# Author: Josh Wilson
# Date: 08-08-2021

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

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

# Import Data
data_clean <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

data_fia <- data_clean %>%
    # keep only flights where the drone was advancing
    filter(approach_type == "advancing") %>%
    # keep only flights where the birds flew, or we got within 20m
    filter(behaviour == 1 | xy_disp_m <= 20) %>%
    # degrade data into first instance of maximum behaviour per approach type
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(test, flight, species) %>%
    filter(behaviour == max(behaviour)) %>%
    slice(1) %>%
    # drop drone without enough data to converge
    filter(drone != "inspire 2") %>%
    # drop species without enough data to converge
    group_by(species) %>%
    filter(n() > 3) %>%
    # refactor
    mutate(
    species = factor(species),
    drone = factor(drone),
    flock_number = factor(flock_number),
    location = factor(location),
    tide_type = factor(tide_type),
    migration_prep = factor(migration_prep))
summary(data_fia)

#################
#### Fit gam ####
#################

# Main Effects

# altitude:
# does the behaviour vary with drone altitude?

# species:
# does the behaviour vary with species?

# eastern curlew presence:
# does the behaviour vary with the presence of eastern curlew?

# life stage
# does the behaviour vary with life stage?

# Random Effects

# flock number
# each flock represents a random subset of the entire population

# Interactions

# altitude - species:
# does the relationship between altitude and behaviour vary between species

# altitude - drone:
# does the relationship between altitude and behaviour depend on the drone

gam_fia <- gam(
    behaviour ~
    common_name +
    drone +
    # s(eastern_curlew_abundance) +
    s(count) +
    s(z_disp_m) +
    eastern_curlew_presence +
    # s(month_aest, bs = "cc", k = 7) +
    migration_prep +
    s(tide_height_m, bs = "cc") +
    tide_type +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(rel_wind_dir_d) +
    s(cloud_cover_p) +
    s(location, bs = "re") +
    s(flock_number, bs = "re"),
    data = data_fia,
    family = "binomial",
    method = "REML",
    select = T)

summary(gam_fia)
windows()
visreg(gam_fia, ylim = c(-10, 2))

###########################
#### Creating New Data ####
###########################

common_name_new <- unique(data_fia$common_name)
drone_new <- unique(data_fia$drone)
z_disp_m_new <- seq(min(data_fia$z_disp_m), max(data_fia$z_disp_m), by = 10)
eastern_curlew_prescence_new <- unique(data_fia$eastern_curlew_presence)
month_aest_new <- unique(data_fia$month_aest)
wind_speed_ms_new <- seq(
    min(data_fia$wind_speed_ms),
    max(data_fia$wind_speed_ms),
    by = 1)
could_cover_p_new <- seq(
    min(data_fia$could_cover_p),
    max(data_fia$could_cover_p),
    by = 1)
flock_number_new <- "186"

new_data_fia <- expand.grid(
    common_name = common_name_new,
    drone = drone_new,
    z_disp_m = z_disp_m_new,
    eastern_curlew_presence = eastern_curlew_prescence_new,
    month_aest = month_aest_new,
    wind_speed_ms = wind_speed_ms_new,
    could_cover_p = could_cover_p_new,
    flock_number = flock_number_new)

pred_fia <- predict.gam(gam_fia,
                    new_data_fia,
                    trans = binomial()$linkinv,
                    type = "response",
                    se.fit = TRUE)

results_fia <- new_data_fia %>%
    mutate(
        prediction = pred_fia$fit,
        upper = pred_fia$fit + (2 * pred_fia$se.fit),
        lower = pred_fia$fit - (2 * pred_fia$se.fit))

#############################
#### Visualising Results ####
#############################

data_fia_plot <- results_fia  %>%
    filter(
        drone == "phantom 4 pro",
        eastern_curlew_presence == FALSE,
        month_aest == 8,
        wind_speed_ms == wind_speed_ms_new[length(wind_speed_ms_new) / 2],
        could_cover_p == could_cover_p_new[length(could_cover_p_new) / 2])

# species sensitivity vs height above takeoff
ggplot() +
    theme_set(theme_bw()) +
    facet_wrap(~common_name) +
    geom_line(
        data = data_fia_plot,
        aes(
            z_disp_m,
            prediction,
            group = common_name,
            colour = common_name),
        size = 1.2) +
    geom_ribbon(
        data = data_fia_plot,
        aes(
            z_disp_m,
            ymin = lower,
            ymax = upper,
            group = common_name,
            colour = common_name,
            fill = common_name),
        alpha = 0.05) +
    geom_rug(
        aes(z_disp_m, group = common_name, colour = common_name),
        inherit.aes = FALSE,
        transform(
            filter(data_fia, behaviour == 0),
            expl.name = z_disp_m),
        sides = "b") +
    geom_rug(
        aes(z_disp_m, group = common_name, colour = common_name),
        inherit.aes = FALSE,
        transform(
            filter(data_fia, behaviour == 1),
            expl.name = z_disp_m),
        sides = "t") +
    labs(
        x = "Drone Altitude Above Birds (m)",
        y = "Probability of Inducing Bird Flight",
        fill = "Eastern Curlew Presence",
        colour = "Eastern Curlew Presence") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.text.x = element_text(size = 10, face = "bold"))