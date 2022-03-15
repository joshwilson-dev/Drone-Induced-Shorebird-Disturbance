################
#### Header ####
################

# Title: Shorebird Disturbance Analysis: FIT
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
data_fit <- data_clean %>%
    # keep only flights where the drone was ascending
    filter(approach_type == "ascending") %>%
    # change ascent distance to the most common value for entire approach
    group_by(test, flight) %>%
    mutate(xy_disp_m = which.max(tabulate(xy_disp_m))) %>%
    # degrade data into first instance of flight, or data from maximum
    # altitude if flight didn't occur
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(test, flight, species) %>%
    filter(behaviour == max(behaviour)) %>%
    filter(behaviour == 1 | z_disp_m == max(z_disp_m)) %>%
    arrange(video_time_s) %>%
    slice(1) %>%
    # drop eastern curlew to enable eastern curlew presence
    # filter(common_name != "eastern curlew") %>%
    # refactor
    mutate(
    species = factor(species),
    common_name = factor(common_name),
    drone = factor(drone),
    flock_number = factor(flock_number),
    location = factor(location),
    test = factor(test))
summary(data_fit)

#################
#### Fit gam ####
#################

# Main Effects
# Question: at what altitude does flight not occur?

# drone:
# does the behaviour vary with drone type?

# altitude:
# does the behaviour vary with drone altitude?

# species:
# does the behaviour vary with species

gam_fit <- gam(
    behaviour ~
    common_name +
    drone +
    s(count) +
    s(xy_disp_m) +
    eastern_curlew_presence +
    # s(eastern_curlew_abundance) +
    s(month_aest, bs = "cc", k = 7) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(rel_wind_dir_d) +
    s(cloud_cover_p) +
    s(location, bs = "re") +
    # s(test, bs = "re") +
    s(flock_number, bs = "re"),
    data = data_fit,
    family = "binomial",
    method = "REML",
    select = T)

summary(gam_fit)
windows()
visreg(gam_fit)
###########################
#### Creating New Data ####
###########################

common_name_new <- unique(data_fit$common_name)
drone_new <- unique(data_fit$drone)
xy_disp_m_new <- seq(min(data_fit$xy_disp_m), max(data_fit$xy_disp_m), by = 10)
eastern_curlew_prescence_new <- unique(data_fit$eastern_curlew_presence)
month_aest_new <- unique(data_fit$month_aest)
wind_speed_ms_new <- seq(
    min(data_fit$wind_speed_ms),
    max(data_fit$wind_speed_ms),
    by = 1)
could_cover_p_new <- seq(
    min(data_fit$could_cover_p),
    max(data_fit$could_cover_p),
    by = 1)
flock_number_new <- "186"

new_data_fit <- expand.grid(
    common_name = common_name_new,
    drone = drone_new,
    z_disp_m = z_disp_m_new,
    eastern_curlew_presence = eastern_curlew_prescence_new,
    month_aest = month_aest_new,
    wind_speed_ms = wind_speed_ms_new,
    could_cover_p = could_cover_p_new,
    flock_number = flock_number_new)

pred_fia <- predict.gam(gam_fit,
                    new_data_fit,
                    trans = binomial()$linkinv,
                    type = "response",
                    se.fit = TRUE)

results_fit <- new_data_fit %>%
    mutate(
        prediction = pred_fit$fit,
        upper = pred_fit$fit + (2 * pred_fit$se.fit),
        lower = pred_fit$fit - (2 * pred_fit$se.fit))

#############################
#### Visualising Results ####
#############################

data_fit_plot <- results_fit  %>%
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
        data = data_fit_plot,
        aes(
            xy_disp_m,
            prediction,
            group = common_name,
            colour = common_name),
        size = 1.2) +
    geom_ribbon(
        data = data_fit_plot,
        aes(
            xy_disp_m,
            ymin = lower,
            ymax = upper,
            group = common_name,
            colour = common_name,
            fill = common_name),
        alpha = 0.05) +
    geom_rug(
        aes(xy_disp_m, group = common_name, colour = common_name),
        inherit.aes = FALSE,
        transform(
            filter(data_fit, behaviour == 0),
            expl.name = xy_disp_m),
        sides = "b") +
    geom_rug(
        aes(z_disp_m, group = common_name, colour = common_name),
        inherit.aes = FALSE,
        transform(
            filter(data_fit, behaviour == 1),
            expl.name = xy_disp_m),
        sides = "t") +
    labs(
        x = "Drone Launch Distance (m)",
        y = "Probability of Inducing Bird Flight",
        fill = "Eastern Curlew Presence",
        colour = "Eastern Curlew Presence") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.text.x = element_text(size = 10, face = "bold"))