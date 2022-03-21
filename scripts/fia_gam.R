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
    # change altitude to the most common value for entire approach
    group_by(test, flight) %>%
    mutate(z_disp_m = which.max(tabulate(z_disp_m))) %>%
    # for each approach, degrade data into first instance of flight,
    # or data from closest approach distance, provided it is less than 20m
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(test, flight, species) %>%
    filter(behaviour == max(behaviour)) %>%
    filter(behaviour == 1 | xy_disp_m == min(xy_disp_m)) %>%
    filter(behaviour == 1 | xy_disp_m <= 20) %>%
    arrange(video_time_s) %>%
    slice(1) %>%
    # drop eastern curlew true as because their presence significantly
    # affects results (based on fid) and we can't include in gam because
    # approaches stopped once EC took flight.
    filter(
        common_name == "eastern curlew" |
        eastern_curlew_presence == FALSE) %>%
    # refactor
    mutate(
    species = factor(species),
    drone = factor(drone),
    flock_number = factor(flock_number),
    location = factor(location),
    test = factor(test))
summary(data_fia)

#################
#### Fit gam ####
#################

# Main Effects

# common_name:
# does the behaviour vary with species?

# drone:
# does the behaviour vary with the type of drone?

# z_disp_m:
# does the behaviour vary with drone altitude?

# count:
# does the behaviour vary with flock size?

# month_aest

# hrs_since_low_tide

# temperature_dc
# does the behaviour vary with temperature?

# wind_speed_ms
# rel_wind_dir_d
# cloud_cover_p

# Random Effects

# flock number
# each flock represents a random subset of the entire population

gam_fia <- gam(
    behaviour ~
    common_name +
    drone +
    s(z_disp_m) +
    # s(count) +
    # s(month_aest, bs = "cc", k = 7) +
    # s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    s(wind_speed_ms) +
    s(rel_wind_dir_d) +
    # s(cloud_cover_p) +
    s(flock_number, bs = "re"),
    data = data_fia,
    family = "binomial",
    method = "REML",
    select = T)

summary(gam_fia)
windows()
visreg(gam_fia)

###########################
#### Creating New Data ####
###########################
common_name_new <- unique(data_fia$common_name)
drone_new <- unique(data_fia$drone)
z_disp_m_new <- seq(0, 120, by = 10)
count_new <- seq(0, 1000, by = 100)
month_aest_new <- seq(0, 12, by = 1)
hrs_since_low_tide_new <- seq(0, 12, by = 1)
temperature_dc_new <- seq(15, 30, by = 2)
wind_speed_ms_new <- seq(0, 10, by = 1)
rel_wind_dir_d_new <- seq(0, 180, by = 20)
cloud_cover_p_new <- seq(0, 100, by = 20)
flock_number_new <- unique(data_fia$flock_number)

ndata <- expand.grid(
    common_name = common_name_new,
    drone = drone_new,
    z_disp_m = z_disp_m_new,
    # count = count_new,
    # month_aest = month_aest_new,
    # hrs_since_low_tide = hrs_since_low_tide_new,
    # temperature_dc = temperature_dc_new,
    wind_speed_ms = wind_speed_ms_new,
    rel_wind_dir_d = rel_wind_dir_d_new,
    # cloud_cover_p = cloud_cover_p_new,
    flock_number = flock_number_new)

# grab the inverse link function
ilink <- family(gam_fia)$linkinv

# add the fitted values by predicting from the model for the new data
ndata <- add_column(
    ndata,
    fit = predict(
        gam_fia,
        exclude = c("s(flock_number)"),
        newdata = ndata,
        trans = binomial()$linkinv,
        type = "response"))

# add fit and se.fit on the link scale
ndata <- bind_cols(
    ndata,
    setNames(as_tibble(predict(
        gam_fia,
        exclude = c("s(flock_number)"),
        newdata = ndata,
        trans = binomial()$linkinv,
        se.fit = TRUE)[1:2]),
        c("fit_link", "se_link")))

# create the interval and backtransform
results_fia <- mutate(
    ndata,
    prediction = ilink(fit_link),
    upper = ilink(fit_link + (2 * se_link)),
    lower = ilink(fit_link - (2 * se_link)))

#############################
#### Visualising Results ####
#############################

data_fia_plot <- results_fia  %>%
    filter(
        drone == "mavic 2 pro",
        wind_speed_ms == 2,
        rel_wind_dir_d == 80)

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
        strip.text.x = element_text(size = 10, face = "bold")) +
    ylim(0, 1)
