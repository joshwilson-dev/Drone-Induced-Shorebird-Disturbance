################
#### Header ####
################

# Title: Shorebird Disturbance Analysis: FID
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

data_fid <- data_clean %>%
    # drop control data
    filter(approach_type != "control") %>%
    # keep only first instance of flight for each approach and species
    filter(behaviour == 1) %>%
    group_by(test, flight, species) %>%
    arrange(video_time_s) %>%
    slice(1) %>%
    # drop low tide data as it is innaccurate
    filter(
        notes !=
        "low tide count, species and gps may be inaccurate" |
        is.na(notes)) %>%
    # need to take out eastern curlew to fit effect of ec presence
    filter(common_name != "eastern curlew") %>%
    # refactor
    mutate(
    species = factor(species),
    drone = factor(drone),
    flock_number = factor(flock_number),
    location = factor(location))
summary(data_fid)

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

gam_fid <- gam(
    xy_disp_m ~
    common_name +
    drone +
    # s(count) +
    s(z_disp_m) +
    eastern_curlew_presence +
    # s(month_aest, bs = "cc", k = 7) +
    s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    # s(wind_speed_ms) +
    # s(rel_wind_dir_d) +
    # s(cloud_cover_p) +
    s(flock_number, bs = "re"),
    data = data_fid,
    family = gaussian(),
    method = "REML",
    select = T)

summary(gam_fid)
windows()
visreg(gam_fid)

###########################
#### Creating New Data ####
###########################
common_name_new <- unique(data_fid$common_name)
drone_new <- unique(data_fid$drone)
z_disp_m_new <- seq(0, 120, by = 10)
eastern_curlew_presence_new <- unique(data_fid$eastern_curlew_presence)
count_new <- seq(0, 1000, by = 100)
month_aest_new <- seq(0, 12, by = 1)
hrs_since_low_tide_new <- seq(0, 12, by = 1)
temperature_dc_new <- seq(15, 30, by = 2)
wind_speed_ms_new <- seq(0, 10, by = 1)
rel_wind_dir_d_new <- seq(0, 180, by = 20)
cloud_cover_p_new <- seq(0, 100, by = 20)
flock_number_new <- unique(data_fid$flock_number)

ndata <- expand.grid(
    common_name = common_name_new,
    drone = drone_new,
    z_disp_m = z_disp_m_new,
    eastern_curlew_presence = eastern_curlew_presence_new,
    # count = count_new,
    # month_aest = month_aest_new,
    hrs_since_low_tide = hrs_since_low_tide_new,
    # temperature_dc = temperature_dc_new,
    # wind_speed_ms = wind_speed_ms_new,
    # rel_wind_dir_d = rel_wind_dir_d_new,
    # cloud_cover_p = cloud_cover_p_new,
    flock_number = flock_number_new)

# grab the inverse link function
ilink <- family(gam_fid)$linkinv

# add the fitted values by predicting from the model for the new data
ndata <- add_column(
    ndata,
    fit = predict(
        gam_fid,
        exclude = c("s(flock_number)"),
        newdata = ndata,
        trans = binomial()$ilink,
        type = "response"))

# add fit and se.fit on the link scale
ndata <- bind_cols(
    ndata,
    setNames(as_tibble(predict(
        gam_fid,
        exclude = c("s(flock_number)"),
        newdata = ndata,
        trans = binomial()$linkinv,
        se.fit = TRUE)[1:2]),
        c("fit_link", "se_link")))

# create the interval and backtransform
results_fid <- mutate(
    ndata,
    prediction = ilink(fit_link),
    upper = ilink(fit_link + (2 * se_link)),
    lower = ilink(fit_link - (2 * se_link)))

#############################
#### Visualising Results ####
#############################

data_fid_plot <- results_fid  %>%
    filter(
        drone == "mavic 2 pro",
        eastern_curlew_presence == FALSE,
        hrs_since_low_tide == 6)

ggplot() +
    theme_set(theme_bw()) +
    facet_wrap(~common_name) +
    geom_line(
        data = data_fid_plot,
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
    geom_point(
        data = filter(data_fid, drone == "mavic 2 pro"),
        aes(z_disp_m,
            xy_disp_m,
            group = common_name,
            colour = common_name))
    # coord_flip(ylim = c(0, 260), xlim = c(0, 120)) +
    # labs(x = "Drone Altitude (m)", y = "Flight Initiation Distance (m)") +
    # theme(
    #     axis.text = element_text(face = "bold", color = "black"),
    #     axis.title = element_text(size = 14, face = "bold"),
    #     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    #     legend.position = "none",
    #     aspect.ratio = 0.45) +
    # scale_x_continuous(
    #     minor_breaks = round(seq(0, 140, 20)),
    #     breaks = round(seq(0, 140, by = 20), 1),
    #     expand = c(0, 0)) +
    # scale_y_continuous(
    #     minor_breaks = round(seq(-400, 400, 20)),
    #     breaks = round(seq(-400, 400, by = 20), 1),
    #     expand = c(0, 0))

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
