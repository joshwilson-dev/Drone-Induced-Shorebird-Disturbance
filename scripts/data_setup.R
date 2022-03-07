################
#### Header ####
################

# Title: Shorebird Disturbance Dataset Setup
# Author: Josh Wilson
# Date: 07-03-2022

###############
#### Setup ####
###############

# Install Packages
packages <- c("tidyverse", "lubridate")
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
data <- read_csv(choose.files(), guess_max = 1000000)

###################################
#### General Data Augmentation ####
###################################

# scientific to common names
sci_com <- data.frame(
    species = c(
        "ardea intermedia",
        "calidris tenuirostris",
        "chroicocephalus novaehollandiae",
        "cygnus atratus",
        "egretta garzetta",
        "egretta novaehollandiae",
        "gelochelidon nilotica",
        "haematopus longirostris",
        "himantopus leucocephalus",
        "hydroprogne caspia",
        "limosa lapponica",
        "numenius madagascariensis",
        "numenius phaeopus",
        "pelecanus conspicillatus",
        "platalea regia",
        "threskiornis molucca",
        "tringa brevipes",
        "tringa stagnatilis",
        "vanellus miles",
        "xenus cinereus"),
    common_name = c(
        "intermediate egret",
        "great knot",
        "silver gull",
        "black swan",
        "little egret",
        "white-faced heron",
        "gull-billed tern",
        "pied oystercatcher",
        "pied stilt",
        "caspian tern",
        "bar-tailed godwit",
        "eastern curlew",
        "whimbrel",
        "australian pelican",
        "royal spoonbill",
        "australian white ibis",
        "grey-tailed tattler",
        "marsh sandpiper",
        "masked lapwing",
        "terek sandpiper"))

data_aug <- data %>%
    mutate(
        # calculate drone acceleration
        z_acc_mss = (`zSpeed(m/s)` - lag(`zSpeed(m/s)`)) / 0.1,
        x_acc_mss = (`xSpeed(m/s)` - lag(`xSpeed(m/s)`))  / 0.1,
        y_acc_mss = (`ySpeed(m/s)` - lag(`ySpeed(m/s)`))  / 0.1,
        xy_acc_mss = (`speed(m/s)` - lag(`speed(m/s)`))  / 0.1,
        # rename drone velocity
        z_vel_ms = `zSpeed(m/s)`,
        x_vel_ms = `xSpeed(m/s)`,
        y_vel_ms = `ySpeed(m/s)`,
        xy_vel_ms = `speed(m/s)`,
        # rename drone displacement
        z_disp_m = `height_above_takeoff(meters)`,,
        # rename drone heading
        heading_d = `compass_heading(degrees)`,
        # rename drone altitude
        drone_latitude_d = latitude,
        drone_longitude_d = longitude,
        # add eastern curlew abundance
        eastern_curlew_abundance = (
            case_when(
                is.na(`numenius madagascariensis count`) ~ 0,
                TRUE ~ `numenius madagascariensis count`)),
        # add eastern curlew presence
        eastern_curlew_presence = (
            case_when(
                is.na(`numenius madagascariensis behaviour`) ~ FALSE,
                TRUE ~ TRUE)),
        # add datetime in aest
        datetime_aest = (
            as_datetime(
                `datetime(utc)` * 60 * 60 * 24,
                origin = "1899/12/30 0:00:00",
                tz = "australia/queensland")),
        # add month integer
        month = month(datetime_aest),
        # add life stage
        lifestage = (
            case_when(
                month < 3 | month > 10 ~ "nonbreeding",
                month > 3 & month < 6 ~ "northwardmigration",
                month > 6 & month < 9 ~ "breeding",
                month > 8 & month < 12 ~ "southwardmigration")),
        # rename tide height
        tide_height_m = `tide height (m)`,
        # rename video time
        video_time_s = `video time (seconds)`,
        # rename tide type
        tide_type = `tide type`,
        # rename temperature
        temperature_dc = `temperature (degrees celcius)`,
        # rename cloud cover
        could_cover_p = `cloud cover (%)`,
        # rename wind speed
        wind_speed_ms = `wind speed (m/s)`,
        # rename wind direction
        wind_dir_d = `wind direction (degrees)`,
        # rename approach type
        approach_type = `approach type`) %>%
    # pivot long so that each species is on a different row
    pivot_longer(
        cols =
        ends_with("behaviour") |
        ends_with("count") |
        ends_with("lat") |
        ends_with("long") |
        ends_with("notes"),
        names_to = c("species", ".value"),
        names_pattern = "(.+) (.+)",
        names_transform = list(species = as.factor),
        values_drop_na = TRUE) %>%
    # add common name
    merge(., sci_com, all.x = TRUE) %>%
    # drop species bird
    filter(species != "bird") %>%
    # convert behaviour to binary
    mutate(
        # convert behaviour to binary
        behaviour = case_when(behaviour == "nominal" ~ 0, TRUE ~ 1),
        # add in distance between drone and birds
        xy_disp_m = (
            6371009 * sqrt(((pi / 180) *
            (lat - latitude))^2 +
            ((cos((pi / 180) * (lat + latitude) / 2) * (pi / 180) *
            (long - longitude))) ^ 2))) %>%
    # drop unused columns
    select(
        test,
        flight,
        video_time_s,
        tide_height_m,
        tide_type,
        temperature_dc,
        could_cover_p,
        wind_speed_ms,
        wind_dir_d,
        drone,
        approach_type,
        datetime_aest,
        latitude,
        longitude,
        species,
        common_name,
        behaviour,
        count,
        lat,
        long,
        notes,
        z_acc_mss,
        x_acc_mss,
        y_acc_mss,
        xy_acc_mss,
        z_vel_ms,
        x_vel_ms,
        y_vel_ms,
        xy_vel_ms,
        z_disp_m,
        heading_d,
        drone_latitude_d,
        drone_longitude_d,
        eastern_curlew_abundance,
        eastern_curlew_presence,
        month,
        lifestage)

##################
#### Save CSV ####
##################

write.csv(
    data_aug,
    "shorebird-disturbance-clean.csv",
    row.names = FALSE)
