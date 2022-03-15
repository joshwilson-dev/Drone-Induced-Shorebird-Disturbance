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
data <- read_csv(choose.files(), guess_max = 100000)

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

gps_loc <- data.frame(
    lat_rnd = c(
        -27.05, -27.05, -27.04, -27.48, -27.48, -27.49, -27.48, -27.54, -27.45),
    lon_rnd = c(
        153.11, 153.12, 153.12, 153.20, 153.21, 153.24, 153.24, 153.28, 153.19),
    location = c(
        "toorbul",
        "toorbul",
        "toorbul",
        "queens esplanade",
        "queens esplanade",
        "geoff skinner",
        "geoff skinner",
        "oyster point",
        "manly")
)

loc_low <- data.frame(
    location = c(
        "oyster point",
        "geoff skinner",
        "oyster point",
        "geoff skinner",
        "queens esplanade",
        "queens esplanade",
        "oyster point",
        "queens esplanade",
        "oyster point",
        "oyster point",
        "queens esplanade",
        "oyster point",
        "oyster point",
        "queens esplanade",
        "geoff skinner",
        "queens esplanade",
        "toorbul",
        "geoff skinner",
        "queens esplanade",
        "queens esplanade",
        "toorbul",
        "queens esplanade",
        "toorbul",
        "toorbul",
        "toorbul",
        "queens esplanade",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "queens esplanade",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "toorbul",
        "manly",
        "queens esplanade"),
    date_aest = as_date(c(
        "2020-04-18",
        "2020-04-19",
        "2020-04-24",
        "2020-04-24",
        "2020-04-26",
        "2020-05-02",
        "2020-05-02",
        "2020-05-03",
        "2020-05-03",
        "2020-05-09",
        "2020-05-11",
        "2020-05-18",
        "2020-05-23",
        "2020-05-25",
        "2021-02-01",
        "2021-02-01",
        "2021-02-03",
        "2021-02-05",
        "2021-02-05",
        "2021-02-08",
        "2021-02-10",
        "2021-02-12",
        "2021-02-15",
        "2021-02-17",
        "2021-02-19",
        "2021-02-24",
        "2021-02-26",
        "2021-07-06",
        "2021-07-11",
        "2021-07-15",
        "2021-07-23",
        "2021-07-28",
        "2021-08-12",
        "2021-08-16",
        "2021-08-19",
        "2021-08-19",
        "2021-08-23",
        "2021-08-27",
        "2021-09-20",
        "2021-09-24",
        "2021-09-28",
        "2021-09-29",
        "2021-09-30",
        "2021-12-30",
        "2022-01-09",
        "2022-01-13",
        "2022-01-13")),
    prev_low_tide = as_datetime(c(
        "2020-04-18 00:30:00",
        "2020-04-19 01:06:00",
        "2020-04-24 04:39:00",
        "2020-04-24 04:20:00",
        "2020-04-26 05:40:00",
        "2020-05-01 22:06:00",
        "2020-05-01 22:17:00",
        "2020-05-02 23:26:00",
        "2020-05-02 23:37:00",
        "2020-05-09 05:25:00",
        "2020-05-11 06:57:00",
        "2020-05-18 00:45:00",
        "2020-05-23 04:23:00",
        "2020-05-25 05:31:00",
        "2021-02-01 05:42:00",
        "2021-02-01 05:50:00",
        "2021-02-03 07:51:00",
        "2021-02-05 09:32:00",
        "2021-02-05 09:40:00",
        "2021-02-08 00:19:00",
        "2021-02-10 02:42:00",
        "2021-02-12 03:50:00",
        "2021-02-15 06:22:00",
        "2021-02-17 07:46:00",
        "2021-02-19 09:39:00",
        "2021-02-24 01:06:00",
        "2021-02-26 03:19:00",
        "2021-07-06 02:20:00",
        "2021-07-11 05:30:00",
        "2021-07-15 07:57:00",
        "2021-07-23 03:54:00",
        "2021-07-28 07:36:00",
        "2021-08-12 06:50:00",
        "2021-08-16 09:29:00",
        "2021-08-19 01:49:00",
        "2021-08-19 13:06:00",
        "2021-08-23 05:14:00",
        "2021-08-27 07:31:00",
        "2021-09-20 04:10:00",
        "2021-09-24 05:23:00",
        "2021-09-28 08:39:00",
        "2021-09-29 09:43:00",
        "2021-09-30 11:00:00",
        "2021-12-30 00:25:00",
        "2022-01-09 08:56:00",
        "2022-01-12 23:59:00",
        "2022-01-12 23:57:00"),
        tz = "australia/queensland"))

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
        z_disp_m = `height_above_takeoff(meters)`,
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
                origin = "1899/12/30 0:00:00.00",
                tz = "australia/queensland")),
        # add month integer
        month_aest = month(datetime_aest),
        # add date
        date_aest = as.Date(datetime_aest, tz = "australia/queensland"),
        # add migration prep
        migration_prep = (
            case_when(
                month_aest > 2 | month_aest < 6 ~ "yes",
                TRUE ~ "no")),
        # rename tide height
        tide_height_m = `tide height (m)`,
        # rename video time
        video_time_s = `video time (seconds)`,
        # rename tide type
        tide_type = `tide type`,
        # rename temperature
        temperature_dc = `temperature (degrees celcius)`,
        # rename cloud cover
        cloud_cover_p = `cloud cover (%)`,
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
            (lat - drone_latitude_d))^2 +
            ((cos((pi / 180) * (lat + drone_latitude_d) / 2) * (pi / 180) *
            (long - drone_longitude_d))) ^ 2)),
        # round latitude and longitude for location
        lat_rnd = round(lat, 2),
        lon_rnd = round(long, 2),
        # add in bearing between drone and birds
        bearing  = (
            (180 / pi) * atan2(
                cos((pi / 180) * drone_latitude_d) *
                sin((pi / 180) * (drone_longitude_d - long)),
                cos((pi / 180) * lat) *
                sin((pi / 180) * drone_latitude_d) -
                sin((pi / 180) * lat) *
                cos((pi / 180) * drone_latitude_d) *
                cos((pi / 180) * (drone_longitude_d - long)))),
        # add in relative wind direction
        rel_wind_dir_d = (
            abs(
                abs(bearing - wind_dir_d) -
                360 * abs(round((bearing - wind_dir_d) / 360))))) %>%
    # add location
    merge(., gps_loc, all.x = TRUE) %>%
    # add previous low tide time
    merge(., loc_low, all.x = TRUE) %>%
    # add time since low tide
    mutate(hrs_since_low_tide = as.numeric(difftime(
        datetime_aest,
        prev_low_tide,
        units = "hours"))) %>%
    # add flock
    group_by(date_aest, location) %>%
    mutate(flock_number = cur_group_id()) %>%
    # ungroup
    group_by() %>%
    # drop unused columns
    select(
        test,
        flight,
        flock_number,
        datetime_aest,
        date_aest,
        prev_low_tide,
        hrs_since_low_tide,
        location,
        video_time_s,
        tide_height_m,
        temperature_dc,
        cloud_cover_p,
        wind_speed_ms,
        wind_dir_d,
        rel_wind_dir_d,
        drone,
        approach_type,
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
        xy_disp_m,
        heading_d,
        drone_latitude_d,
        drone_longitude_d,
        eastern_curlew_abundance,
        eastern_curlew_presence,
        month_aest,
        migration_prep)

##################
#### Save CSV ####
##################

write.csv(
    data_aug,
    "shorebird-disturbance-clean.csv",
    row.names = FALSE)
