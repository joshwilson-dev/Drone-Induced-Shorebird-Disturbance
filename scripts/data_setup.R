################
#### Header ####
################

# Title: Shorebird Disturbance Dataset Setup
# Author: Josh Wilson
# Date: 07-03-2022

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

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
        "intermediate_egret",
        "great_knot",
        "silver_gull",
        "black_swan",
        "little_egret",
        "white_faced_heron",
        "gull_billed_tern",
        "pied_oystercatcher",
        "pied_stilt",
        "caspian_tern",
        "bar_tailed_godwit",
        "eastern_curlew",
        "whimbrel",
        "australian_pelican",
        "royal_spoonbill",
        "australian_white_ibis",
        "grey_tailed_tattler",
        "marsh_sandpiper",
        "masked_lapwing",
        "terek_sandpiper"))

# GPS data to location label
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
        "manly"))

# most recent low tide for each test
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

# the drone clock was set an hour early in the tests below
incorrect_time <- c(
    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,
    54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 68, 69)

# creating clean dataset
data_long <- data %>%
    group_by(test, flight) %>%
    arrange(`video time (seconds)`) %>%
    mutate(
        # set zspeed to zero if it's NA, because it's needed for acceleration
        z_vel_ms = case_when(is.na(`zSpeed(m/s)`) ~ 0, T ~ `zSpeed(m/s)`),
        x_vel_ms = case_when(is.na(`xSpeed(m/s)`) ~ 0, T ~ `xSpeed(m/s)`),
        y_vel_ms = case_when(is.na(`ySpeed(m/s)`) ~ 0, T ~ `ySpeed(m/s)`),
        xy_vel_ms = case_when(is.na(`speed(m/s)`) ~ 0, T ~ `speed(m/s)`),
        # calculate drone acceleration
        z_acc_mss = (
            z_vel_ms -
            lag(z_vel_ms, default = first(z_vel_ms))) /
            0.1,
        x_acc_mss = (
            x_vel_ms -
            lag(x_vel_ms, default = first(x_vel_ms))) /
            0.1,
        y_acc_mss = (
            y_vel_ms -
            lag(y_vel_ms, default = first(y_vel_ms))) /
            0.1,
        xyz_acc_mss = (z_acc_mss**2 + x_acc_mss**2 + y_acc_mss**2)**0.5,
        # rename drone heading
        heading_d = `compass_heading(degrees)`,
        # rename drone displacement
        z_disp_m = `height_above_takeoff(meters)`,
        # rename drone altitude
        drone_latitude_d = latitude,
        drone_longitude_d = longitude,
        # add datetime in aest
        datetime_aest = (
            as_datetime(
                `datetime(utc)` * 60 * 60 * 24,
                origin = "1899/12/30 0:00:00.00",
                tz = "australia/queensland")),
        # subtract 1 hour from the incorrect times
        datetime_aest = case_when(
            test %in% incorrect_time  & flight != 0 ~ datetime_aest - 60 * 60,
            TRUE ~ datetime_aest),
        # add month integer
        month_aest = month(datetime_aest),
        # add date
        date_aest = as.Date(datetime_aest, tz = "australia/queensland"),
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
        # rename wind direction and convert to same coordinate system
        wind_dir_d = (`wind direction (degrees)` + 180) %% 360,
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
    # drop species bird
    filter(species != "bird") %>%
    # add common name
    merge(., sci_com, all.x = TRUE) %>%
    # drop data where alternate disturbance occured
    filter(notes != "alternate disturbance" | is.na(notes)) %>%
    # convert behaviour to binary
    drop_na(behaviour) %>%
    mutate(
        # convert behaviour to binary
        behaviour = case_when(behaviour == "nominal" ~ 0, TRUE ~ 1),
        # add in distance between drone and birds
        xy_disp_m = (
            6371009 * sqrt(((pi / 180) *
            (lat - drone_latitude_d))^2 +
            ((cos((pi / 180) * (lat + drone_latitude_d) / 2) * (pi / 180) *
            (long - drone_longitude_d))) ^ 2)),
        xyz_disp_m = (xy_disp_m**2 + z_disp_m**2)**0.5,
        # round latitude and longitude for location
        lat_rnd = round(lat, 2),
        lon_rnd = round(long, 2),
        # add in bearing between drone and birds
        bearing_d  = (
            (180 / pi) * atan2(
                cos((pi / 180) * lat) *
                sin((pi / 180) * (long - drone_longitude_d)),
                cos((pi / 180) * drone_latitude_d) *
                sin((pi / 180) * lat) -
                sin((pi / 180) * drone_latitude_d) *
                cos((pi / 180) * lat) *
                cos((pi / 180) * (long - drone_longitude_d)))),
        # angle between direction of travel and bearing to birds
        travel_dir_d = ((180 / pi) * atan2(y_vel_ms, x_vel_ms)) %% 360,
        bird_rel_dir_travel_d = (travel_dir_d - bearing_d) %% 360,
        # find drone velocity relative to birds
        xb_vel_ms = (
            x_vel_ms * cos((pi / 180) * bearing_d) +
            y_vel_ms * sin((pi / 180) * bearing_d)),
        # yb symmetric
        yb_vel_ms = (abs(
            y_vel_ms * cos((pi / 180) * bearing_d) -
            x_vel_ms * sin((pi / 180) * bearing_d))),
        # add in relative wind direction
        travel_rel_wind_dir_d = case_when(
            (wind_dir_d - travel_dir_d) %% 360 > 180  ~
            abs((wind_dir_d - travel_dir_d) %% 360) - 360,
            TRUE ~ (wind_dir_d - travel_dir_d) %% 360)) %>%
    # add location
    merge(., gps_loc, all.x = TRUE) %>%
    # add previous low tide time
    merge(., loc_low, all.x = TRUE) %>%
    filter(datetime_aest > prev_low_tide) %>%
    group_by(test, flight, video_time_s, species) %>%
    arrange(desc(prev_low_tide)) %>%
    slice(1) %>%
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
        prev_low_tide,
        hrs_since_low_tide,
        location,
        video_time_s,
        tide_height_m,
        temperature_dc,
        cloud_cover_p,
        wind_speed_ms,
        wind_dir_d,
        travel_rel_wind_dir_d,
        drone,
        species,
        common_name,
        behaviour,
        count,
        lat,
        long,
        notes,
        drone_latitude_d,
        drone_longitude_d,
        travel_dir_d,
        heading_d,
        bearing_d,
        bird_rel_dir_travel_d,
        xyz_acc_mss,
        z_vel_ms,
        x_vel_ms,
        y_vel_ms,
        xy_vel_ms,
        xb_vel_ms,
        yb_vel_ms,
        z_disp_m,
        xy_disp_m,
        xyz_disp_m,
        month_aest) %>%
    # id is identifier for each test, flight, species
    group_by(test, flight, species) %>%
    mutate(id = cur_group_id()) %>%
    # test valid if drone is logging GPS and video is on
    filter(!is.na(xy_disp_m)) %>%
    filter(drone_latitude_d != 0 | drone_latitude_d != 0) %>%
    filter(!is.na(video_time_s)) %>%
    # set start time to drone launch
    mutate(video_time_s = round(video_time_s - first(video_time_s), 1)) %>%
    # add id for merge
    group_by(test, flight, video_time_s) %>%
    mutate(col_id = cur_group_id()) %>%
    # rename video_time
    rename(time = video_time_s)

# add back on species counts
data_wide <- data_long %>%
    pivot_wider(
        id_cols = col_id,
        names_from = common_name,
        names_prefix = "count_",
        values_from = count) %>%
    replace(is.na(.), 0)

data_complete <- merge(data_wide, data_long) %>%
    select(-col_id)

##################
#### Save CSV ####
##################

write.csv(
    data_complete,
    "shorebird-disturbance-clean.csv",
    row.names = FALSE)
