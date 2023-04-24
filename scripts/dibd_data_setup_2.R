################
#### Header ####
################

# Title: Shorebird Disturbance Data Setup
# Author: Josh Wilson
# Date: 29-08-2022

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("readr", "dplyr", "tidyr", "lubridate")
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
data <- read_csv(unz("data/dibd.zip", "dibd.csv"), guess_max = 1000000)

###################################
#### General Data Augmentation ####
###################################

# scientific to common names
sci_com <- data.frame(
    scientific_name = c(
        "ardea alba",
        "ardea intermedia",
        "bubulcus ibis",
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
        "xenus cinereus",
        "calidris ferruginea"
    ),
    species = c(
        "Great Egret",
        "Intermediate Egret",
        "Cattle Egret",
        "Great Knot",
        "Silver Gull",
        "Black Swan",
        "Little Egret",
        "White-faced Heron",
        "Gull-billed Tern",
        "Pied Oystercatcher",
        "Pied Stilt",
        "Caspian Tern",
        "Bar-tailed Godwit",
        "Eastern Curlew",
        "Whimbrel",
        "Australian Pelican",
        "Royal Spoonbill",
        "Australian White Ibis",
        "Grey-tailed Tattler",
        "Marsh Sandpiper",
        "Masked Lapwing",
        "Terek Sandpiper",
        "Curlew Sandpiper"
    ),
    body_mass = c(
        "0.874",
        "0.462",
        "0.366",
        "0.192",
        "0.289",
        "5.650",
        "0.312",
        "0.560",
        "0.264",
        "0.614",
        "0.161",
        "0.655",
        "0.278",
        "0.792",
        "0.352",
        "5.505",
        "1.731",
        "1.806",
        "0.127",
        "0.078",
        "0.387",
        "0.079",
        "0.058")
)

# creating clean dataset
data_long <- data %>%
    # rename tree position
    rename(lat_tree = tree_lat, lon_tree = tree_lon) %>%
    # pivot long so that each species is on a different row
    pivot_longer(
        cols =
        ends_with("behaviour") |
        ends_with("activity") |
        ends_with("count") |
        ends_with("lat") |
        ends_with("long") |
        ends_with("notes"),
        names_to = c("scientific_name", ".value"),
        names_pattern = "(.+) (.+)",
        names_transform = list(scientific_name = as.factor),
        values_drop_na = TRUE)

data_clean <- data_long %>%
    # remove bad data
    filter(
        # remove data when drone isn't flying
        !is.na(time_since_launch),
        # custom drone
        drone != "spibis",
        # any other notes
        is.na(notes)) %>%
    # rename variables
    rename(
        # rename time
        time_since_launch_s = time_since_launch,
        # rename ascent velocity
        velocity_z_m.s = `zSpeed(m/s)`,
        # rename drone heading
        heading_deg = `compass_heading(degrees)`,
        # rename drone position
        drone_latitude = latitude,
        drone_longitude = longitude,
        # rename temperature
        temperature_degC = `temperature (degrees celcius)`,
        # rename cloud cover
        cloud_cover_percent = `cloud cover (%)`,
        # rename wind speed
        wind_speed_m.s = `wind speed (m/s)`,
        # rename bird position
        bird_latitude = lat,
        bird_longitude = long,
        # rename altitude above sea level
        altitude_above_sea_m = `altitude_above_seaLevel(meters)`,
        # rename response
        response = behaviour
    ) %>%
    # add common name
    merge(., sci_com, all.x = TRUE) %>%
    # drop NA response
    drop_na(response) %>%
    # arrange correctly
    arrange(flight, species, time_since_launch_s) %>%
    group_by(flight, species) %>%
    # creat new variables
    mutate(
        # convert response to binary
        response = case_when(response == "nominal" ~ 0, TRUE ~ 1),
        # add in distance between drone and birds
        distance_x_m = (
            6371009 * sqrt(((pi / 180) *
            (bird_latitude - drone_latitude))^2 +
            ((cos((pi / 180) * (bird_latitude + drone_latitude) / 2) *
            (pi / 180) *
            (bird_longitude - drone_longitude))) ^ 2)),
        # add in bearing between drone and birds
        bearing_deg  = (
            (180 / pi) * atan2(
                cos((pi / 180) * bird_latitude) *
                sin((pi / 180) * (bird_longitude - drone_longitude)),
                cos((pi / 180) * drone_latitude) *
                sin((pi / 180) * bird_latitude) -
                sin((pi / 180) * drone_latitude) *
                cos((pi / 180) * bird_latitude) *
                cos((pi / 180) * (bird_longitude - drone_longitude)))),
        # find drone velocity in direction of birds
        velocity_x_m.s = (
            `xSpeed(m/s)` * cos((pi / 180) * bearing_deg) +
            `ySpeed(m/s)` * sin((pi / 180) * bearing_deg)),
        # find drone velocity perpendicular to direction of birds (symmetric)
        velocity_y_m.s = (abs(
            `ySpeed(m/s)` * cos((pi / 180) * bearing_deg) -
            `xSpeed(m/s)` * sin((pi / 180) * bearing_deg))),
        # calculate drone acceleration
        acceleration_z_m.s.s = (
            velocity_z_m.s -
            lag(velocity_z_m.s, default = first(velocity_z_m.s))) /
            0.1,
        acceleration_x_m.s.s = (
            `xSpeed(m/s)` -
            lag(`xSpeed(m/s)`, default = first(`xSpeed(m/s)`))) /
            0.1,
        acceleration_y_m.s.s = (
            `ySpeed(m/s)` -
            lag(`ySpeed(m/s)`, default = first(`ySpeed(m/s)`))) /
            0.1,
        acceleration_xyz_m.s.s = (
            acceleration_y_m.s.s**2 +
            acceleration_x_m.s.s**2 +
            acceleration_z_m.s.s**2)**0.5,
        # change datetime to aest
        datetime_aest = (
            as_datetime(
                `datetime(utc)` * 60 * 60 * 24,
                origin = "1899/12/30 0:00:00.00",
                tz = "australia/queensland")),
        # add date
        date_aest = as.Date(
            datetime_aest,
            tz = "australia/queensland"),
        # add month integer
        month_aest = month(datetime_aest),
        # convert wind direction to same coordinate system as drone
        wind_direction_deg = (
            `wind direction (degrees)`
            + 180) %% 360,
        # add location
        location = case_when(
            bird_latitude > -27.0424 - 0.01 &
            bird_latitude < -27.0424 + 0.01 &
            bird_longitude > 153.1056 - 0.01 &
            bird_longitude < 153.1056 + 0.01 ~ "Toorbul",
            bird_latitude > -27.4796 - 0.01 &
            bird_latitude < -27.4796 + 0.01 &
            bird_longitude > 153.2051 - 0.01 &
            bird_longitude < 153.2051 + 0.01 ~ "Thorneside",
            bird_latitude > -27.5351 - 0.01 &
            bird_latitude < -27.5351 + 0.01 &
            bird_longitude > 153.2837 - 0.01 &
            bird_longitude < 153.2837 + 0.01 ~ "Cleveland",
            bird_latitude > -27.4854 - 0.01 &
            bird_latitude < -27.4854 + 0.01 &
            bird_longitude > 153.2415 - 0.01 &
            bird_longitude < 153.2415 + 0.01 ~ "Wellington Point",
            bird_latitude > -27.0360 - 0.01 &
            bird_latitude < -27.0360 + 0.01 &
            bird_longitude > 153.0530 - 0.01 &
            bird_longitude < 153.0530 + 0.01 ~ "Meldale",
            bird_latitude > -27.4503 - 0.01 &
            bird_latitude < -27.4503 + 0.01 &
            bird_longitude > 153.1878 - 0.01 &
            bird_longitude < 153.1878 + 0.01 ~ "Manly"),
        # calculate altitude above birds
        distance_z_m = case_when(
            location == "Toorbul" ~ altitude_above_sea_m - 0,
            location == "Thorneside" ~ altitude_above_sea_m - 0,
            location == "Cleveland" ~ altitude_above_sea_m - 0,
            location == "Wellington Point" ~ altitude_above_sea_m - 3,
            location == "Meldale" ~ altitude_above_sea_m - 0,
            location == "Manly" ~ altitude_above_sea_m - 0),
        # check if drone obscured by trees
        tree_dist = (
            6371009 * sqrt(((pi / 180) *
            (bird_latitude - lat_tree))^2 +
            ((cos((pi / 180) * (bird_latitude + lat_tree) / 2) * (pi / 180) *
            (bird_longitude - lon_tree))) ^ 2)),
        behind_trees = case_when(
            is.na(tree_dist) |
            (distance_x_m < tree_dist |
            distance_z_m > tree_height *
            distance_x_m / tree_dist) ~ "NO",
            TRUE ~ "YES"),
        # calculate hours to nearest high tide
        hightide_time_aest = (
            as_datetime(
                (`high tide time (aest)` - 10 / 24) * 60 * 60 * 24,
                origin = "1899/12/30 0:00:00.00",
                tz = "australia/queensland")),
        time_to_high_tide_hr = as.numeric(difftime(
            datetime_aest,
            hightide_time_aest,
            units = "hours")),
        # add constant parameters
        life_stage = "non-breeding",
        age = "adult",
        stimulus = drone) %>%
    # add repeat approaches
    group_by(date_aest, location, species) %>%
    mutate(repeat_approaches = flight - first(flight)) %>%
    # crop data to 10s after birds take flight
    group_by(flight, species, response) %>%
    filter(response == 0 | row_number() <= 100) %>%
    # add id for merge
    group_by(flight, time_since_launch_s) %>%
    mutate(col_id = cur_group_id())

# add back on non-target species counts
data_wide_count <- data_clean %>%
    group_by(flight, species) %>%
    pivot_wider(
        id_cols = col_id,
        names_from = species,
        names_prefix = "count_",
        values_from = count) %>%
    replace(is.na(.), 0)

# add back non-target species response
data_wide_response <- data_clean %>%
    group_by(flight, species) %>%
    pivot_wider(
        id_cols = col_id,
        names_from = species,
        names_prefix = "response_",
        values_from = response) %>%
    replace(is.na(.), 0)

data_complete <- data_clean %>%
    merge(., data_wide_count) %>%
    merge(., data_wide_response) %>%
    # approach ends if birds take flight
    group_by(flight, species, response) %>%
    filter(response == 0 | row_number() <= 1) %>%
    # degrade data to every second but keep
    # points where birds took flight
    filter(response == 1 | time_since_launch_s %% 1 == 0) %>%
    # select only needed columns
    select(
        flight,
        time_since_launch_s,
        response,
        species,
        scientific_name,
        count,
        life_stage,
        activity,
        age,
        body_mass,
        bird_latitude,
        bird_longitude,
        contains("count_"),
        contains("response_"),
        stimulus,
        repeat_approaches,
        distance_x_m,
        distance_z_m,
        velocity_x_m.s,
        velocity_y_m.s,
        velocity_z_m.s,
        acceleration_xyz_m.s.s,
        behind_trees,
        location,
        datetime_aest,
        temperature_degC,
        wind_speed_m.s,
        wind_direction_deg,
        cloud_cover_percent,
        time_to_high_tide_hr)

##################
#### Save CSV ####
##################

write.csv(data_complete, "data/dibd_clean.csv", row.names = FALSE)
