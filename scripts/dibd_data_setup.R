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
packages <- c("tidyverse", "lubridate", "zoo")
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
        "xenus cinereus"
    ),
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
        "terek_sandpiper"
    ),
    species_sensitivity = c(
        NA,
        2,
        NA,
        1,
        NA,
        NA,
        1,
        1,
        1,
        1,
        2,
        3,
        2,
        2,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    )
)

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
    high_tide = as_datetime(c(
        "2020-04-18 06:48:00",
        "2020-04-19 07:14:00",
        "2020-04-24 10:21:00",
        "2020-04-24 10:02:00",
        "2020-04-26 11:14:00",
        "2020-05-02 04:38:00",
        "2020-05-02 04:49:00",
        "2020-05-03 05:45:00",
        "2020-05-03 05:56:00",
        "2020-05-09 10:57:00",
        "2020-05-11 12:21:00",
        "2020-05-18 06:45:00",
        "2020-05-23 09:53:00",
        "2020-05-25 10:56:00",
        "2021-02-01 11:59:00",
        "2021-02-01 12:07:00",
        "2021-02-03 14:09:00",
        "2021-02-05 15:17:00",
        "2021-02-05 15:25:00",
        "2021-02-08 07:04:00",
        "2021-02-10 09:34:00",
        "2021-02-12 10:13:00",
        "2021-02-15 12:50:00",
        "2021-02-17 13:52:00",
        "2021-02-19 15:14:00",
        "2021-02-24 07:40:00",
        "2021-02-26 09:56:00",
        "2021-07-06 07:42:00",
        "2021-07-11 10:58:00",
        "2021-07-15 13:46:00",
        "2021-07-23 09:26:00",
        "2021-07-28 13:25:00",
        "2021-08-12 12:43:00",
        "2021-08-16 16:27:00",
        "2021-08-19 07:15:00",
        "2021-08-19 07:15:00",
        "2021-08-23 10:58:00",
        "2021-08-27 13:39:00",
        "2021-09-20 10:03:00",
        "2021-09-24 11:27:00",
        "2021-09-28 15:23:00",
        "2021-09-29 16:35:00",
        "2021-09-30 17:57:00",
        "2021-12-30 07:11:00",
        "2022-01-09 15:18:00",
        "2022-01-13 06:37:00",
        "2022-01-13 06:37:00"),
        tz = "australia/queensland"),
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
    # remove data when drone isn't flying
    filter(!is.na(time_since_launch)) %>%
    # rename variables
    rename(
        # rename velocities
        z_vel_ms = `zSpeed(m/s)`,
        x_vel_ms = `xSpeed(m/s)`,
        y_vel_ms = `ySpeed(m/s)`,
        xy_vel_ms = `speed(m/s)`,
        # rename drone heading
        heading_d = `compass_heading(degrees)`,
        # rename height
        z_disp_m = `height_above_takeoff(meters)`,
        # rename drone position
        drone_latitude_d = latitude,
        drone_longitude_d = longitude,
        # rename temperature
        temperature_dc = `temperature (degrees celcius)`,
        # rename cloud cover
        cloud_cover_p = `cloud cover (%)`,
        # rename wind speed
        wind_speed_ms = `wind speed (m/s)`,
        # rename tree position
        lat_tree = tree_lat,
        lon_tree = tree_lon,
        # rename flight to approach
        approach = flight,
    ) %>%
    mutate(
        # calculate flight number
        flight = case_when(
            test > 33 ~ paste0(test, approach),
            TRUE ~ as.character(test))) %>%
    group_by(flight) %>%
    # create new variables
    mutate(
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
        # create date in aest
        datetime_aest = (
            as_datetime(
                `datetime(utc)` * 60 * 60 * 24,
                origin = "1899/12/30 0:00:00.00",
                tz = "australia/queensland")),
        # subtract 1 hour from the incorrect times
        datetime_aest = case_when(
            test %in% incorrect_time ~ datetime_aest - 60 * 60,
            TRUE ~ datetime_aest),
        # add date
        date_aest = as.Date(datetime_aest, tz = "australia/queensland"),
        # add month integer
        month_aest = month(datetime_aest),
        # rename wind direction and convert to same coordinate system
        wind_dir_d = (`wind direction (degrees)` + 180) %% 360
    ) %>%
    # pivot long so that each species is on a different row
    pivot_longer(
        cols =
        ends_with("sentinel") |
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
    # drop NA behaviour
    drop_na(behaviour) %>%
    group_by(flight) %>%
    # creat new variables
    mutate(
        # add migrating for migratory shorebirds
        migrating = case_when(
            (month_aest == 4 | month_aest == 5) &
            (common_name == "eastern_curlew" |
            common_name == "whimbrel" |
            common_name == "bar_tailed_godwit" |
            common_name == "great_knot") ~ "migrating",
            TRUE ~ "not migrating"),
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
        travel_dir_d = case_when(
            xy_vel_ms <= 0.3 ~ heading_d,
            TRUE ~ travel_dir_d),
        horizontal_approach_angle = abs(travel_dir_d - bearing_d) %% 180,
        # find drone velocity relative to birds
        xb_vel_ms = (
            x_vel_ms * cos((pi / 180) * bearing_d) +
            y_vel_ms * sin((pi / 180) * bearing_d)),
        # yb symmetric
        yb_vel_ms = (abs(
            y_vel_ms * cos((pi / 180) * bearing_d) -
            x_vel_ms * sin((pi / 180) * bearing_d))),
        # add in relative wind direction
        track_rel_wind_dir_d = (wind_dir_d - travel_dir_d) %% 180) %>%
    # add location
    merge(., gps_loc, all.x = TRUE) %>%
    # add tides
    merge(., loc_low, all.x = TRUE) %>%
    # add time since high tide
    mutate(hrs_from_high = as.numeric(difftime(
        datetime_aest,
        high_tide,
        units = "hours"))) %>%
    # id is identifier for each flight, species
    group_by(flight, common_name) %>%
    mutate(id = cur_group_id()) %>%
    # add id for merge
    group_by(flight, time_since_launch) %>%
    mutate(col_id = cur_group_id()) %>%
    # smooth acceleration over 0.5s
    group_by(id) %>%
    arrange(id, time_since_launch) %>%
    mutate(xyz_acc_mss = rollapply(xyz_acc_mss, 5, mean, fill = "extend")) %>%
    # check if drone obscured by trees
    mutate(tree_dist =
            6371009 * sqrt(((pi / 180) *
            (lat - lat_tree))^2 +
            ((cos((pi / 180) * (lat + lat_tree) / 2) * (pi / 180) *
            (long - lon_tree))) ^ 2)) %>%
    mutate(drone_obscured = case_when(
        is.na(tree_dist) |
        (xy_disp_m < tree_dist |
        z_disp_m > tree_height * xy_disp_m / tree_dist) ~ "not obscured",
        TRUE ~ "obscured")) %>%
    # exclude bad approaches
    filter(is.na(notes)) %>%
    # add flock
    group_by(date_aest, location) %>%
    mutate(flock = cur_group_id()) %>%
    # fix sentinel flight proportion
    mutate(sentinel_flight_proportion = case_when(
        sentinel_flight_proportion == "scattered" ~ "scattered",
        TRUE ~ "complete")) %>%
    # add flock approach
    group_by(flock) %>%
    mutate(flock_approach = cumsum(!duplicated(flight))) %>%
    # normalise count
    group_by(common_name) %>%
    mutate(normalised_count = count / max(count)) %>%
    # drop unused columns
    select(
        id,
        col_id,
        test,
        flight,
        approach,
        time_since_launch,
        # target
        sentinel,
        common_name,
        behaviour,
        count,
        normalised_count,
        sentinel_flight_proportion,
        flock,
        flock_approach,
        # environemnt
        datetime_aest,
        date_aest,
        hrs_from_high,
        location,
        temperature_dc,
        cloud_cover_p,
        wind_speed_ms,
        wind_dir_d,
        month_aest,
        migrating,
        # stimulus
        drone,
        horizontal_approach_angle,
        travel_dir_d,
        bearing_d,
        heading_d,
        drone_latitude_d,
        drone_longitude_d,
        lat,
        long,
        xyz_acc_mss,
        z_vel_ms,
        x_vel_ms,
        y_vel_ms,
        xy_vel_ms,
        xb_vel_ms,
        yb_vel_ms,
        z_disp_m,
        xy_disp_m,
        drone_obscured)

# add back on species counts
data_wide_count <- data_long %>%
    group_by(flight, common_name) %>%
    pivot_wider(
        id_cols = col_id,
        names_from = common_name,
        names_prefix = "count_",
        values_from = count) %>%
    replace(is.na(.), 0)

data_wide_behaviour <- data_long %>%
    pivot_wider(
        id_cols = col_id,
        names_from = common_name,
        names_prefix = "behaviour_",
        values_from = behaviour) %>%
    replace(is.na(.), 0)

data_complete <- merge(data_wide_count, data_long) %>%
    merge(., data_wide_behaviour) %>%
    mutate(
        common_name = str_replace(common_name, "_", " "),
        common_name = str_replace(common_name, "_", " ")) %>%
    mutate(flock_count = rowSums(select(., contains("count_")))) %>%
    mutate(sum_behaviour = rowSums(select(., contains("behaviour_")))) %>%
    # if another species takes off, sentinel flight is name of that species
    group_by(flight, time_since_launch) %>%
    mutate(
        sentinel_flight = case_when(
            sum_behaviour == 1 &
            behaviour == 1 ~ common_name,
            TRUE ~ "a"),
        sentinel_flight = max(sentinel_flight)) %>%
    group_by(flight, common_name) %>%
    mutate(sentinel_flight2 = sentinel_flight) %>%
    mutate(sentinel_flight2 = na_if(sentinel_flight, "a")) %>%
    fill(sentinel_flight2) %>%
    mutate(
        sentinel_flight = case_when(
            sum_behaviour > 0 &
            max(sentinel_flight) != "a" &
            sentinel_flight != common_name ~ sentinel_flight2,
            TRUE ~ sentinel_flight),
        sentinel_flight = case_when(
            sentinel_flight == common_name ~ "a",
            TRUE ~ sentinel_flight)) %>%
    group_by(flight, common_name) %>%
    # sentinel flight only lasts 5 seconds
    mutate(
        sentinel_flight = case_when(
            lag(sentinel_flight, 50, default = "a") == "a" ~ sentinel_flight,
            TRUE ~ "a")) %>%
    # add sentinel count
    group_by(flight, time_since_launch) %>%
    mutate(
        sentinel_count = case_when(
            sum_behaviour == 1 &
            behaviour == 1 ~ count,
            TRUE ~ 0),
        sentinel_count = max(sentinel_count)) %>%
    group_by(flight, common_name) %>%
    mutate(sentinel_count2 = sentinel_count) %>%
    mutate(sentinel_count2 = na_if(sentinel_count, 0)) %>%
    fill(sentinel_count2) %>%
    mutate(
        sentinel_count = case_when(
            sentinel_flight != "a" ~ sentinel_count2,
            TRUE ~ 0)) %>%
    select(-sentinel_count2, -sentinel_flight2, -sum_behaviour)

# adding other variables
data_final <- data_complete %>%
    # degrade data to every second to make the file smaller
    filter(time_since_launch %% 1 == 0) %>%
    # select only some species
    filter(
        common_name == "eastern curlew" |
        common_name == "whimbrel" |
        common_name == "bar tailed godwit" |
        common_name == "gull billed tern" |
        common_name == "great knot" |
        common_name == "caspian tern" |
        common_name == "pied stilt" |
        common_name == "pied oystercatcher" |
        common_name == "black swan") %>%
    # remove flights where sentinel impacted results except eastern curlew
    group_by(flight, common_name) %>%
    mutate(sentinel = case_when(is.na(sentinel) ~ "a", T ~ sentinel)) %>%
    mutate(sentinel_check = case_when(
        sentinel_flight == "a" | sentinel == "eastern curlew" ~ 0,
        TRUE ~ 1)) %>%
    filter(max(sentinel_check) == 0) %>%
    # change species if eastern curlew took flight
    mutate(sentinel_check = case_when(
        sentinel == "eastern curlew" ~ 1,
        TRUE ~ 0
    )) %>%
    group_by(flight, common_name) %>%
    mutate(common_name = case_when(
        max(sentinel_check) == 1 ~ paste(common_name, "with curlew"),
        TRUE ~ common_name
    )) %>%
    # approach ends if birds take flight
    group_by(flight, common_name, behaviour) %>%
    filter(behaviour == 0 | row_number() <= 1) %>%
    ungroup() %>%
    mutate(
        species = common_name,
        count = count,
        latitude = lat,
        longitude = long,
        flock = flock,
        life_stage = "non-breeding",
        activity = "roosting",
        age = "adult",
        response = behaviour,
        type = "drone",
        shape = "quadcopter",
        size = case_when(
            drone == "inspire 2" ~ 0.6,
            drone == "mavic 2 pro" ~ 0.35,
            drone == "phantom 4 pro" ~ 0.35,
            drone == "mavic mini" ~ 0.35),
        colour = case_when(
            drone == "inspire 2" ~ "grey",
            drone == "mavic 2 pro" ~ "grey",
            drone == "phantom 4 pro" ~ "white",
            drone == "mavic mini" ~ "grey"),
        noise = case_when(
            drone == "inspire 2" ~ 78,
            drone == "mavic 2 pro" ~ 74,
            drone == "phantom 4 pro" ~ 75,
            drone == "mavic mini" ~ 70),
        specification = drone,
        distance_x = xy_disp_m,
        distance_z = z_disp_m,
        velocity_x = xb_vel_ms,
        velocity_y = yb_vel_ms,
        velocity_z = z_vel_ms,
        acceleration = xyz_acc_mss,
        location = location,
        habitat = "IUCN:MT1.2",
        datetime = datetime_aest,
        temperature = temperature_dc,
        wind_speed = wind_speed_ms,
        wind_direction = wind_dir_d,
        cloud_cover = cloud_cover_p,
        high_tide = hrs_from_high,
        obscuring = drone_obscured,
        background_noise = 50,
        light = 100000) %>%
    filter(across(contains("count_"), ~ !max(0))) %>%
    select(
        test,
        approach,
        flight,
        time_since_launch,
        response,
        species,
        flock,
        sentinel,
        count,
        life_stage,
        activity,
        age,
        contains("count_"),
        type,
        specification,
        size,
        shape,
        colour,
        noise,
        distance_x,
        distance_z,
        velocity_x,
        velocity_y,
        velocity_z,
        acceleration,
        latitude,
        longitude,
        location,
        habitat,
        datetime,
        temperature,
        wind_speed,
        wind_direction,
        cloud_cover,
        high_tide,
        obscuring,
        background_noise,
        light)

##################
#### Save CSV ####
##################

write.csv(
    data_final,
    "data/dibd_data.csv",
    row.names = FALSE)
