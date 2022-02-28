#### Header ####
# Title: Shorebird Disturbance Analysis
# Author: Josh Wilson
# Date: 08-08-2021

#### Setup ####
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
library(tidyverse)
library(mgcv)
library(visreg)
library(lubridate)

# Clear Environment
rm(list = ls())

# Import Data
data <- read_csv(choose.files(), guess_max = 1000000)

#### general data augmentation ####

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
    # add datetime in aest
    mutate(
        datetime_aest =
        as_datetime(
            `datetime(utc)` * 60 * 60 * 24,
            origin = "1899/12/30 0:00:00",
            tz = "australia/queensland")) %>%
    # add month integer
    mutate(month = month(datetime_aest)) %>%
    # add life stage
    mutate(
        lifestage =
        as.factor(
            case_when(
                month < 3 | month > 10 ~ "nonbreeding",
                month > 3 & month < 6 ~ "northwardmigration",
                month > 6 & month < 9 ~ "breeding",
                month > 8 & month < 12 ~ "southwardmigration")
                )
     ) %>%
    # add flightcode to group by
    mutate(flightcode = paste0(as.character(test), as.character(flight))) %>%
    # group by flight code
    group_by(flightcode) %>%
    # add column for eastern curlew presence
    mutate(
        `eastern.curlew.presence` =
        as.factor(case_when(
            (is.na(`numenius madagascariensis behaviour`)) ~ FALSE,
            (TRUE ~ TRUE)))) %>%
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
    # add a column with distance between drone and birds
    mutate(
        drone_bird_distance = 6371009 * sqrt(((pi / 180) *
        (lat - latitude))^2 +
        ((cos((pi / 180) * (lat + latitude) / 2) * (pi / 180) *
        (long - longitude))) ^ 2)) %>%
    # convert behaviour to binary
    mutate(
        behaviour =
        case_when(
            behaviour == "nominal" ~ 1,
            behaviour == "flight" ~ 2,
            behaviour == "landed" ~ 3,
            TRUE ~ 0)) %>%
    # degrade data into first instance of maximum behaviour per approach type
    mutate(
        behaviour =
        factor(
            behaviour,
            levels = c(0, 1, 2, 3),
            ordered = TRUE)) %>%
    group_by(flightcode, species, `approach type`) %>%
    filter(behaviour == max(behaviour)) %>%
    slice(1) %>%
    # make column names correct format for gam
    rename_all(make.names)

#### fia analysis ####
data_fia <- data_aug %>%
    # keep only flights where the drone was advancing
    filter(approach.type == "advancing") %>%
    # drop the landed behaviour state
    filter(behaviour != 3) %>%
    # keep only the maximum behaviour state
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(flightcode, species) %>%
    filter(behaviour == max(behaviour)) %>%
    # filter out species without enough data
    group_by(species) %>%
    filter(n() > 25) %>%
    filter(drone != "inspire 2", drone != "phatom 4 pro")

# fit GAM
# Question: at what altitude does flight not occur?

# Main Effects

# altitude:
# does the behaviour vary with drone altitude?

# species:
# does the behaviour vary with species

# eastern curlew presence:
# does the behaviour vary with the presence of eastern curlew

# life stage
# does the behaviour vary with life stage

# Interactions

# altitude - species:
# does the relationship between altitude and behaviour vary between species

# altitude - drone:
# does the relationship between altitude and behaviour depend on the drone

gam_fia <- gam(
    behaviour
    ~ species +
    s(height_above_takeoff.meters.) +
    eastern.curlew.presence,
    data = data_fia,
    family = "binomial",
    method = "REML",
    select = T)
summary(gam_fia)

# Creating new data

altitude_fia <- seq(0, 120, by = 1)
species_fia <- unique(data_fia$species)
eastern_curlew_prescence_fia <- unique(data_fia$eastern.curlew.presence)

new_data_fia <- expand.grid(
    height_above_takeoff.meters. = altitude_fia,
    species = species_fia,
    eastern.curlew.presence = eastern_curlew_prescence_fia,
    month = month_fia)

pred_fia <- predict.gam(gam_fia,
                    new_data_fia,
                    trans = binomial()$linkinv,
                    type = "response",
                    se.fit = TRUE)

results_fia <- new_data_fia %>%
    mutate(prediction = pred_fia$fit) %>%
    mutate(upper = pred_fia$fit + (2 * pred_fia$se.fit)) %>%
    mutate(lower = pred_fia$fit - (2 * pred_fia$se.fit))

# species sensitivity vs height above takeoff

ggplot() +
    theme_set(theme_bw()) +
    geom_line(
        data = filter(results_fia, eastern.curlew.presence == FALSE),
        aes(
            height_above_takeoff.meters.,
            prediction,
            group = species,
            colour = species),
        size = 1.2) +
    geom_ribbon(
        data = filter(results_fia, eastern.curlew.presence == FALSE),
        aes(
            height_above_takeoff.meters.,
            ymin = lower,
            ymax = upper,
            group = species,
            colour = species,
            fill = species),
        alpha = 0.05) +
    facet_wrap(~species) +
    geom_rug(
        aes(height_above_takeoff.meters., group = species, colour = species),
        inherit.aes = FALSE,
        transform(
            filter(data_fia, behaviour == 0),
            expl.name = height_above_takeoff.meters.),
        sides = "b") +
    geom_rug(
        aes(height_above_takeoff.meters., group = species, colour = species),
        inherit.aes = FALSE,
        transform(
            filter(data_fia, behaviour == 1),
            expl.name = height_above_takeoff.meters.),
        sides = "t") +
    labs(
        x = "Drone Altitude Above Birds (m)",
        y = "Probability of Inducing Bird Flight",
        fill = "Eastern Curlew Precense",
        colour = "Eastern Curlew Precense") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.text.x = element_text(size = 10, face = "bold"))

# species sensitivity vs eastern curlew presence
ggplot() +
    theme_set(theme_bw()) +
    geom_rug(
        data =
        filter(
            data_fia,
            behaviour == 0 &
            species != "eastern curlew" &
            height_above_takeoff.meters. > 40),
        aes(eastern.curlew.presence,
            as.numeric(behaviour),
            colour = eastern.curlew.presence),
        sides = "b",
        position = "jitter",
        size = 1) +
    geom_rug(
        data =
        filter(
            data_fia,
            behaviour == 1 &
            species != "eastern curlew" &
            height_above_takeoff.meters. > 40),
        aes(
            eastern.curlew.presence,
            as.numeric(behaviour),
            colour = eastern.curlew.presence),
        sides = "t",
        position = "jitter",
        size = 1) +
    geom_boxplot(
        data =
        filter(
            results_fia,
            species != "eastern curlew" &
            height_above_takeoff.meters. > 40),
        aes(
            eastern.curlew.presence,
            as.numeric(prediction),
            fill = eastern.curlew.presence),
        alpha = 0.4) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(
        x = "Eastern Curlew Presence",
        y = "Probability of Inducing Bird Flight") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none")

#### fid analysis ####

# degrade data to one entry column and filter for just mavic 2 pro disturbance
data_fid <- data_aug %>%

    # only retain disturbance manoeuvre flights
    filter(approach.type == "advancing") %>%

    # remove species without enough data
    filter(species == "eastern curlew") %>%

    # remove all except alarm and flight
    filter(behaviour == 1) %>%

    #remove all drones except mavic 2 pro
    filter(drone == "mavic 2 pro")

# fit GAM

# Main Effects
# Question: What is the flight initiation distance for eastern curlew?

# drone:
# does the fid vary with drone type?

# altitude:
# does the fid vary with drone altitude?

gam_fid <- gam(
    dronebirddistance ~
    s(height_above_takeoff.meters.),
    data = data_fid,
    family = "gaussian",
    method = "REML")

summary(gam_fid)

# Creating new data

altitude_fid <- seq(30, 110, by = 1)

new_data_fid <- expand.grid(height_above_takeoff.meters. = altitude_fid)

pred_fid <- predict.gam(
    gam_fid,
    new_data_fid,
    trans = binomial()$linkinv,
    type = "response",
    se.fit = TRUE)

fid_results <- new_data_fid %>%
    mutate(prediction = pred_fid$fit) %>%
    mutate(upper = pred_fid$fit + (2 * pred_fid$se.fit)) %>%
    mutate(lower = pred_fid$fit - (2 * pred_fid$se.fit))

# Visualising Results

# effect of altitude for mavic 2 data
ggplot() +
    theme_set(theme_bw()) +
    geom_ribbon(
        data = fid_results,
        aes(
            height_above_takeoff.meters.,
            ymin = lower,
            ymax = upper,
            colour = "orange",
            fill = "orange"),
        alpha = 0.1) +
    geom_line(
        data = fid_results,
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

#### takeoff analysis ####
data_fit <- data_aug %>%

    # keep only flights where the drone was advancing
    filter(approach.type == "ascending") %>%

    # drop the landed behaviour state
    filter(behaviour != 2) %>%

    # keep only the maximum behaviour state
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(flightcode, species) %>%
    filter(behaviour == max(behaviour)) %>%

    # filter out species without enough data
    # group_by(species) %>%
    # filter(n() > 25) %>%
    filter(species == "eastern curlew") %>%

    # filter out drones without much data
    filter(
        drone != "mavic mini",
        drone != "phantom 4 pro",
        drone != "phantom pro 4")

    # to fix
    # drop_na(drone) %>%
    # filter(drone != "mavic pro 2") %>%
    # filter(flightcode != "311") %>%
    # filter(flightcode != "411")


# fit GAM

# Main Effects
# Question: at what altitude does flight not occur?

# drone:
# does the behaviour vary with drone type?

# altitude:
# does the behaviour vary with drone altitude?

# species:
# does the behaviour vary with species

gam_fit <- gam(
    behaviour
    ~ s(dronebirddistance)
    + drone,
    data = data_fit,
    family = "binomial",
    method = "REML")

summary(gam_fit)

# Creating new data

dronebirddistance_fit = seq(75, 750, by = 1)
drone_fit <- unique(data_fit$drone)

new_data_fit <- expand.grid(
    dronebirddistance = dronebirddistance_fit,
    drone = drone_fit)

pred_fit <- predict.gam(
    gam_fit,
    new_data_fit,
    trans = binomial()$linkinv,
    type = "response",
    se.fit = TRUE)

fit_results <- new_data_fit %>%
    mutate(prediction = pred_fit$fit) %>%
    mutate(upper = pred_fit$fit + (2 * pred_fit$se.fit)) %>%
    mutate(lower = pred_fit$fit - (2 * pred_fit$se.fit))

# Visualising Results
ggplot() +
    theme_set(theme_bw()) +
    geom_line(
        data = fit_results,
        aes(dronebirddistance, prediction, group = drone, colour = drone),
        size = 1.2) +
    geom_ribbon(
        data = fit_results,
        aes(
            dronebirddistance,
            ymin = lower,
            ymax = upper,
            group = drone,
            fill = drone,
            colour = drone),
        alpha = 0.1) +
    geom_rug(
        aes(dronebirddistance,group = drone, colour = drone),
        inherit.aes = FALSE,
        transform(
            filter(data_fit, behaviour == 0),
            expl.name = dronebirddistance),
        sides = "b",
        size = 1) +
    geom_rug(
        aes(dronebirddistance, group = drone, colour = drone),
        inherit.aes = FALSE,
        transform(
            filter(data_fit, behaviour == 1),
            expl.name = dronebirddistance),
        sides = "t",
        size = 1) +
    labs(
        x = "Drone Takeoff Distance (m)",
        y = "Probability of Inducing Bird Flight",
        fill = "Drone",
        colour = "Drone") +
    coord_cartesian(xlim = c(75, 750)) +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(fill = alpha("blue", 0))) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = round(seq(0, 800, by = 100), 1)) +
    scale_y_continuous(expand = c(0, 0))


#### Ethics Analysis ####

# number of approaches
approaches <- data_aug %>%
    # flights not in 2021
    filter(datetime_aest > as.Date("2021-01-01")) %>%
    filter(datetime_aest < as.Date("2022-01-01")) %>%
    # remove flights where birds took off in control
    filter(approach.type != "control") %>%
    # group by flight
    group_by(flightcode) %>%
    # take first entry
    slice(1)

# number of disturbances
disturbance_approaches <- data_aug %>%
    # flights not in 2021
    filter(datetime_aest > as.Date("2021-01-01")) %>%
    filter(datetime_aest < as.Date("2022-01-01")) %>%
    # only keep flights where drone was approaching
    filter(
        approach.type == "ascending" |
        approach.type == "advancing") %>%
    # filter for birds fligt
    filter(behaviour == 2) %>%
    # remove disturbances from alternate sources
    mutate(notes = case_when(
        notes == "alternate disturbance" ~ 1,
        TRUE ~ 2)) %>%
    filter(notes == 2) %>%
    # group by flight
    group_by(flightcode) %>%
    # take first entry
    slice(1)

# total birds disturbed
birds_disturbed <- data_aug  %>%
    # filter for birds flight
    filter(behaviour == 2) %>%
    # group each species for each flight
    group_by(flightcode, species) %>%
    # take first entry
    slice(1) %>%
    # only keep flights where drone was approaching
    filter(
        approach.type == "ascending" |
        approach.type == "advancing") %>%
    # remove disturbances from alternate sources
    mutate(notes = case_when(
        notes == "alternate disturbance" ~ 1,
        TRUE ~ 2)) %>%
    filter(notes == 2) %>%
    # flights not in 2021
    filter(datetime_aest > as.Date("2021-01-01")) %>%
    filter(datetime_aest < as.Date("2022-01-01")) %>%
    # summarise
    group_by(species) %>%
    summarise(sum = sum(count))
(sum(birds_disturbed$sum) - 4000)

# species
species <- data_aug %>%
    # filter out species without enough data
    group_by(species) %>%
    filter(n() > 5) %>%
    # take first entry to get unique species
    slice(1)

#### PLAYING AROUND ####
summary(data)
data_play <- data_fia %>%

# Remove all drones except inspire 2
filter(drone == "inspire 2") %>%
filter(behaviour == 0)