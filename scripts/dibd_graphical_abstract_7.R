################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Model Prediction
# Author: Josh Wilson
# Date: 13-06-2022
# References:
# https://cran.r-project.org/web/packages/mgcv/mgcv.pdf
# https://adibender.github.io/pammtools/

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Specify required packages
packages <- c("tidyr", "ggplot2", "readr", "dplyr")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages)) {
    package_consent <- readline(
        prompt <- (paste("Install", new_packages, " y/n?\n")))
    if (tolower(package_consent) == "y") {
        install.packages(new_packages)
        }
    else print(paste("This code cannot be run without", new_packages))
}

# load packages, install if not available provided user approves
lapply(packages, require, character.only = TRUE)

# import data
data <- read_csv("data/dibd_prep.csv")

data_fit <- data %>%
    # remove data points where another species already took flight
    filter(response == 0 | sum_response == 1) %>%
    # specify factors
    mutate(
        stimulus = as.factor(stimulus),
        flight = as.factor(flight),
        species = as.factor(species),
        location = as.factor(location),
        behind_trees = as.factor(behind_trees))

###########################################################################
#### Survival Probability and Flight Initiation Distance Visualisation ####
###########################################################################

# load model
fit <- readRDS("models/model.rds")
summary(fit)

# determine the mean or mode for all numerical or categorical variables
modus <- function(var) {
  # calculate modus
  freqs <- table(var)
  mod   <- names(freqs)[which.max(freqs)]
  # factors should be returned as factors with all factor levels
  if (is.factor(var)) {
    mod <- factor(mod, levels = levels(var))
  }
  return(mod)
}

num <- summarize_if(data_fit, .predicate = is.numeric, ~mean(., na.rm = TRUE))
fac <- summarise_all(select_if(data_fit, ~!is.numeric(.x)), modus)
ref <- bind_cols(num, fac)

# load test flight launched from 500m, approach at 120m
test_flight <- read_csv("data/dibd_test_flight.csv") %>%
    filter(time_since_launch_s %% 1 == 0)

# get inverse link
ilink <- family(fit)$linkinv

# get a list of the species that were included in the model
fit_species <- data_fit %>%
    # filter out species that never took flight
    group_by(species) %>%
    filter(max(response) == 1) %>%
    filter(!is.na(species)) %>%
    {unique(.$species)}

# This function adds the required explanitory variables to the test flight
log_simulator <- function(fit, altitude_list, species_list) {
    df_i <- data.frame()

    for (species_index in seq_len(length(species_list))) {
        target <- species_list[species_index]
        print(target)
        # loop through test altitudes, crop the test flight accordingly
        # predict the survival probability and save the output
        for (altitude_index in seq_len(length(altitude_list))) {
            test_altitude <- altitude_list[altitude_index]
            # cropping test flight to specified altitude
            flight_ascent <- test_flight %>%
                # there is 4 meters of deceleration
                filter(distance_z_m < test_altitude - 4)
            flight_approach <- test_flight %>%
                # the drone was ascending for the first 47 seconds
                slice(round(47):n()) %>%
                # move the approach altitude from 120 to the test altitude
                mutate(distance_z_m = distance_z_m - (120 - test_altitude))
            # adding on explanitory variables
            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    distance_x_m,
                    distance_z_m,
                    velocity_x_m.s,
                    velocity_y_m.s,
                    velocity_z_m.s,
                    acceleration_xyz_m.s.s) %>%
                mutate(
                    time_since_launch_s = row_number(),
                    species = target,
                    flight = ref$flight,
                    cloud_cover_percent = ref$cloud_cover_percent,
                    time_to_high_tide_hr = ref$time_to_high_tide_hr,
                    wind_speed_m.s = ref$wind_speed_m.s,
                    temperature_degC = ref$temperature_degC,
                    location = ref$location,
                    stimulus = ref$stimulus,
                    behind_trees = ref$behind_trees,
                    count = ref$count,
                    test_altitude = test_altitude,
                    repeat_approaches = 0,
                    body_mass = unlist(slice(
                        filter(data_fit, species == target)["body_mass"], 1)))
            # predicting survival probability
            if (target %in% fit_species) {
                pred <- predict(
                    fit,
                    flight_log_new,
                    se.fit = TRUE,
                    type = "link",
                    exclude = "s(flight)")
                # transform prediction to response scale
                # and calculate survival probability
                prediction <- cbind(flight_log_new, pred) %>%
                    mutate(
                        upr = ilink(fit + (2 * se.fit)),
                        lwr = ilink(fit - (2 * se.fit)),
                        fit = ilink(fit),
                        flight_prob = 1 - exp(-cumsum(fit)),
                        flight_lower = 1 - exp(-cumsum(upr)),
                        flight_upper = 1 - exp(-cumsum(lwr)))
            }
            else {
                prediction <- flight_log_new %>%
                    mutate(
                        flight_prob = 1,
                        flight_lower = 1,
                        flight_upper = 1)
            }
            # saving dataframe
            df_i <- bind_rows(df_i, prediction)
        }
    }
    return(df_i)
}

# predict for altitudes between 0 and 120m
altitudes <- seq(from = 0, to = 120, by = 10)

# predict just for species with enough data and relevant
target_birds <- c(
            "Eastern Curlew",
            # "Australian Pelican",
            # "Marsh Sandpiper",
            # "Grey-tailed Tattler",
            "Whimbrel",
            # "Gull-billed Tern",
            # "Royal Spoonbill",
            # "Great Knot",
            "Pied Stilt",
            # "Masked Lapwing",
            # "Caspian Tern",
            "Pied Oystercatcher"
            # "Bar-tailed Godwit",
            # "White-faced Heron",
            # "Curlew Sandpiper",
            # "Australian White Ibis",
            # "Black Swan",
            # "Cattle Egret",
            # "Great Egret",
            # "Intermediate Egret",
            # "Little Egret",
            # "Silver Gull",
            # "Terek Sandpiper"
            )

# generate predictions
flight_data <- log_simulator(fit, altitudes, target_birds) %>%
    mutate(species = factor(species, levels = target_birds, ordered = TRUE))

# creating contour plot of flight probability for each species
# only use advancing data to avoid conflicting points
advancing <- flight_data %>%
    mutate(distance_z_m = round(distance_z_m)) %>%
    filter(distance_z_m == test_altitude)

# create plot
graphical_abstract <- ggplot() +
    # add contour lines
    geom_contour(
        data = advancing,
        aes(
            x = distance_x_m,
            y = distance_z_m,
            z = flight_prob,
            linetype = species),
        breaks = 0.2,
        size = 0.5,
        colour = "black") +
    # plot aesthetics
    theme_classic() +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0.5, "cm"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
    labs(linetype = "20% Probability of\nBirds Taking Flight") +
    ylab("Altitude [m]") +
    xlab("Ground Distance [m]") +
    scale_x_continuous(limits = c(0, 300), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_linetype_manual(values = c("solid", "dotdash", "dotted", "dashed")) +
    coord_fixed()

# save plot
ggsave(
    "plots/graphical_abstract.png",
    graphical_abstract,
    height = 2.5,
    width = 5)
