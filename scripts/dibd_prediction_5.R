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
                    # stimulus = "inspire 2",
                    behind_trees = ref$behind_trees,
                    count = ref$count,
                    test_altitude = test_altitude)
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
            "Grey-tailed Tattler",
            "Whimbrel",
            "Gull-billed Tern",
            "Royal Spoonbill",
            "Great Knot",
            "Pied Stilt",
            "Masked Lapwing",
            "Caspian Tern",
            "Pied Oystercatcher",
            "Bar-tailed Godwit",
            "Terek Sandpiper")

# generate predictions
flight_data <- log_simulator(fit, altitudes, target_birds) %>%
    mutate(species = factor(species, levels = target_birds, ordered = TRUE))

# example prediction
example_prediction <- flight_data %>%
    # select eastern curlew
    filter(
        test_altitude == 120,
        species == "Eastern Curlew") %>%
    # normalise drone position
    mutate(
        distance_x_m = distance_x_m / max(distance_x_m),
        distance_z_m = distance_z_m / max(distance_z_m)) %>%
    # pivot for plotting
    pivot_longer(
        c(distance_x_m, distance_z_m, flight_prob),
        names_to = "legend",
        values_to = "line") %>%
    mutate(legend = case_when(
        legend == "flight_prob" ~ "probability of birds taking flight",
        legend == "distance_x_m" ~ "normalised distance x [0:500m]",
        legend == "distance_z_m" ~ "normalised distance z [0:120m]"))

# plot the prediction
example_prediction_plot <- ggplot() +
    geom_line(
        data = example_prediction,
        aes(y = line, x = time_since_launch_s, linetype = legend),
        size = 1) +
    geom_ribbon(
        data = filter(
            example_prediction,
            legend == "probability of birds taking flight"),
        aes(x = time_since_launch_s, ymin = flight_lower, ymax = flight_upper),
        alpha = 0.3) +
    scale_linetype_manual(values = c("dotted", "twodash", "solid")) +
    xlab("time since launch s") +
    ylab("normalised values") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15)) +
    guides(linetype = guide_legend(nrow = 3))

ggsave(
    "plots/example_prediction.png",
    example_prediction_plot,
    height = 5,
    width = 5)

# creating contour plot of flight probability for each species
# only use advancing data to avoid conflicting points
advancing <- flight_data %>%
    mutate(distance_z_m = round(distance_z_m)) %>%
    filter(distance_z_m == test_altitude)

# add data used for plots
data_raw <- data_fit %>%
    # inspire 2 significantly more disturbance than smaller drones
    filter(stimulus != "inspire 2") %>%
    #only getraw data for included species
    filter(species %in% target_birds) %>%
    # only keep points at 20s intervals to avoid crowding plots
    filter(time_since_launch_s %% 30 == 0 | response == 1) %>%
    # change binary response to descriptive
    mutate(response = case_when(
        response == 1 ~ "flight",
        response == 0 ~ "no flight")) %>%
    arrange(desc(response))

fid_plot <- ggplot() +
    # add filled contours
    geom_contour_filled(
        data = advancing,
        aes(x = distance_x_m, y = distance_z_m, z = flight_prob),
        binwidth = 0.25) +
    # add black contour lines
    geom_contour(
        data = advancing,
        aes(x = distance_x_m, y = distance_z_m, z = flight_prob),
        binwidth = 0.25,
        size = 0.15,
        colour = "black") +
    # add dashed line for upper confidence interval at 0.5
    geom_contour(
        data = advancing,
        aes(
            linetype = "50% flight prob upper ci",
            x = distance_x_m,
            y = distance_z_m,
            z = flight_lower),
        breaks = 0.5,
        colour = "black",
        size = 0.35) +
    # add raw data points
    geom_point(
        data = data_raw,
        aes(x = distance_x_m, y = distance_z_m,
        colour = factor(response)),
        size = 0.35) +
    # define colours for flight probability contours
    scale_fill_grey(start = 1.0, end = 0.5) +
    # define colours for points
    scale_colour_grey() +
    # define linetype for 50% Flight Prob Upper CI contour
    scale_linetype_manual(values = c("dashed")) +
    # plot aesthetics
    theme(panel.background = element_rect(fill = "white")) +
    xlab("distance x m") +
    ylab("distance z m") +
    labs(
        colour = "raw data",
        fill = "flight probability",
        linetype = "95% confidence interval") +
    scale_x_continuous(limits = c(0, 500), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
    coord_fixed() +
    # facet wrap by common name
    facet_wrap("species", ncol = 3)

# save plot
ggsave("plots/fid.png", fid_plot, height = 5, width = 10)

# plot proportion of eastern curlew flights
# that induced flight in other species within 10s

# create data
sentinel_proportion <- data %>%
    # keep flights where eastern curlew were present
    group_by(flight) %>%
    filter(any(species == "Eastern Curlew"))  %>%
    # keep flight were eastern curlew took off
    filter(max(`response_Eastern Curlew`) == 1) %>%
    # keep only the max response for each flight and species
    group_by(flight, species) %>%
    filter(species != "Eastern Curlew") %>%
    filter(response == max(response)) %>%
    slice(1) %>%
    # count number of flights vs no flights
    group_by(species) %>%
    count(species, response, sort = TRUE) %>%
    # rename response
    mutate(response = case_when(
        response == 1 ~ "Flight",
        TRUE ~ "No Flight")) %>%
    # change to wider format
    pivot_wider(names_from = response, values_from = n) %>%
    # fix na's
    mutate(
        Flight = case_when(is.na(Flight) ~ 0, T ~ as.double(Flight)),
        `No Flight` = case_when(
            is.na(`No Flight`) ~ 0,
            T ~ as.double(`No Flight`))) %>%
    # add n
    mutate(
        `sentinel induced flight proportion` =
        (100 * Flight) / (Flight + `No Flight`),
        n = paste(Flight + `No Flight`))

# create plot
sentinel_proportion_plot <- ggplot(
    data = sentinel_proportion,
    aes(
        x = reorder(species, -`sentinel induced flight proportion`),
        y = `sentinel induced flight proportion`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), vjust = 1.25, size = 2.5, color = "black") +
    theme(
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab("species")

ggsave(
    "plots/sentinel_proportion.png",
    sentinel_proportion_plot,
    height = 4,
    width = 4)

# plot points where species took flight due to
# sentinel over the top of expected response
sentinel_points <- data %>%
    # keep flights where eastern curlew were present
    group_by(flight) %>%
    filter(any(species == "Eastern Curlew"))  %>%
    # keep flight were eastern curlew took off
    filter(max(`response_Eastern Curlew`) == 1) %>%
    # keep only the times they took off for each
    # flight and species
    group_by(flight, species) %>%
    filter(response == 1) %>%
    mutate(response = "flight") %>%
    # keep only target species
    filter(species %in% target_birds) %>%
    # drop Eastern Curlew
    mutate(species = factor(species, levels = target_birds, ordered = TRUE)) %>%
    filter(species != "Eastern Curlew") %>%
    slice(1)

sentinel_pts_plot <- ggplot() +
    # add filled contours
    geom_contour_filled(
        data = filter(advancing, species != "Eastern Curlew"),
        aes(x = distance_x_m, y = distance_z_m, z = flight_prob),
        binwidth = 0.25) +
    # add black contour lines
    geom_contour(
        data = filter(advancing, species != "Eastern Curlew"),
        aes(x = distance_x_m, y = distance_z_m, z = flight_prob),
        binwidth = 0.25,
        size = 0.15,
        colour = "black") +
    # add dashed line for upper confidence interval at 0.5
    geom_contour(
        data = filter(advancing, species != "Eastern Curlew"),
        aes(
            linetype = "50% flight prob upper ci",
            x = distance_x_m,
            y = distance_z_m,
            z = flight_lower),
        breaks = 0.5,
        colour = "black",
        size = 0.5) +
    # add raw data points
    geom_point(
        data = sentinel_points,
        aes(colour = factor(response), x = distance_x_m, y = distance_z_m),
        size = 1) +
    # define colours for flight probability contours
    scale_fill_grey(start = 1.0, end = 0.5) +
    # define colours for points
    scale_colour_grey() +
    # define linetype for 50% Flight Prob Upper CI contour
    scale_linetype_manual(values = c("dashed")) +
    # plot aesthetics
    theme(panel.background = element_rect(fill = "white")) +
    labs(
        colour = "raw data",
        fill = "flight probability",
        linetype = "95% confidence interval") +
    scale_x_continuous(limits = c(0, 500), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
    coord_fixed() +
    xlab("distance x m") +
    ylab("distance z m") +
    # facet wrap by common name
    facet_wrap("species", ncol = 3)

ggsave("plots/sentinel_points.png", sentinel_pts_plot, height = 5, width = 10)
