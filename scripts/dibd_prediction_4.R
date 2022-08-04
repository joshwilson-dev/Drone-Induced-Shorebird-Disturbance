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
packages <- c("tidyr", "ggplot2", "readr", "dplyr", "pammtools")
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
data_ped <- read_csv("data/dibd_ped_data.csv") %>%
    # specify factors
    mutate(
        flight = as.factor(flight),
        flock = as.factor(flock))

###########################################################################
#### Survival Probability and Flight Initiation Distance Visualisation ####
###########################################################################

# load model
fit <- readRDS("models/dibd-model-26-07-22_09-36.rds")
summary(fit)

# determine the mean, or mode for all numerical or categorical variables
ref <- data_ped %>%
    ungroup() %>%
    sample_info()

# load test flight launched from 500m, approach at 120m
test_flight <- read_csv("data/dibd_test_flight.csv")

# This function adds the required explanitory variables to the test flight
log_simulator <- function(fit, altitude_list, species_list) {
    df_i <- data.frame()
    for (x in 1:length(species_list)) {
        target <- species_list[x]
        # determine species specific referencedata
        ref_species <- data_ped %>%
            ungroup() %>%
            filter(species == as.character(target)) %>%
            sample_info()
        # loop through test altitudes, crop the test flight accordingly
        # predict the survival probability and save the output
        for (y in 1:length(altitude_list)) {
            altitude <- altitude_list[y]
            # cropping test flight to specified altitude
            flight_ascent <- test_flight %>%
                filter(distance_z < altitude - 4)

            flight_approach <- test_flight %>%
                slice(round(47):n()) %>%
                mutate(distance_z = distance_z - (120 - altitude))
            # adding on explanitory variables
            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    distance_x,
                    distance_z,
                    velocity_x,
                    velocity_y,
                    velocity_z,
                    acceleration) %>%
                mutate(
                    tend = row_number(),
                    species = target,
                    flight = ref_species$flight,
                    flock = ref_species$flock,
                    cloud_cover = ref$cloud_cover,
                    high_tide = 0,
                    wind_speed = ref$wind_speed,
                    temperature = ref$temperature,
                    location = ref$location,
                    specification = "mavic 2 pro",
                    obscuring = "not obscured",
                    count = 100,
                    # sentinel = "a",
                    altitude = altitude)
            # predicting survival probability
            prediction <- flight_log_new %>%
                mutate(intlen = 1) %>%
                add_surv_prob(fit, exclude = c("s(flight)", "s(location)"))
            # saving dataframe
            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

# test altitudes between 0 and 120m for all species in model
altitudes <- seq_range(0:120, by = 10)
target_birds <- unique(data_ped$species)

survival_dat <- log_simulator(fit, altitudes, target_birds)
survival_data <- survival_dat %>%
    mutate(
        surv_prob = 1 - surv_prob,
        surv_lower = 1 - surv_lower,
        surv_upper = 1 - surv_upper) %>%
    mutate(
        surv_prob = case_when(surv_prob == 1 ~ 0.9999999, TRUE ~ surv_prob),
        surv_lower = case_when(surv_lower == 1 ~ 0.9999999, TRUE ~ surv_lower),
        surv_upper = case_when(surv_upper == 1 ~ 0.9999999, TRUE ~ surv_upper))

# creating and plotting example of output predicted probability
flight_log <- survival_data %>%
    filter(
        altitude == 120,
        species == "eastern curlew") %>%
    mutate(
        distance_x = distance_x / max(distance_x),
        distance_z = distance_z / max(distance_z)) %>%
    pivot_longer(
        c(distance_x, distance_z, surv_prob),
        names_to = "legend",
        values_to = "line") %>%
    mutate(legend = case_when(
        legend == "surv_prob" ~ "Probability of Birds Taking Flight",
        legend == "distance_x" ~ "Normalised Distance [0:500m]",
        legend == "distance_z" ~ "Normalised Altitude [0:120m]"))

surv_plot <- ggplot() +
    geom_line(
        data = flight_log,
        aes(y = line, x = tend, linetype = legend),
        size = 1) +
    geom_ribbon(
        data = filter(
            flight_log,
            legend == "Probability of Birds Taking Flight"),
        aes(x = tend, ymin = surv_lower, ymax = surv_upper),
        alpha = 0.3) +
    scale_linetype_manual(values = c("dotted", "longdash", "solid")) +
    xlab("Time Since Launch [s]") +
    ylab("Normalised Values") +
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
    "plots/eastern_curlew_flight_probability.png",
    surv_plot,
    height = 10,
    width = 10)

# creating contour plot of flight probability for each species
advancing <- survival_data %>%
    mutate(distance_z = round(distance_z)) %>%
    filter(species != "eastern curlew") %>%
    filter(distance_z == altitude) %>%
    mutate(species = case_when(
        species == "eastern curlew" ~ "Eastern Curlew",
        species == "bar tailed godwit" ~ "Bar-tailed Godwit",
        species == "whimbrel" ~ "Whimbrel",
        species == "gull billed tern" ~ "Gull-billed Tern",
        species == "great knot" ~ "Great Knot",
        species == "caspian tern" ~ "Caspian Tern",
        species == "pied stilt" ~ "Pied Stilt",
        species == "pied oystercatcher" ~ "Pied Oystercatcher",
        species == "black swan" ~ "Black Swan")) %>%
    mutate(
        species = factor(
        species,
        ordered = TRUE,
        levels = c(
            "Eastern Curlew",
            "Bar-tailed Godwit",
            "Whimbrel",
            "Gull-billed Tern",
            "Great Knot",
            "Caspian Tern",
            "Pied Stilt",
            "Pied Oystercatcher",
            "Black Swan")))

# Creating line at 50% flight probability with corresponding CI
ribbon <- advancing  %>%
    pivot_longer(
        contains("surv"),
        names_to = "Confidence Intervals",
        values_to = "fit") %>%
    mutate(`Confidence Intervals` = case_when(
        `Confidence Intervals` == "surv_prob" ~ "50% Flight Probability",
        `Confidence Intervals` == "surv_upper" ~ "95% Confidence Interval",
        `Confidence Intervals` == "surv_lower" ~ "95% Confidence Interval"))

data <- read_csv("data/dibd_data.csv")

raw_data <- data %>%
    filter(grepl("with curlew", species)) %>%
    filter(response == 1) %>%
    filter(
        specification == "mavic 2 pro" |
        specification == "phantom 4 pro") %>%
    # filter(specification == "inspire 2 pro") %>%
    mutate(response = case_when(
        response == 1 ~ "Flight",
        response == 0 ~ "No Flight")) %>%
    arrange(desc(response)) %>%
    filter(
        time_since_launch %% 10 == 0 |
        response == "Flight") %>%
    mutate(species = case_when(
        species == "eastern curlew" ~ "Eastern Curlew",
        species == "bar tailed godwit" ~ "Bar-tailed Godwit",
        species == "whimbrel" ~ "Whimbrel",
        species == "gull billed tern" ~ "Gull-billed Tern",
        species == "great knot" ~ "Great Knot",
        species == "caspian tern" ~ "Caspian Tern",
        species == "pied stilt" ~ "Pied Stilt",
        species == "pied oystercatcher" ~ "Pied Oystercatcher",
        species == "black swan" ~ "Black Swan",
        TRUE ~ species))

raw_data$species <- gsub(" with curlew", "", raw_data$species)

raw_data <- data_ped %>%
    filter(
        specification == "mavic 2 pro" |
        specification == "phantom 4 pro") %>%
    mutate(ped_status = case_when(
        ped_status == 1 ~ "Flight",
        ped_status == 0 ~ "No Flight")) %>%
    arrange(desc(ped_status)) %>%
    filter(
        time_since_launch %% 10 == 0 |
        ped_status == "Flight") %>%
    mutate(species = case_when(
        species == "eastern curlew" ~ "Eastern Curlew",
        species == "bar tailed godwit" ~ "Bar-tailed Godwit",
        species == "whimbrel" ~ "Whimbrel",
        species == "gull billed tern" ~ "Gull-billed Tern",
        species == "great knot" ~ "Great Knot",
        species == "caspian tern" ~ "Caspian Tern",
        species == "pied stilt" ~ "Pied Stilt",
        species == "pied oystercatcher" ~ "Pied Oystercatcher",
        species == "black swan" ~ "Black Swan"))

raw_data <- raw_data %>%
    filter(species != "eastern curlew") %>%
    mutate(species = case_when(
        species == "eastern curlew" ~ "Eastern Curlew",
        species == "bar tailed godwit" ~ "Bar-tailed Godwit",
        species == "whimbrel" ~ "Whimbrel",
        species == "gull billed tern" ~ "Gull-billed Tern",
        species == "great knot" ~ "Great Knot",
        species == "caspian tern" ~ "Caspian Tern",
        species == "pied stilt" ~ "Pied Stilt",
        species == "pied oystercatcher" ~ "Pied Oystercatcher",
        species == "black swan" ~ "Black Swan",
        TRUE ~ species)) %>%
    mutate(
        species = factor(
            species,
            ordered = TRUE,
            levels = c(
                "Eastern Curlew",
                "Bar-tailed Godwit",
                "Whimbrel",
                "Gull-billed Tern",
                "Great Knot",
                "Caspian Tern",
                "Pied Stilt",
                "Pied Oystercatcher",
                "Black Swan")))

fid_plot <- ggplot() +
    # create base contour plots
    geom_contour_filled(
        data = advancing,
        aes(x = distance_x, y = distance_z, z = surv_prob),
        binwidth = 0.1) +
    # define colours for flight probability contours
    scale_fill_brewer(
        type = "div",
        palette = 5,
        direction = -1,
        aesthetics = "fill") +
    # add line and ribbons at 50% flight probability
    # geom_contour(
    #     data = ribbon,
    #     aes(
    #         x = distance_x,
    #         y = distance_z,
    #         z = fit,
    #         linetype = `Confidence Intervals`),
    #     colour = "black",
    #     binwidth = 0.5,
    #     size = 4) +
    # define colours for 50% flight prob
    scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
    # add raw flight or no flight endpoints for sub-2kg drones
    geom_point(
        data = raw_data,
        aes(x = distance_x, y = distance_z,
        colour = factor(response)),
        size = 10) +
    # define colours for raw data
    scale_color_manual(values = c("red", "blue")) +
    # facet wrap by common name
    facet_wrap("species", nrow = 4) +
    # make plot aesthetic
    theme_bw() +
    scale_x_continuous(limits = c(0, 450), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
    xlab("Horizontal Distance [m]") +
    ylab("Altitude [m]") +
    labs(fill = "Probability of Birds Having Taken Flight\n In Single Species Flock, Figure 3") +
    labs(colour = "Eastern Curlew Induced Flight Points") +
    # ggtitle("350mm Quadcopter Induced Bird Flight Probability") +
    # ggtitle("350mm Quadcopter Induced Waterbird Flight Probability\n When in a Mixed Species Flock Containing Eastern Curlew") +
    ggtitle("Inspire 2 Induced Bird Flight Probability") +
    coord_fixed(ratio = 1) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 80),
        panel.spacing = unit(5, "lines"),
        strip.text = element_text(size = 50, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "in"),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.15, "in"),
        axis.text = element_text(size = 60, colour = "black"),
        axis.title = element_text(size = 80, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(1, "in"),
        legend.title.align = 0.5,
        legend.text.align = 0,
        legend.box = "horizontal",
        legend.margin = margin(1, 2, 0, 2, unit = "in"),
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 50, face = "bold"),
        legend.key.height = unit(5, "cm"),
        # transparent background
        panel.background = element_rect(fill = "transparent", colour = NA_character_),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA_character_),
        legend.background = element_rect(fill = "transparent", colour = NA_character_),
        legend.box.background = element_rect(fill = "transparent", colour = NA_character_),
        legend.key = element_rect(fill = "transparent")) +
        guides(
            colour = guide_legend(
                nrow = 2,
                title.position = "top",
                title.hjust = 0.5),
            fill = guide_legend(
                nrow = 2,
                title.position = "top",
                title.hjust = 0.5),
            linetype = guide_legend(
                nrow = 3,
                title.position = "top",
                title.hjust = 0.5))
# save plot
ggsave("plots/eci_flight_initiation_distance.png", fid_plot, height = 40, width = 60, limitsize = FALSE)


check <- data %>%
    filter(species == "eastern curlew") %>%
    group_by(flight) %>%
    slice(1)

# % of flights where eastern curlew took flight and other species didn't
check <- data %>%
    filter(
        species != "eastern curlew" &
        species != "whimbrel" &
        species != "bar tailed godwit" &
        species != "gull billed tern" &
        species != "great knot" &
        species != "caspian tern" &
        species != "pied stilt" &
        species != "pied oystercatcher" &
        species != "black swan") %>%
    group_by(flight, species) %>%
    filter(response == max(response)) %>%
    slice(1) %>%
    group_by(species) %>%
    select(flight, species, test, approach, response, time_since_launch) %>%
    count(species, response, sort = TRUE) %>%
    mutate(response = case_when(
        response == 1 ~ "Flight",
        TRUE ~ "No Flight")) %>%
    pivot_wider(names_from = response, values_from = n) %>%
    mutate(Flight_Percentage = (100 * Flight) / (Flight + `No Flight`))
