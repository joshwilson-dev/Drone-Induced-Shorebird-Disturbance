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
data_ped <- read_csv("data/dibd_ped_data.csv")

###########################################################################
#### Survival Probability and Flight Initiation Distance Visualisation ####
###########################################################################

# load model
fit <- readRDS("models/dibd-model-25-06-22_06-13.rds")

summary(fit)
# determine the mean, or mode for all numerical or categorical variables
ref <- data_ped %>%
    ungroup() %>%
    sample_info()

# load test flight launched from 300m, approach at 120m
test_flight <- read_csv("data/dibd_test_flight.csv") %>%
    filter(time_since_launch %% 1 == 0)

# This function adds the required explanitory variables to the test flight
log_simulator <- function(fit, altitude_list, species_list) {
    df_i <- data.frame()
    for (x in 1:length(species_list)) {
        species <- species_list[x]
        # determine species specific referencedata
        ref_species <- data_ped %>%
            ungroup() %>%
            filter(common_name == as.character(species)) %>%
            sample_info()
        # loop through test altitudes, crop the test flight accordingly
        # predict the survival probability and save the output
        for (y in 1:length(altitude_list)) {
            altitude <- altitude_list[y]
            # cropping test flight to specified altitude
            flight_ascent <- test_flight %>%
                filter(z_disp_m < altitude - 4)

            flight_approach <- test_flight %>%
                slice(round(47):n()) %>%
                mutate(z_disp_m = z_disp_m - (120 - altitude))
            # adding on explanitory variables
            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    xy_disp_m,
                    z_disp_m,
                    xb_vel_ms,
                    z_vel_ms,
                    xyz_acc_mss) %>%
                mutate(
                    tend = row_number(),
                    common_name = species,
                    flight = ref$flight,
                    cloud_cover_p = ref$cloud_cover_p,
                    hrs_from_high = ref$hrs_from_high,
                    wind_speed_ms = ref$wind_speed_ms,
                    temperature_dc = ref$temperature_dc,
                    location = ref$location,
                    drone = "mavic 2 pro",
                    drone_obscured = FALSE,
                    count = ref$count,
                    normalised_count = 0.5,
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
target_birds <- unique(data_ped$common_name)
survival_data <- log_simulator(fit, altitudes, target_birds)

# creating and plotting example of output predicted probability
flight_log <- survival_data %>%
    filter(
        common_name == "eastern curlew",
        altitude == 120) %>%
    mutate(
        surv_prob = 1 - surv_prob,
        surv_lower = 1 - surv_lower,
        surv_upper = 1 - surv_upper) %>%
    mutate(
        xy_disp_m = xy_disp_m / max(xy_disp_m),
        z_disp_m = z_disp_m / max(z_disp_m)) %>%
    pivot_longer(
        c(xy_disp_m, z_disp_m, surv_prob),
        names_to = "legend",
        values_to = "line") %>%
    mutate(legend = case_when(
        legend == "surv_prob" ~ "Flight Probability [%]",
        legend == "xy_disp_m" ~ "Normalised Distance [0:300m]",
        legend == "z_disp_m" ~ "Normalised Altitude [0:120m]"))

surv_plot <- ggplot() +
    geom_line(
        data = flight_log,
        aes(y = line, x = tend, colour = legend),
        size = 1) +
    geom_ribbon(
        data = filter(flight_log, legend == "Flight Probability [%]"),
        aes(x = tend, ymin = surv_lower, ymax = surv_upper),
        alpha = 0.3,
        fill = "black") +
    scale_color_manual(values = c("black", "green", "red")) +
    xlab("Time Since Launch [s]") +
    ylab("Normalised Values") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 40),
        axis.title = element_text(size = 40, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = c(0.70, 0.125),
        legend.box.background = element_rect(colour = "black"))

ggsave("plots/plot_survival.png", surv_plot, height = 10, width = 10)

# creating contour plot of flight probability for each species
advancing <- survival_data %>%
    mutate(z_disp_m = round(z_disp_m)) %>%
    filter(z_disp_m == altitude)

# Creating line at 50% flight probability with corresponding CI
ribbon <- advancing  %>%
    pivot_longer(
        contains("surv"),
        names_to = "Confidence Intervals",
        values_to = "fit") %>%
    mutate(`Confidence Intervals` = case_when(
        `Confidence Intervals` == "surv_prob" ~ "50% Flight Probability",
        `Confidence Intervals` == "surv_upper" ~ "95% Confidence Interval",
        `Confidence Intervals` == "surv_lower" ~ "95% Lower Confidence Interval"))

raw_data <- data_ped %>%
    mutate(data_ped, ped_status = case_when(
        ped_status == 1 ~ "Flight",
        ped_status == 0 ~ "No Flight")) %>%
    arrange(desc(ped_status))

fid_plot <- ggplot() +
    # create base contour plots
    geom_contour_filled(
        data = advancing,
        aes(x = xy_disp_m, y = z_disp_m, z = surv_prob),
        binwidth = 0.1) +
    # define colours for flight probability contours
    scale_fill_brewer(
        type = "div",
        palette = 8,
        direction = 1,
        aesthetics = "fill") +
    # add line and ribbons at 50% flight probability
    geom_contour(
        data = ribbon,
        aes(
            x = xy_disp_m,
            y = z_disp_m,
            z = fit,
            linetype = `Confidence Intervals`),
        colour = "black",
        binwidth = 0.5,
        size = 3) +
    # define colours for 50% flight prob
    scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
    # add raw flight or no flight endpoints for sub-2kg drones
    geom_point(
        data = raw_data,
        aes(x = xy_disp_m, y = z_disp_m,
        colour = factor(ped_status)),
        size = 10) +
    # define colours for raw data
    scale_color_manual(values = c("red", "green")) +
    # facet wrap by common name
    facet_wrap("common_name") +
    # make plot aesthetic
    theme_bw() +
    # scale_x_continuous(limits = c(0, 300.5), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    # scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Horizontal Distance [m]") +
    ylab("Altitude [m]") +
    labs(fill = "Predicted Flight Probability") +
    labs(colour = "Raw Data") +
    theme(
        panel.spacing = unit(5, "lines"),
        strip.text = element_text(size = 60, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "in"),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.15, "in"),
        axis.text = element_text(size = 60),
        axis.title = element_text(size = 80, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "in"),
        legend.title.align = 0.5,
        legend.text.align = 0,
        legend.box = "horizontal",
        legend.margin = margin(1, 2, 0, 2, unit = "in"),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60, face = "bold")) +
        guides(
            colour = guide_legend(
                nrow = 2,
                title.position = "top",
                title.hjust = 0.5),
            fill = guide_legend(
                nrow = 5,
                title.position = "top",
                title.hjust = 0.5),
            linetype = guide_legend(
                nrow = 3,
                title.position = "top",
                title.hjust = 0.5))
# save plot
ggsave("plots/plot_flight_initiation_distance.png", fid_plot, height = 60, width = 60, limitsize = FALSE)

check <- data_ped %>%
    filter(common_name == "bar tailed godwit") %>%
    select(test, approach, ped_status, z_disp_m, xy_disp_m)
