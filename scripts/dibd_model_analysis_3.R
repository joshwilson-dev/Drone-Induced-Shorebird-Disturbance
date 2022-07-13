################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Model Analysis
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
packages <- c("readr", "dplyr", "ggplot2", "mgcv", "pammtools")
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
    mutate(
        flock = as.factor(flock),
        flight = as.factor(flight))

#########################
#### Analysis of fit ####
#########################
# load model and print outputs
fit <- readRDS("models/dibd-model-13-07-22_15-38.rds")
summary(fit)

# create dataframes investigating fit of each model parameter individually
new_data <- function(variable) {
    print(variable)
    df_i <- data.frame()
    variable_type <- typeof(eval(parse(text = paste0("data_ped$", variable))))
    # the below creates a dataframe varying the specified variables then
    # determines the contribution of that variable to the fit of the model
    new_dataframe <- data_ped %>%
        ungroup() %>%
        mutate(na = 0) %>%
        mutate(new_col1 = !!sym(variable)) %>%
    {
        # if altitude then interaction with species
        if (variable == "distance_z" | variable == "normalised_count") {
            # create dataset with specified variable varying
            make_newdata(
                .,
                new_col1 = seq_range(!!sym(variable), n = 100),
                species = unique(species)) %>%
            mutate(sentinel_flight = TRUE) %>%
            select(-!!sym(variable)) %>%
            rename({{variable}} := new_col1) %>%
            # use model to predict contribution of explanitory variable
            add_term(., fit, term = c(variable, "species"))
        }
        # if the explanitory variable is numerical
        else if (variable_type == "double") {
            # create dataset with specified variable varying
            make_newdata(
                .,
                new_col1 = seq_range(!!sym(variable), n = 100)) %>%
            mutate(sentinel_flight = TRUE) %>%
            select(-!!sym(variable)) %>%
            rename({{variable}} := new_col1) %>%
            # use model to predict contribution of explanitory variable
            add_term(., fit, term = variable)
        }
        # if the explanitory variable is catagorical
        else {
            make_newdata(., new_col1 = unique(!!sym(variable))) %>%
            select(-!!sym(variable)) %>%
            rename({{variable}} := new_col1) %>%
            add_term(., fit, term = variable)
        }
    }
    df_i <- bind_rows(df_i, new_dataframe)
    assign(
        paste0("fit_", variable),
        df_i,
        envir = .GlobalEnv)
}

# run the above function for each explanitory variable
variables <- c(
    "specification",
    "distance_x",
    "distance_z",
    # "velocity_x",
    # "velocity_y",
    # "velocity_z",
    # "acceleration",
    "tend",
    "obscuring",
    # "wind_speed",
    # "cloud_cover",
    # "high_tide",
    # "temperature",
    # "location",
    "species",
    "sentinel",
    "normalised_count",
    # "flock",
    "flight")

invisible(mapply(new_data, variables))

# plot the contribution of each explnitory variable
plot_fit <- function(variable, target) {
    dataframe <- eval(parse(text = paste0("fit_", variable)))
    variable_type <- typeof(
        eval(parse(text = paste0("fit_", variable, "$", variable))))
    height <- 10
    width <- 10
    title <- paste0("plots/", variable, ".png")
    # if distance, then interaction with species
    if (variable == "distance_z" | variable == "normalised_count") {
        plot <- (
            ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
            geom_line() +
            coord_cartesian(ylim = c(-10, 10)) +
            geom_ribbon(
                aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2) +
            ylab("Effect")) +
            facet_wrap("species")
        height <- 17
        width <- 17
    }
    # if numerical predictor
    else if (variable_type == "double") {
        plot <- (
            ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
            geom_line() +
            coord_cartesian(ylim = c(-10, 10)) +
            geom_ribbon(
                aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2) +
            ylab("Effect"))
    }
    # if categorical predictor
    else {
        plot <- (
            ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
            coord_cartesian(ylim = c(-10, 10)) +
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
            ylab("Effect"))
    }
    # Setting axis labels
    if (variable == "distance_x") plot <- plot + xlab("Ground Distance [m]")
    if (variable == "distance_z") plot <- plot + xlab("Height [m]")
    if (variable == "velocity_x") plot <- plot + xlab("Approach Velocity [m/s]")
    if (variable == "velocity_y") plot <- plot + xlab("Perpendicular Velocity [m/s]")
    if (variable == "velocity_z") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (variable == "acceleration") plot <- plot + xlab("Acceleration [m/s/s]")
    if (variable == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (variable == "wind_speed") plot <- plot + xlab("Wind Speed [m/s]")
    if (variable == "cloud_cover") plot <- plot + xlab("Cloud Cover [%]")
    if (variable == "high_tide") plot <- plot + xlab("Time From High Tide [hr]")
    if (variable == "temperature") plot <- plot + xlab("Temperature (\u00B0C)")
    if (variable == "normalised_count") plot <- plot + xlab("Normalised Count")
    if (variable == "flight") plot <- plot + xlab("Flight")
    if (variable == "flock") plot <- plot + xlab("Flock")
    # making plot aesthetics
    plot <- plot +
        theme_bw() +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "in"),
            axis.ticks = element_line(size = 2),
            axis.ticks.length = unit(.15, "in"),
            axis.text = element_text(size = 40),
            axis.title = element_text(size = 40, face = "bold"),
            legend.position = c(.90, .23),
            legend.key.size = unit(0.5, "in"),
            legend.title.align = 0.5,
            legend.text.align = 0.5,
            legend.box.background = element_rect(color = "black", size = 1),
            legend.text = element_text(size = 30),
            legend.title = element_text(size = 30, face = "bold"),
            panel.spacing = unit(2, "cm"),
            strip.text = element_text(size = 25))
    if (variable_type == "double") {
        plot <- plot +
            scale_x_continuous(expand = c(0, 0))}
    if (variable == "species" | variable == "specification" | variable == "location") {
        height <- 15
        width <- 12.5
        plot <- plot +
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 0.95))}
    if (variable == "flight" | variable == "flock") {
        plot <- plot + theme(axis.text.x = element_blank())}
    # saving the plots
    ggsave(title, plot, height = height, width = width)
}

# running plot_fit function for each explanitory variable
mapply(plot_fit, variables)