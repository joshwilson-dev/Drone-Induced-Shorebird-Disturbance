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
fit <- readRDS("models/dibd-model-26-06-22_21-33.rds")

summary(fit)

# create dataframes investigating fit of each model parameter individually
new_data <- function(var1, var2) {
    print(paste(var1, var2))
    df_i <- data.frame()
    var1_type <- typeof(eval(parse(text = paste0("data_ped$", var1))))
    # the below creates a dataframe varying the specified variables then
    # determines the contribution of that variable to the fit of the model
    new_dataframe <- data_ped %>%
        ungroup() %>%
        mutate(na = 0) %>%
        mutate(new_col1 = !!sym(var1)) %>%
        mutate(new_col2 = !!sym(var2)) %>%
    {
        # if there is no interaction term
        if (var2 == "na") {
            # if the explanitory variable is numerical
            if (var1_type == "double") {
                # create dataset with specified variable varying
                make_newdata(
                    .,
                    new_col1 = seq_range(!!sym(var1), n = 100)) %>%
                mutate(sentinel_flight = TRUE) %>%
                select(-!!sym(var1)) %>%
                rename({{var1}} := new_col1) %>%
                # use model to predict contribution of explanitory variable
                add_term(., fit, term = var1)
            }
            # if the explanitory variable is catagorical
            else {
                make_newdata(., new_col1 = unique(!!sym(var1))) %>%
                select(-!!sym(var1)) %>%
                rename({{var1}} := new_col1) %>%
                add_term(., fit, term = var1)
            }
        }
        # if there is an interaction term
        else {
            make_newdata(
                .,
                species = unique(species),
                new_col1 = seq_range(!!sym(var1), n = 100),
                new_col2 = seq_range(!!sym(var2), n = 100)) %>%
            select(-!!sym(var1)) %>%
            rename({{var1}} := new_col1) %>%
            select(-!!sym(var2)) %>%
            rename({{var2}} := new_col2) %>%
            add_term(., fit, term = c("species", paste0(var1, ",", var2)))
        }
    }
    df_i <- bind_rows(df_i, new_dataframe)
    assign(
        paste0("fit_", var1, "_", var2),
        df_i,
        envir = .GlobalEnv)
}

# run the above function for each explanitory variable
predictors <- data.frame(
    var1 = c(
        "specification",
        "distance_x",
        "velocity_x",
        "velocity_y",
        "velocity_z",
        "acceleration",
        "tend",
        "obscuring",
        "wind_speed",
        "cloud_cover",
        "high_tide",
        "temperature",
        "location",
        "normalised_count",
        "flock",
        "flight"),
    var2 = c(
        "na",
        "distance_z",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na",
        "na"))

invisible(mapply(new_data, predictors$var1, predictors$var2))

# plot the contribution of each explnitory variable
plot_fit <- function(var1, var2, target) {
    dataframe <- eval(parse(text = paste0("fit_", var1, "_", var2)))
    var1_type <- typeof(
        eval(parse(text = paste0("fit_", var1, "_", var2, "$", var1))))
    height <- 10
    width <- 10
    title <- paste0("plots/plot_", var1, "_", var2, ".png")
    # if no interaction
    if (var2 == "na") {
        # if numerical predictor
        if (var1_type == "double") {
            plot <- (
                ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
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
                ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
                coord_cartesian(ylim = c(-10, 10)) +
                geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
                ylab("Effect"))
        }
    }
    # if interaction
    else {
        dataframe <- dataframe %>%
        filter(!!sym(var1) >= 0, !!sym(var1) <= 500)
        title <- paste0("plots/plot_", var1, "_", var2, "_", target, ".png")
        plot <- ggplot(
                data = filter(dataframe, species == target),
                aes(x = .data[[var1]], y = .data[[var2]], z = fit)) +
            geom_raster(aes(fill = fit)) +
            geom_contour(colour = "black") +
            facet_wrap("species", scales = "free") +
            scale_fill_gradientn(colours = c("green", "red")) +
            xlab("Horizontal Distance [m]") +
            ylab("Altitude [m]")
    }
    # Setting axis labels
    if (var1 == "velocity_x") plot <- plot + xlab("Approach Velocity [m/s]")
    if (var1 == "velocity_y") plot <- plot + xlab("Perpendicular Velocity [m/s]")
    if (var1 == "velocity_z") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (var1 == "acceleration") plot <- plot + xlab("Acceleration [m/s/s]")
    if (var1 == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (var1 == "wind_speed") plot <- plot + xlab("Wind Speed [m/s]")
    if (var1 == "cloud_cover") plot <- plot + xlab("Cloud Cover [%]")
    if (var1 == "high_tide") plot <- plot + xlab("Time From High Tide [hr]")
    if (var1 == "temperature") plot <- plot + xlab("Temperature (\u00B0C)")
    if (var1 == "flight") plot <- plot + xlab("Flight")
    if (var1 == "flock") plot <- plot + xlab("Flock")
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
            legend.title = element_text(size = 30, face = "bold"))
    if (var1_type == "double") {
        plot <- plot +
            scale_x_continuous(expand = c(0, 0))}
    if (var1 == "species" | var1 == "specification" | var1 == "location") {
        height <- 15
        width <- 12.5
        plot <- plot +
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 0.95))}
    if (var1 == "flight" | var1 == "flock") {
        plot <- plot + theme(axis.text.x = element_blank())}
    # saving the plots
    ggsave(title, plot, height = height, width = width)
}

# running plot_fit function for each explanitory variable
mapply(plot_fit, predictors$var1, predictors$var2)
plot_fit("distance_x", "distance_z", "pied stilt near eastern curlew")
unique(data_ped$species)
