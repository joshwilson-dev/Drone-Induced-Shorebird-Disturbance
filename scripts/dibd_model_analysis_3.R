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
    mutate(flight = as.factor(flock))

#########################
#### Analysis of fit ####
#########################
# load model and print outputs
fit <- readRDS("models/dibd-model-24-06-22_12-21.rds")

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
        mutate(new_column = !!sym(variable)) %>%
    {
        # if the explanitory variable is numerical
        if (variable_type == "double") {
            # create dataset with specified variable varying
            make_newdata(
                .,
                new_column = seq_range(!!sym(variable), n = 100)) %>%
            select(-!!sym(variable)) %>%
            rename({{variable}} := new_column) %>%
            # use model to predict contribution of explanitory variable
            add_term(., fit, term = variable)
        }
        # if the explaitory variable is catagorical
        else {
            make_newdata(., new_column = unique(!!sym(variable))) %>%
            select(-!!sym(variable)) %>%
            rename({{variable}} := new_column) %>%
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
        "drone",
        "xy_disp_m",
        "z_disp_m",
        "xb_vel_ms",
        "z_vel_ms",
        # "xyz_acc_mss",
        "tend",
        "drone_obscured",
        "wind_speed_ms",
        "cloud_cover_p",
        "hrs_from_high",
        "temperature_dc",
        "location",
        "count",
        "flock")

invisible(mapply(new_data, variables))

# plot the contribution of each explnitory variable
plot_fit <- function(variable) {
    dataframe <- eval(parse(text = paste0("fit_", variable)))
    variable_type <- typeof(
        eval(parse(text = paste0("fit_", variable, "$", variable))))
    height <- 10
    width <- 10
    title <- paste0("plots/plot_", variable, ".png")

    # if numerical predictor
    if (variable_type == "double") {
        plot <- (
            ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
            geom_line() +
            coord_cartesian(ylim = c(-10, 10)) +
            geom_ribbon(
                aes(ymin = ci_lower, ymax = ci_upper),
                alpha = 0.2) +
            ylab("Contribution"))
    }
    # if categorica predictor
    else {
        plot <- (
            ggplot( data = dataframe, aes(.data[[variable]], y = fit)) +
            coord_cartesian(ylim = c(-10, 10)) +
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
            ylab("Contribution"))
    }

    # Setting axis labels
    if (variable == "xb_vel_ms") plot <- plot + xlab("Approach Velocity [m/s]")
    if (variable == "z_vel_ms") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (variable == "xyz_acc_mss") plot <- plot + xlab("Acceleration [m/s/s]")
    if (variable == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (variable == "drone_obscured") plot <- plot + xlab("Drone Obscured")
    if (variable == "wind_speed_ms") plot <- plot + xlab("Wind Speed [m/s]")
    if (variable == "cloud_cover_p") plot <- plot + xlab("Cloud Cover [%]")
    if (variable == "hrs_from_high") plot <- plot + xlab("Time From High Tide [hr]")
    if (variable == "temperature_dc") plot <- plot + xlab("Temperature (\u00B0C)")
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
            legend.title = element_text(size = 30, face = "bold"))
    if (variable_type == "double") {
        plot <- plot +
            scale_x_continuous(expand = c(0, 0))}
    if (variable == "drone" | variable == "location") {
        height <- 15
        width <- 12.5
        plot <- plot +
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 0.95))}
    if (variable == "flock") {
        plot <- plot + theme(axis.text.x = element_blank())}
    # saving the plots
    ggsave(title, plot, height = height, width = width)
}

# running plot_fit function for each explanitory variable
mapply(plot_fit, variables)
