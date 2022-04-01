################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Analysis
# Author: Josh Wilson
# Date: 23-03-2022
# Reference: https://cran.r-project.org/web/packages/mgcv/mgcv.pdf

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("tidyverse", "mgcv", "visreg", "pammtools", "gridExtra")
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
data <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

data_clean <- data %>%
    # id is identifier for each test, flight, species
    group_by(test, flight, species) %>%
    mutate(id = cur_group_id()) %>%
    # test valid if drone is logging GPS and video is on
    filter(!is.na(xy_disp_m)) %>%
    filter(drone_latitude_d != 0 | drone_latitude_d != 0) %>%
    filter(!is.na(video_time_s)) %>%
    # set start time to drone launch
    mutate(video_time_s = round(video_time_s - first(video_time_s), 1)) %>%
    # test ends if birds take off
    group_by(test, flight, species, behaviour) %>%
    filter(behaviour == 0 | row_number() <= 1) %>%
    # we can take the abs of yb_vel_ms, because it's the same either way
    mutate(yb_vel_ms = abs(yb_vel_ms)) %>%
    # can also make wind dir symmetric because left and right are the same
    mutate(travel_rel_wind_dir_d = case_when(
        travel_rel_wind_dir_d > 180 ~ abs(travel_rel_wind_dir_d - 360),
        TRUE ~ travel_rel_wind_dir_d)) %>%
    # convert to factors
    mutate(
        eastern_curlew_presence = as.factor(eastern_curlew_presence),
        video_time_ms = as.factor(video_time_s * 10),
        location = as.factor(location),
        drone = as.factor(drone),
        common_name = as.factor(common_name)) %>%
    # keep only useful columns
    select(- notes)

data_fit <- data_clean %>%
    # remove times except if a flight event occurred in at least one approach
    group_by(video_time_s) %>%
    filter(!all(behaviour == 0)) %>%
    droplevels()

# Save Data Fit
write.csv(
    data_fit,
    "survival_function_fit_data.csv",
    row.names = FALSE)

summary(data_fit)

###################
#### Fit Model ####
###################

fit <- bam(
    behaviour ~
    video_time_ms - 1 +

    ## target
    s(common_name, bs = "fs") +
    s(eastern_curlew_presence, bs = "fs") +
    s(flock_number, bs = "re") +
    s(count) +

    ## drone
    s(drone, bs = "fs") +

    ## approach
    ti(z_disp_m, xy_disp_m) +
    # ti(z_vel_ms, xyz_disp_m) +
    # ti(xb_vel_ms, xyz_disp_m) +
    # ti(yb_vel_ms, xyz_disp_m) +
    # ti(xyz_acc_mss, xyz_disp_m) +
    # s(xyz_disp_m) +
    s(z_disp_m) +
    s(xy_disp_m) +
    s(z_vel_ms) +
    s(xb_vel_ms) +
    s(yb_vel_ms) +
    s(xyz_acc_mss) +

    ## environment
    # s(location, bs = "re") +
    s(month_aest, bs = "cc", k = 7) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(travel_rel_wind_dir_d, bs = "cc") +
    s(cloud_cover_p),

    family = poisson(),
    data = data_fit)
    # method = "REML")

################################
#### Save/Load Fitted Model ####
################################

save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
fit <- readRDS(choose.files())
visreg(fit, "drone")

#########################
#### Analysis of fit ####
#########################
summary(fit)

# Create New Data

# medium or mode of numerical and character/factor variables
ref <- data_fit %>%
    ungroup() %>%
    sample_info() %>%
    mutate(eastern_curlew_presence = as.logical(eastern_curlew_presence))

# make a dataframe using medium/mode for all values except chosen variable
new_data <- function(variable) {
    # get the original variable data so we can ceck its type later
    var_data <- eval(parse(text = paste0("data_fit$", variable)))
    print(variable)
    new_dataframe <- data_fit %>%
        ungroup() %>%
        mutate(new_col = !!sym(variable)) %>%
        # create new data
        {if (is.numeric(var_data)) {
            make_newdata(., new_col = seq_range(!!sym(variable), n = 10))}
        else if (is.character(var_data) | is.logical(var_data)) {
            make_newdata(., new_col = unique(!!sym(variable)))}} %>%
        select(-!!sym(variable)) %>%
        rename({{ variable }} := new_col) %>%
        mutate(
            eastern_curlew_presence =
            as.logical(eastern_curlew_presence)) %>%
        # predict fit for new data
        add_term(
            fit,
            term = variable,
            exclude = c("s(flock_number)", "s(location)"),
            reference = ref)
    assign(paste0(variable, "_df"), new_dataframe, envir = .GlobalEnv)
}

predictors <- c(
    "video_time_ms",
    "common_name",
    "eastern_curlew_presence",
    "count",
    "flock_number",
    "drone",
    "z_disp_m",
    "xy_disp_m",
    "z_vel_ms",
    "xb_vel_ms",
    "yb_vel_ms",
    "xyz_acc_mss",
    "location",
    "month_aest",
    "hrs_since_low_tide",
    "temperature_dc",
    "wind_speed_ms",
    "travel_rel_wind_dir_d",
    "cloud_cover_p")

mapply(new_data, predictors)

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(variable) {
    var_data <- eval(parse(text = paste0(variable, "_df$", variable)))
    fit <- eval(parse(text = paste0(variable, "_df$fit")))
    dataframe <- eval(parse(text = paste0(variable, "_df")))
    plot <- ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
    coord_cartesian(ylim = c(-5, 5)) +
    {if(is.character(var_data) | is.logical(var_data)) list(
        geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)),
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)))} +
    {if(is.numeric(var_data)) list(
        geom_line(),
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2))}
    assign(paste0(variable, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors)
do.call("grid.arrange", c(lapply(paste(predictors, "plot", sep = "_"), get)))