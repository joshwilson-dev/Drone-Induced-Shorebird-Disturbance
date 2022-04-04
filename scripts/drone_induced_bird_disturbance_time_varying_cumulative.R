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
packages <- c("tidyverse", "mgcv", "pammtools", "gridExtra")
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

data_fit <- data %>%
    # degrade data to log once a second for convergence
    filter(time %% 1 == 0) %>%
    # test ends if birds take off
    group_by(test, flight, species, behaviour) %>%
    filter(behaviour == 0 | row_number() <= 1) %>%
    # add end time and outcome
    group_by(id) %>%
    mutate(
        end_time = max(time),
        outcome = max(behaviour))

summary(data_fit)

# Save Data Fit
write.csv(
    data_fit,
    "survival_function_fit_data.csv",
    row.names = FALSE)

# getting the initial conditions and the time to event
event_df <- data_fit %>%
    select(
        id,
        common_name,
        count,
        count_eastern_curlew,
        flock_number,
        drone,
        z_disp_m,
        xy_disp_m,
        z_vel_ms,
        xb_vel_ms,
        yb_vel_ms,
        xyz_acc_mss,
        location,
        month_aest,
        hrs_since_low_tide,
        temperature_dc,
        wind_speed_ms,
        travel_rel_wind_dir_d,
        cloud_cover_p,
        outcome,
        end_time) %>%
    group_by(id) %>%
    slice(n()) %>%
    ungroup()

# time-dependent covariates dataset
tdc_df <- data_fit %>%
    select(
        id,
        time,
        z_disp_m,
        xy_disp_m,
        z_vel_ms,
        xb_vel_ms,
        yb_vel_ms,
        xyz_acc_mss,
        hrs_since_low_tide,
        travel_rel_wind_dir_d)

# data transformation note that the latency is dictated by the 5
ped <- as_ped(
    list(event_df, tdc_df),
    Surv(end_time, outcome) ~ . + cumulative(
        latency(time),
        z_disp_m,
        xy_disp_m,
        z_vel_ms,
        xb_vel_ms,
        yb_vel_ms,
        xyz_acc_mss,
        hrs_since_low_tide,
        travel_rel_wind_dir_d,
        tz_var = "time",
        ll_fun = function(t, tz) t >= tz &  t <= tz + 0),
  id = "id")
ped$time_latency <- ped$time_latency * ped$LL

###################
#### Fit Model ####
###################

mod <- gam(
    ped_status ~
    s(tend) +

    # target
    common_name +
    s(count_eastern_curlew) +
    s(flock_number, bs = "re") +
    s(count) +

    # drone
    drone +

    # approach
    # te(time_latency, z_disp_m, xy_disp_m, by = LL) +
    ti(z_disp_m, xy_disp_m) +
    s(z_disp_m) +
    s(xy_disp_m) +
    # te(time_latency, z_vel_ms, by = LL) +
    # te(time_latency, xb_vel_ms, by = LL) +
    # te(time_latency, yb_vel_ms, by = LL) +
    # te(time_latency, xyz_acc_mss, by = LL) +
    s(z_vel_ms) +
    s(xb_vel_ms) +
    s(yb_vel_ms) +
    s(xyz_acc_mss) +

    # environment
    # s(location, bs = "re") +
    s(month_aest, bs = "cc", k = 7) +
    # te(time_latency, hrs_since_low_tide, bs = "cc", by = LL) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    # te(time_latency, travel_rel_wind_dir_d, bs = "cc", by = LL) +
    s(travel_rel_wind_dir_d, bs = "cc") +
    s(cloud_cover_p),
    method = "REML",
    offset = offset,
    family = poisson(),
    data = ped)

summary(mod)

save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(mod, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
# fit <- readRDS(choose.files())

#########################
#### Analysis of fit ####
#########################

gg_tensor(mod, ci = TRUE) + xlab("latency (s)")

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