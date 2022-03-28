################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Analysis
# Author: Josh Wilson
# Date: 23-03-2022
# Reference: https://adibender.github.io/pammtools/articles/tdcovar.html

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
data_clean <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################
data_fit <- data_clean %>%
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
    # set up dataset for pam
    group_by(test, flight, species) %>%
    mutate(
        tstart = video_time_s * 10,
        tend = lead(video_time_s * 10),
        interval = tend - tstart,
        offset = log(interval),
        ped_status = case_when(
            tend == last(video_time_s * 10) ~ lead(behaviour),
            TRUE ~ behaviour)) %>%
    drop_na(tend) %>%
    # keep only useful columns
    select(- notes)

summary(data_fit)

fit <- gam(
    ped_status ~
    s(tend) +

    ## target
    common_name +
    eastern_curlew_presence +
    s(count) +
    s(flock_number, bs = "re") +

    ## drone
    drone +

    ## approach
    s(z_disp_m) +
    s(xy_disp_m) +
    s(z_vel_ms) +
    s(xy_vel_ms) +
    s(rel_dir_travel_d, bs = "cc") +
    # s(rel_dir_travel_d, bs = "cc", by = xy_vel_ms) +
    s(z_acc_mss) +
    s(xy_acc_mss) +

    ## environment
    location +
    s(month_aest, bs = "cc", k = 7) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(drone_rel_wind_dir_d, bs = "cc") +
    # s(drone_rel_wind_dir_d, bs = "cc", by = wind_speed_ms) +
    s(cloud_cover_p),

    ## model
    data = data_fit,
    # family = poisson(),
    family = binomial(),
    offset = offset)

summary(fit)

###########################
#### Save Fitted Model ####
###########################

saveRDS(fit, paste0("drone-induced-bird-disturbance-gam-", Sys.Date(), ".rds"))
# fit <- readRDS("./models/drone-induced-bird-disturbance-gam-2022-03-25.rds")

#########################
#### Create New Data ####
#########################

ref <- sample_info(ungroup(data_fit))

new_data <- function(colname) {
    col_type <- typeof(eval(parse(text = paste0("data_fit$", colname))))
    print(colname)
    if (col_type == "double") {
        new_dataframe <- data_fit %>%
            ungroup() %>%
            mutate(new_col = !!sym(colname)) %>%
            make_newdata(new_col = seq_range(!!sym(colname), n = 100)) %>%
            select(-!!sym(colname)) %>%
            rename({{ colname }} := new_col) %>%
            add_term(
                fit,
                term = colname,
                exclude = c("s(flock_number)"),
                reference = ref)
    }
    else if (col_type == "character") {
        new_dataframe <- data_fit %>%
            ungroup() %>%
            mutate(new_col = !!sym(colname)) %>%
            make_newdata(new_col = unique(!!sym(colname))) %>%
            select(-!!sym(colname)) %>%
            rename({{ colname }} := new_col) %>%
            add_term(
                fit,
                term = colname,
                exclude = c("s(flock_number)"),
                reference = ref)
    }
    assign(paste0(colname, "_df"), new_dataframe, envir = .GlobalEnv)
}

predictors <- c(
    "tend",
    "common_name",
    "eastern_curlew_presence",
    "count",
    "flock_number",
    "drone",
    "z_disp_m",
    "xy_disp_m",
    "z_vel_ms",
    "xy_vel_ms",
    "z_acc_mss",
    "xy_acc_mss",
    "location",
    "month_aest",
    "hrs_since_low_tide",
    "temperature_dc",
    "wind_speed_ms",
    "rel_wind_dir_d",
    "cloud_cover_p")

mapply(new_data, predictors)

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(colname) {
    col_type <- typeof(eval(parse(text = paste0(colname, "_df$", colname))))
    fit <- eval(parse(text = paste0(colname, "_df$fit")))
    dataframe <- eval(parse(text = paste0(colname, "_df")))
    plot <- ggplot(data = dataframe, aes(.data[[colname]], y = fit)) +
    # ylim(-10, 10) +
    {if(col_type == "character") {
        geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper))}} +
    {if(col_type == "double") {
        geom_line()}} +
    {if(col_type == "double") {
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)}}
    assign(paste0(colname, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors)
do.call("grid.arrange", c(lapply(paste(predictors, "plot", sep = "_"), get)))

ggplot(data = location_df, aes(x = location, y = fit)) +
ylim(-10, 10) +
geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

################################
#### Response Visualisation ####
################################