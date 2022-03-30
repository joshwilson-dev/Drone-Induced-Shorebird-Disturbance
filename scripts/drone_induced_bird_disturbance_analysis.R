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
    # degrade data so it converges faster
    group_by(test, flight, species) %>%
    # filter(behaviour == 1 | row_number() %% 1 == 1) %>%
    # mutate(video_time_s = video_time_s / 1) %>%
    filter(common_name == "pied stilt") %>%
    filter(eastern_curlew_presence == FALSE) %>%
    filter(drone == "phantom 4 pro") %>%
    # we can take the abs of yb_vel_ms, because it's the same either way
    mutate(yb_vel_ms = abs(yb_vel_ms)) %>%
    # also take the abs of xyz_acc_ms as I only want to check the noise
    mutate(xyz_acc_mss = abs(xyz_acc_mss)) %>%
    # can also make wind dir symmetric because left and right are the same
    mutate(travel_rel_wind_dir_d = case_when(
        travel_rel_wind_dir_d > 180 ~ abs(travel_rel_wind_dir_d - 360),
        TRUE ~ travel_rel_wind_dir_d)) %>%
    # remove all data except where some flight event occurred
    group_by(video_time_s) %>%
    filter(!all(behaviour == 0)) %>%
    mutate(video_time_ms = as.factor(video_time_s * 10)) %>%
    # set up dataset for pam
    # group_by(test, flight, species) %>%
    # mutate(
    #     tstart = video_time_s * 10,
    #     tend = lead(video_time_s * 10),
    #     interval = tend - tstart,
    #     offset = log(interval),
    #     ped_status = case_when(
    #         tend == last(video_time_s * 10) ~ lead(behaviour),
    #         TRUE ~ behaviour)) %>%
    # drop_na(tend) %>%
    # keep only useful columns
    select(- notes)

summary(data_fit)

fit <- bam(
    behaviour ~
    video_time_ms - 1 +

    ## target
    # common_name +
    # eastern_curlew_presence +
    # s(common_name, bs = "fs") +
    # s(eastern_curlew_presence, bs = "fs") +
    s(flock_number, bs = "re") +
    # s(count) +

    ## drone
    # s(drone, bs = "fs") +
    # drone +

    ## approach
    s(z_disp_m) +
    s(xy_disp_m),
    # s(z_vel_ms) +
    # s(xb_vel_ms) +
    # s(yb_vel_ms) +
    # s(bird_rel_dir_travel_d, bs = "cc") +
    # s(xyz_acc_mss) +

    ## environment
    # location +
    # s(location, bs = "fs") +
    # s(month_aest, bs = "cc", k = 7) +
    # s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    # s(wind_speed_ms) +
    # s(travel_rel_wind_dir_d, bs = "cc") +
    # s(travel_rel_wind_dir_d, bs = "cc", by = wind_speed_ms) +
    # s(cloud_cover_p),

    family = poisson(),
    # correlation = corAR1(form = ~ video_time_ms),
    data = data_fit)

fit <- gam(
    ped_status ~
    s(tend) +

    ## target
    # common_name +
    # eastern_curlew_presence +
    # s(common_name, bs = "fs") +
    # s(eastern_curlew_presence, bs = "fs") +
    s(flock_number, bs = "re") +
    # s(count) +

    ## drone
    # s(drone, bs = "fs") +
    # drone +

    ## approach
    s(z_disp_m) +
    s(xy_disp_m),
    # s(z_vel_ms) +
    # s(xb_vel_ms) +
    # s(yb_vel_ms) +
    # s(bird_rel_dir_travel_d, bs = "cc") +
    # s(xyz_acc_mss) +

    ## environment
    # location +
    # s(location, bs = "fs") +
    # s(month_aest, bs = "cc", k = 7) +
    # s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    # s(wind_speed_ms) +
    # s(travel_rel_wind_dir_d, bs = "cc") +
    # s(travel_rel_wind_dir_d, bs = "cc", by = wind_speed_ms) +
    # s(cloud_cover_p),

    ## model
    data = data_fit,
    # family = poisson(),
    family = binomial(),
    # correlation = corAR1(form = ~ tend),
    offset = offset)

###########################
#### Save Fitted Model ####
###########################

# saveRDS(fit, paste0("drone-induced-bird-disturbance-gam-", Sys.Date(), ".rds"))
# fit <- readRDS("./drone-induced-shorebird-disturbance-git/models/drone-induced-bird-disturbance-gam-2022-03-30.rds")

summary(fit)

#########################
#### Create New Data ####
#########################

ref <- sample_info(ungroup(data_fit)) %>%
    mutate(eastern_curlew_presence = as.logical(eastern_curlew_presence))

new_data <- function(colname) {
    col_type <- typeof(eval(parse(text = paste0("data_fit$", colname))))
    print(colname)
    print(col_type)
    new_dataframe <- data_fit %>%
        ungroup() %>%
        mutate(new_col = !!sym(colname)) %>%
        {if (col_type == "double") {
            make_newdata(., new_col = seq_range(!!sym(colname), n = 100))}
        else if (col_type == "character" | col_type == "logical" | col_type == "integer") {
            make_newdata(., new_col = unique(!!sym(colname)))}} %>%
        select(-!!sym(colname)) %>%
        rename({{ colname }} := new_col) %>%
        mutate(
            eastern_curlew_presence =
            as.logical(eastern_curlew_presence)) %>%
        add_term(
            fit,
            term = colname,
            exclude = c("s(flock_number)"),
            reference = ref)
    response <- data.frame(predict.gam(
        object = fit,
        newdata = new_dataframe,
        trans = binomial()$linkinv,
        type = "response",
        se.fit = TRUE)) %>%
        rename(response = fit, se.response = se.fit)
    new_dataframe <- cbind(new_dataframe, response)
    assign(paste0(colname, "_df"), new_dataframe, envir = .GlobalEnv)
}

predictors <- c(
    "video_time_ms",
    # "common_name",
    # "eastern_curlew_presence",
    # "count",
    "flock_number",
    # "drone",
    "z_disp_m",
    "xy_disp_m")
    # "z_vel_ms",
    # "xb_vel_ms",
    # "yb_vel_ms",
    # "xyz_acc_mss",
    # "location",
    # "month_aest",
    # "hrs_since_low_tide",
    # "temperature_dc",
    # "wind_speed_ms",
    # "travel_rel_wind_dir_d",
    # "cloud_cover_p")

mapply(new_data, predictors)

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(colname) {
    col_type <- typeof(eval(parse(text = paste0(colname, "_df$", colname))))
    fit <- eval(parse(text = paste0(colname, "_df$fit")))
    dataframe <- eval(parse(text = paste0(colname, "_df")))
    plot <- ggplot(data = dataframe, aes(.data[[colname]], y = fit)) +
    coord_cartesian(ylim = c(-5, 5)) +
    {if(col_type == "character" | col_type == "logical") {
        geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper))}} +
    {if(col_type == "double") {geom_line()}}
    {if(col_type == "double") {
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)}}
    assign(paste0(colname, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors)
do.call("grid.arrange", c(lapply(paste(predictors, "plot", sep = "_"), get)))

ggplot(data = common_name_df, aes(x = common_name, y = fit)) +
ylim(-5, 5) +
geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

###########################
#### Survival Function ####
###########################

unique(data_fit$test)

## compute residuals... cumulative hazard
cumulative_hazard <- tapply(fitted(fit), data_fit$id, sum) ## cum haz by subject
censor_indicator <- tapply(data_fit$behaviour, data_fit$id, sum) ## censoring indicator
martingale <- censor_indicator - cumulative_hazard ## Martingale
deviance <- sign(martingale) * sqrt(-2 * (martingale + censor_indicator * log(cumulative_hazard))) ## deviance

## plot survivor function and s.e. band for subject 25
event_time <- sort(unique(data_fit$video_time_ms)) ## event times
event_time
data_i <- data_clean  %>%
    filter(test ==  15 & flight == 6 & common_name == "pied stilt") %>%
    mutate(video_time_ms = as.factor(round(video_time_s * 10))) %>%
    filter(video_time_ms %in% event_time == TRUE)

predictions <- predict(fit, newdata = data_i, type = "lpmatrix")

time_estimate <- drop(predictions %*% coef(fit))
hazard <- cumsum(exp(time_estimate))
jacob <- apply(exp(time_estimate) * predictions, 2, cumsum)

standard_error <- diag(jacob %*% vcov(fit) %*% t(jacob))^.5

plot(c(0, event_time), c(1, exp(-hazard)), ylim = c(0, 1), ylab="S(t)", xlab="t(ms)")

lines(c(0, event_time, c(1, exp(-hazard + standard_error))), do.points = FALSE)
lines(stepfun(event_time, c(1, exp(-hazard - standard_error))), do.points = FALSE)
rug(pbcseq$day[pbcseq$id==25]) ## measurement times

100 %in% event_time

####################################
#### Flight Initiation Distance ####
####################################

# Plotting the FID vs altitude under normal conditions
new_dataframe <- data_fit %>%
    ungroup() %>%
    make_newdata(
        .,
        # tend = seq_range(0:2500, by = 100),
        video_time_ms = unique(video_time_ms),
        common_name = unique(common_name),
        # eastern_curlew_presence = c(TRUE),
        # drone = c("mavic 2 pro"),
        z_disp_m = seq_range(10:120, by = 10),
        xy_disp_m = seq_range(0:1000, by = 10),
        flock_number = unique(flock_number))
        # z_vel_ms = c(0),
        # xb_vel_ms = c(5),
        # yb_vel_ms = c(0),
        # travel_rel_wind_dir_d = c(90)) %>%
    # mutate(
    #     eastern_curlew_presence =
    #     as.logical(eastern_curlew_presence))

response <- data.frame(predict.gam(
    object = fit,
    newdata = new_dataframe,
    trans = binomial()$linkinv,
    type = "response",
    exclude = c("s(video_time_ms)", "s(flock_number)"),
    se.fit = TRUE))

new_dataframe <- cbind(new_dataframe, response)

ggplot(filter(new_dataframe, z_disp_m == 20), aes(x = xy_disp_m, y = fit, group = common_name, colour = common_name)) + 
  geom_line() +
  coord_cartesian(ylim = c(0, 1))
