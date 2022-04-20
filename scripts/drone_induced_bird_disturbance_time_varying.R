################
#### Header ####
################

# Title: Drone Induced Bird Disturbance - Time-Varying Effects
# Author: Josh Wilson
# Date: 23-03-2022
# Reference: https://cran.r-project.org/web/packages/mgcv/mgcv.pdf

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

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

# load packages, install if not available provided user approves
lapply(packages, require, character.only = TRUE)

##########################
#### Data Preparation ####
##########################

# import data
data <- read_csv(choose.files(), guess_max = 1000000)

trial <- data %>%
    filter(test == 157) %>%
    filter(flight == 3)

ggplot(trial, aes(x = time)) +
geom_line(aes(y = xy_disp_m / 100)) +
geom_line(aes(y = z_disp_m  / 100)) +
geom_line(aes(y = xyz_acc_mss_2savg)) +
geom_line(aes(y = behaviour))
# prepare train and test data in ped format
prepare_data <- function(df, istrain) {
    if (istrain) {
        df <- df %>%
            filter(time %% 1 == 0) %>%
            group_by(test, flight, species, behaviour) %>%
            # test ends if birds take flight
            filter(behaviour == 0 | row_number() <= 1)
    }

    # acceleration seems to occur ~ 3s before reaction
    data_clean <- df %>%
        filter(time %% 1 == 0) %>%
        # mutate(time = time * 10) %>%
        group_by(test, flight, species) %>%
        mutate(
            xyz_acc_mss_lag =
            lag(xyz_acc_mss_2savg, 3, default = first(xyz_acc_mss_2savg)))

    event_df <- data_clean %>%
        # add end time and status
        group_by(id) %>%
        mutate(
            end_time = max(time),
            status = max(behaviour)) %>%
        group_by(id) %>%
        filter(row_number() == 1) %>%
        select(
            id,
            end_time,
            test,
            flight,
            behaviour,
            status,
            common_name,
            count_eastern_curlew,
            drone,
            flock_number,
            z_disp_m,
            xy_disp_m,
            xyz_acc_mss_2savg,
            xyz_acc_mss_lag)

    tdc_df <- data_clean %>%
        group_by(id) %>%
        filter(row_number() != n()) %>%
        select(
            id,
            time,
            test,
            flight,
            behaviour,
            common_name,
            count_eastern_curlew,
            drone,
            flock_number,
            z_disp_m,
            xy_disp_m,
            xyz_acc_mss_2savg,
            xyz_acc_mss_lag)

    data_ped <- as_ped(
        data = list(event_df, tdc_df),
        formula = Surv(end_time, status) ~ . +
        concurrent(z_disp_m, xy_disp_m, xyz_acc_mss_lag, xyz_acc_mss_2savg, tz_var = "time"),
        id = "id")
    return(data_ped)
}

data_ped_train <- prepare_data(data, TRUE)
data_ped_test <- prepare_data(data, FALSE)

View(head(data_ped_train, 1000))

###################
#### Fit Model ####
###################

# fit model
fit <- gam(
    ped_status ~
    s(tend) +

    # target
    common_name +
    s(count_eastern_curlew) +
    s(flock_number, bs = "re") +
    # s(count) +

    # drone
    drone +

    # approach
    # s(z_disp_m) +
    # s(xy_disp_m) +
    te(z_disp_m, xy_disp_m) +
    # s(xyz_acc_mss_lag),
    # s(xyz_acc_mss_lag, k = 4),
    xyz_acc_mss_lag,
    # te(z_vel_ms, xy_disp_m) +
    # s(z_vel_ms) +
    # s(xb_vel_ms) +
    # s(yb_vel_ms) +

    # environment
    # s(month_aest, bs = "cc", k = 5) +
    # s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    # s(wind_speed_ms) +
    # s(travel_rel_wind_dir_d, bs = "cc") +
    # s(cloud_cover_p),
    data = data_ped_train,
    family = poisson(),
    offset = offset)

# save model
save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))

# load model
fit <- readRDS(choose.files())

#########################
#### Analysis of fit ####
#########################

# check summary
summary(fit)

# create dataframes varing each explanatory variable one at a time while
# holdin others at medium or mode of numerical and character/factor variables
ref1 <- data_ped_train %>%
    ungroup() %>%
    sample_info() %>%
    mutate(count_eastern_curlew = 0)

ref2 <- data_ped_train %>%
    filter(count_eastern_curlew > 0) %>%
    ungroup() %>%
    sample_info()

new_data <- function(variable1, variable2) {
    # get the original variable data so we can check its type later
    var_type1 <- typeof(eval(parse(text = paste0("data_ped_train$", variable1))))
    print(var_type1)
    print(paste0(variable1, variable2))
    new_dataframe <- data_ped_train %>%
        ungroup() %>%
        mutate(new_col1 = !!sym(variable1)) %>%
        # create new data
        {if (!is.na(variable2)) {
            mutate(., new_col2 = !!sym(variable2)) %>%
            make_newdata(
                .,
                new_col1 = seq_range(!!sym(variable1), n = 100),
                new_col2 = seq_range(!!sym(variable2), n = 100)) %>%
            select(-!!sym(variable1)) %>%
            rename({{ variable1 }} := new_col1) %>%
            select(-!!sym(variable2)) %>%
            rename({{ variable2 }} := new_col2) %>%
            mutate(count_eastern_curlew = case_when(
                common_name == "eastern_curlew" ~ ref2$count_eastern_curlew,
                TRUE ~ 0)) %>%
            add_term(
                fit,
                term = paste0(variable1, ",", variable2),
                exclude = c("s(flock_number)"),
                reference = ref1)}
        else if (var_type1 == "double") {
            make_newdata(., new_col1 = seq_range(!!sym(variable1), n = 100)) %>%
            mutate(count_eastern_curlew = case_when(
                common_name == "eastern_curlew" ~ ref1$count_eastern_curlew,
                TRUE ~ 0)) %>%
            select(-!!sym(variable1)) %>%
            rename({{ variable1 }} := new_col1) %>%
            add_term(
                fit,
                term = variable1,
                exclude = c("s(flock_number)"),
                reference = ref1)}
        else if (var_type1 == "character") {
            make_newdata(., new_col1 = unique(!!sym(variable1))) %>%
            select(-!!sym(variable1)) %>%
            rename({{ variable1 }} := new_col1) %>%
            add_term(
                fit,
                term = variable1,
                exclude = c("s(flock_number)"),
                reference = ref1)}}
    if (variable1 == "common_name") {
        ec_dataframe <- data_ped_train %>%
            ungroup() %>%
            mutate(new_col1 = ref2$count_eastern_curlew) %>%
            make_newdata(., new_col1 = c("eastern_curlew")) %>%
            select(-!!sym(variable1)) %>%
            rename({{ variable1 }} := new_col1) %>%
            mutate(count_eastern_curlew = 61.7517) %>%
            add_term(
                fit,
                term = "count_eastern_curlew",
                exclude = c("s(flock_number)"),
                reference = ref1)
        new_dataframe <- new_dataframe %>%
            filter(common_name != "eastern_curlew")
        new_dataframe <- bind_rows(new_dataframe, ec_dataframe)
    }
    assign(paste0(variable1, "_", variable2, "_df"), new_dataframe, envir = .GlobalEnv)
}

predictors <- data.frame(
    variable1 = c(
        "common_name",
        "drone",
        "count_eastern_curlew",
        "flock_number",
        # "count",
        "z_disp_m",
        # "xyz_acc_mss_2savg"),
        "xyz_acc_mss_lag"),
        # "z_vel_ms",
        # "xb_vel_ms",
        # "yb_vel_ms",
        # "xyz_acc_mss_lag",
        # "month_aest",
        # "hrs_since_low_tide",
        # "temperature_dc",
        # "wind_speed_ms",
        # "travel_rel_wind_dir_d",
        # "cloud_cover_p"),
    variable2 = c(
        NA,
        NA,
        NA,
        NA,
        "xy_disp_m",
        NA))
        # NA,
        # NA))
        # NA,
        # NA,
        # NA,
        # NA,
        # NA,
        # NA,
        # NA,
        # NA))

invisible(mapply(new_data, predictors$variable1, predictors$variable2))

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(variable1, variable2) {
    dataframe <- eval(parse(text = paste0(variable1, "_", variable2, "_df")))
    var_type1 <- typeof(eval(parse(text = paste0(variable1, "_", variable2, "_df$", variable1))))
    fit <- eval(parse(text = paste0(variable1, "_", variable2, "_df$fit")))
    {if (!is.na(variable2)) {
        plot <- ggplot(data = filter(dataframe, !!sym(variable1) < 500), aes(x = .data[[variable2]], y = .data[[variable1]], z = fit)) +
        geom_raster(aes(fill = fit)) +
        scale_fill_gradientn(colours = c("green", "red"))
    }
    else {
        plot <- ggplot(data = dataframe, aes(.data[[variable1]], y = fit)) +
        coord_cartesian(ylim = c(-10, 10)) +
        {if(var_type1 == "integer" | var_type1 == "character") list(
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)),
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)))} +
        {if(var_type1 == "double") list(
            geom_line(),
            geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2))}}}
    assign(paste0(variable1, "_", variable2, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors$variable1, predictors$variable2)

show_plots <- function(variable1, variable2) {
    windows()
    par(ask = TRUE)
    for (i in 1:length(variable1)) {
        plot(eval(parse(text = paste(variable1[i], variable2[i], "plot", sep = "_"))))
    }
    par(ask = FALSE)
}

show_plots(predictors$variable1, predictors$variable2)

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight_log <- data_ped_test %>%
    filter(test == 157 & flight == 3) %>%
    filter(common_name == "whimbrel") %>%
    mutate(intlen = tend - tstart) %>%
    add_cumu_hazard(fit, exclude = "s(flight_number)") %>%
    add_surv_prob(fit, exclude = "s(flight_number)")

ggplot(
    flight_log,
    aes(x = tend, y = cumu_hazard, ymin = cumu_lower, ymax = cumu_upper)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
coord_cartesian(ylim = c(0, 5))

ggplot(
    flight_log,
    aes(x = tend, y = surv_prob, ymin = surv_upper, ymax = surv_lower)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
geom_line(aes(y = xy_disp_m / max(xy_disp_m)), colour = "red") +
geom_line(aes(y = z_disp_m / max(z_disp_m)), colour = "green") +
geom_line(aes(y = xyz_acc_mss_lag / max(xyz_acc_mss_lag)), colour = "blue") +
coord_cartesian(ylim = c(0, 1))
##################################################
#### Flight Initiation Distance Visualisation ####
##################################################

# use middle value for all variables, except species, altitude, distance, drone

ref_species <- data_ped_train %>%
    group_by(species) %>%
    sample_info() %>%
    mutate(count_eastern_curlew = case_when(
        common_name == "eastern_curlew" ~ count_eastern_curlew,
        TRUE ~ 0))

ref <- data_ped_train %>%
    ungroup() %>%
    sample_info()

log_simulator <- function(fit, ref_species, ref, altitude_list,
target_species_list, drone_type) {
    df_i <- data.frame()
    for (i in 1:length(target_birds)) {
        target_species <- target_species_list[i]
        for (a in 1:length(altitude_list)) {
            altitude <- altitude_list[a]
            xy_disp_m <- c(300)
            xy_disp_m_i <- xy_disp_m[1]
            velocity <- 5

            while (xy_disp_m_i > 0) {
                xy_disp_m_i <- xy_disp_m_i - velocity
                xy_disp_m <- append(xy_disp_m, xy_disp_m_i)
            }

            tend <- seq(1, length(xy_disp_m))
            common_name <- rep(target_species, length(xy_disp_m))
            count_eastern_curlew <- rep(0, length(xy_disp_m))
            count_eastern_curlew <- rep(
                filter(ref_species, common_name == target_species)$count_eastern_curlew,
                length(xy_disp_m))
            flock_number <- sample(
                unique(data_ped_train$flock_number),
                length(xy_disp_m),
                replace = TRUE)
            count <- rep(
                filter(ref_species, common_name == target_species)$count,
                length(xy_disp_m))
            drone <- rep(drone_type, length(xy_disp_m))
            z_disp_m <- rep(altitude, length(xy_disp_m))
            z_vel_ms <- rep(0, length(xy_disp_m))
            xb_vel_ms <- rep(velocity, length(xy_disp_m))
            yb_vel_ms <- rep(0, length(xy_disp_m))
            xyz_acc_mss <- rep(0, length(xy_disp_m))
            location <- sample(
                unique(data_ped_train$location),
                length(xy_disp_m),
                replace = TRUE)
            month_aest <- rep(ref$month_aest, length(xy_disp_m))
            hrs_since_low_tide <- rep(ref$hrs_since_low_tide, length(xy_disp_m))
            temperature_dc <- rep(ref$temperature_dc, length(xy_disp_m))
            wind_speed_ms <- rep(ref$wind_speed_ms, length(xy_disp_m))
            travel_rel_wind_dir_d <- rep(
                ref$travel_rel_wind_dir_d,
                length(xy_disp_m))
            cloud_cover_p <- rep(ref$cloud_cover_p, length(xy_disp_m))

            new_data <- data.frame(
                tend,
                common_name,
                count_eastern_curlew,
                flock_number,
                count,
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
                cloud_cover_p)

            prediction <- new_data %>%
                mutate(intlen = 1) %>%
                add_cumu_hazard(fit)
                # select(
                #     tend,
                #     cumu_hazard,
                #     cumu_upper,
                #     cumu_lower,
                #     common_name,
                #     z_disp_m,
                #     count_eastern_curlew,
                #     count,
                #     drone,
                #     xy_disp_m)

            df_i <- bind_rows(df_i, prediction)
        }
    }
    assign("cum_haz_data", df_i, envir = .GlobalEnv)
}

# altitudes <- seq(10, 120, n = 100)
altitudes <- seq_range(10:120, n = 20)
target_birds <- unique(data_ped_train$common_name)

# altitudes <- 10
# target_birds <- "bar_tailed_godwit"

log_simulator(fit, ref_species, ref, altitudes, target_birds, "mavic 2 pro")

# visualise
# ggplot(cum_haz_data, aes(x = xy_disp_m, y = z_disp_m, col = cumu_hazard, group = z_disp_m)) +
# facet_wrap("common_name") +
# geom_line() +
# theme(legend.position = "bottom")

fid_95 <- cum_haz_data %>%
    filter(cumu_hazard > 0.95) %>%
    group_by(common_name, z_disp_m) %>%
    slice(1)

ggplot(fid_20, aes(x = xy_disp_m, y = z_disp_m)) +
facet_wrap("common_name") +
geom_line() +
theme(legend.position = "bottom")