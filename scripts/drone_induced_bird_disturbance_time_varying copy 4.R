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
data_new <- read_csv(choose.files(), guess_max = 1000000)

# prepare train and test data in ped format
prepare_data <- function(df, istrain) {
    data_clean <- df %>%
        select(-notes) %>%
        # group drones into size
        mutate(drone = case_when(
            drone == "mavic 2 pro" | drone == "phantom 4 pro" ~ "350mm quad",
            drone == "mavic mini" ~ "200mm quad",
            drone == "inspire 2" ~ "600mm quad")) %>%
        drop_na(drone) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # degrade data to seconds for faster convergence
        filter(time %% 1 == 0) %>%
        # specify factors
        mutate(
            flock_number = as.factor(flock_number),
            drone = as.factor(drone),
            location = as.factor(location),
            common_name = as.factor(common_name))

    if (istrain) {
        data_clean <- data_clean %>%
            # approach ends if birds take flight
            group_by(test, flight, species, behaviour) %>%
            filter(behaviour == 0 | row_number() <= 1)
    }

    event_df <- data_clean %>%
        # add end time and status
        group_by(id) %>%
        mutate(
            end_time = max(time),
            status = max(behaviour)) %>%
        filter(row_number() == 1)

    tdc_df <- data_clean %>%
        group_by(id) %>%
        filter(row_number() != n())

    data_ped <- as_ped(
        data = list(event_df, tdc_df),
        formula = Surv(end_time, status) ~ . +
        concurrent(z_disp_m, xy_disp_m, behaviour, tz_var = "time"),
        id = "id")
    return(data_ped)
}

data_ped_test <- prepare_data(data_new, FALSE)
data_ped_train <- prepare_data(data_new, TRUE)

data_ped_test_species <- data_ped_test %>%
    filter(
        common_name == "pied_stilt" |
        common_name == "bar_tailed_godwit" |
        common_name == "eastern_curlew") %>%
    droplevels() %>%
    select(behaviour, id, test, flight, sentinal_susceptibility, ped_status, tstart, tend, interval, offset, common_name, drone, location, flock_number, cloud_cover_p, wind_speed_ms, xy_disp_m, z_disp_m, xyz_acc_mss, month_aest)

data_ped_train_species <- data_ped_train %>%
    # filter(
        # common_name == "pied_stilt" |
        # common_name == "bar_tailed_godwit" |
        # common_name == "eastern_curlew") %>%
    droplevels() %>%
    select(behaviour, id, test, flight, sentinal_susceptibility, ped_status, tstart, tend, interval, offset, common_name, drone, location, flock_number, cloud_cover_p, wind_speed_ms, xy_disp_m, z_disp_m, xyz_acc_mss, month_aest)

###################
#### Fit Model ####
###################

# fit model
fit <- gam(
    ped_status ~
    s(tend) +

    # target
    common_name +
    s(sentinal_susceptibility, k = 4) +
    s(flock_number, bs = "re", k = 4) +

    # drone
    drone +

    # approach
    te(xy_disp_m, z_disp_m, k = 4) +
    s(xyz_acc_mss, k = 4) +

    # environment
    s(location, bs = "re", k = 4),
    # s(wind_speed_ms) +
    # s(cloud_cover_p),
    data = data_ped_train_species,
    family = poisson(),
    method = "REML",
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
gam.check(fit, rep = 500)

# create dataframes varing each explanatory variable one at a time while
# holding others at medium or mode of numerical and character/factor variables

new_data <- function(var1, var2) {
    # get the original variable data so we can check its type later
    var_type1 <- typeof(eval(parse(text = paste0("data_ped_train_species$", var1))))
    print(var1)
    print(var_type1)
    new_dataframe <- data_ped_train_species %>%
        ungroup() %>%
        mutate(new_col1 = !!sym(var1)) %>%
        # create new data
        {if (!is.na(var2)) {
            mutate(., new_col2 = !!sym(var2)) %>%
            make_newdata(
                .,
                new_col1 = seq_range(new_col1, n = 100),
                new_col2 = seq_range(new_col2, n = 100)) %>%
            select(-!!sym(var1)) %>%
            rename({{ var1 }} := new_col1) %>%
            select(-!!sym(var2)) %>%
            rename({{ var2 }} := new_col2) %>%
            add_term(
                fit,
                term = paste0(var1, ",", var2),
                exclude = c("s(flock_number)", "s(location)"))}
        else {
            {if (var1 == "month_aest") {
                make_newdata(
                    .,
                    new_col1 = unique(new_col1),
                    common_name = unique(common_name)) %>%
                select(-!!sym(var1)) %>%
                rename({{ var1 }} := new_col1)}
            else if (var_type1 == "character" | var_type1 == "integer") {
                make_newdata(., new_col1 = unique(new_col1)) %>%
                select(-!!sym(var1)) %>%
                rename({{ var1 }} := new_col1)}
            else {
                make_newdata(., new_col1 = seq_range(new_col1, n = 100)) %>%
                select(-!!sym(var1)) %>%
                rename({{ var1 }} := new_col1)}} %>%
            add_term(
                fit,
                term = var1,
                exclude = c("s(flock_number)", "s(location)"))}}
    assign(
        paste0(
            var1, "_", var2, "_df"),
            new_dataframe, envir = .GlobalEnv)
}

predictors <- data.frame(
    var1 = c(
        "tend",
        "common_name",
        "sentinal_susceptibility",
        "flock_number",
        "drone",
        "xy_disp_m",
        "xyz_acc_mss",
        "location",
        "month_aest"),
    var2 = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        "z_disp_m",
        NA,
        NA,
        NA))

invisible(mapply(new_data, predictors$var1, predictors$var2))

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(var1, var2) {
    dataframe <- eval(parse(text = paste0(var1, "_", var2, "_df")))
    var_type1 <- typeof(eval(parse(text = paste0(var1, "_", var2, "_df$", var1))))
    fit <- eval(parse(text = paste0(var1, "_", var2, "_df$fit")))
    if (!is.na(var2)) {
        plot <- ggplot(data = filter(dataframe, !!sym(var1) < 500), aes(x = .data[[var2]], y = .data[[var1]], z = fit)) +
            geom_raster(aes(fill = fit)) +
            geom_contour(colour = "black") +
            scale_fill_gradientn(colours = c("green", "red"))}
    else if (var1 == "month_aest") {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
            coord_cartesian(ylim = c(-5, 5)) +
            geom_line() +
            facet_wrap("common_name") +
            geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)}
    else if (var_type1 == "integer" | var_type1 == "character") {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
            coord_cartesian(ylim = c(-5, 5)) +
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))}
    else {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
            coord_cartesian(ylim = c(-5, 5)) +
            geom_line() +
            geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)}
    assign(paste0(var1, "_", var2, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors$var1, predictors$var2)

show_plots <- function(var1, var2) {
    windows()
    par(ask = TRUE)
    for (i in 1:length(var1)) {
        plot(eval(parse(text = paste(var1[i], var2[i], "plot", sep = "_"))))
    }
    par(ask = FALSE)
}

show_plots(predictors$var1, predictors$var2)

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight_log <- data_ped_test_species %>%
    filter(test == 41 & flight == 1) %>%
    filter(common_name == "eastern_curlew") %>%
    mutate(intlen = tend - tstart) %>%
    add_surv_prob(fit, exclude = c("s(flock_number)", "s(location)"))

ggplot(flight_log, aes(x = tend, y = surv_prob, ymin = surv_upper, ymax = surv_lower)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
geom_line(aes(y = xy_disp_m / max(xy_disp_m)), colour = "red") +
geom_line(aes(y = z_disp_m / max(z_disp_m)), colour = "green") +
geom_line(aes(y = xyz_acc_mss / max(xyz_acc_mss)), colour = "blue") +
geom_line(aes(y = behaviour), colour = "purple") +
coord_cartesian(ylim = c(0, 1))

##################################################
#### Flight Initiation Distance Visualisation ####
##################################################

# use middle value for all variables, except species, altitude, distance, drone

log_simulator <- function(fit, altitude_list, target_species_list, drone_type) {
    df_i <- data.frame()
    dt <- 0.1
    for (i in 1:length(target_birds)) {
        target_species <- target_species_list[i]
        for (a in 1:length(altitude_list)) {
            altitude <- altitude_list[a]
            xy_disp_m <- c(300)
            xy_disp_m_i <- xy_disp_m[1]
            velocity <- 5
            tend <- c(0)
            tend_i <- tend[1]

            while (xy_disp_m_i > 0) {
                xy_disp_m_i <- xy_disp_m_i - velocity * dt
                tend_i <- tend_i + dt
                xy_disp_m <- append(xy_disp_m, xy_disp_m_i)
                tend <- append(tend, tend_i)
            }

            common_name <- rep(target_species, length(xy_disp_m))
            sentinal_susceptibility <- rep(0, length(xy_disp_m))
            flock_number <- sample(
                unique(data_ped_train_species$flock_number),
                length(xy_disp_m),
                replace = TRUE)
            drone <- rep(drone_type, length(xy_disp_m))
            z_disp_m <- rep(altitude, length(xy_disp_m))
            xyz_acc_mss <- rep(0, length(xy_disp_m))
            month_aest <- rep(5, length(xy_disp_m))
            location <- sample(
                unique(data_ped_train_species$location),
                length(xy_disp_m),
                replace = TRUE)

            new_data <- data.frame(
                tend,
                common_name,
                sentinal_susceptibility,
                flock_number,
                drone,
                z_disp_m,
                xy_disp_m,
                xyz_acc_mss,
                month_aest,
                location)

            prediction <- new_data %>%
                mutate(intlen = 1) %>%
                add_surv_prob(fit, exclude = c("s(flock_number)", "s(location)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(10:120, n = 20)
target_birds <- unique(data_ped_train_species$common_name)

survival_data <- log_simulator(fit, altitudes, target_birds, "350mm quad")

ggplot(data = survival_data, aes(x = xy_disp_m, y = z_disp_m, z = surv_prob)) +
facet_wrap("common_name") +
geom_contour_filled()

check <- common_name_NA_df %>%
    select(fit, common_name)
