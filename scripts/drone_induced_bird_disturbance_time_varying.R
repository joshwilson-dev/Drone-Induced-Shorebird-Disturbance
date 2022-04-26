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

# inspect data
plot <- data_new %>%
    # filter(common_name ==) %>%
    filter(test == 157) %>%
    filter(flight == 3)

ggplot(plot, aes(x = time)) +
geom_line(aes(y = xy_disp_m / 100), col = "green") +
geom_line(aes(y = z_disp_m / 100), col = "blue") +
geom_line(aes(y = xyz_acc_mss), col = "red") +
geom_line(aes(y = behaviour), col = "orange")

# prepare train and test data in ped format
prepare_data <- function(df, istrain) {
    data_clean <- df %>%
        select(-notes) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 20) %>%
        # degrade data to seconds for faster convergence
        filter(time %% 1 == 0) %>%
        # creat presence instead of counts
        mutate(across(
            contains("count_"),
            ~ case_when(. > 0 ~ "true", TRUE ~ "false"),
            .names = "presence_{col}")) %>%
        rename_with(~str_remove(., "_count")) %>%
        mutate(common_name = as.factor(common_name))

    if (istrain) {
        data_clean <- data_clean %>%
            # test ends if birds take flight
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
        concurrent(z_disp_m, xy_disp_m, transition, behaviour, tz_var = "time"),
        id = "id")
    return(data_ped)
}

data_ped_train <- prepare_data(data_new, TRUE)
data_ped_test <- prepare_data(data_new, FALSE)

data_ped_train_pied_stilt <- data_ped_train %>%
    filter(common_name == "pied_stilt") %>%
    filter(presence_eastern_curlew == "false")

###################
#### Fit Model ####
###################

# fit model
fit <- gam(
    ped_status ~
    s(tend) +

    # target
    # common_name +
    # presence_eastern_curlew +
    # presence_pied_stilt +
    # s(count_eastern_curlew, k = 10) +
    # s(count_pied_stilt, k = 10) +
    s(flock_number, bs = "re") +

    # drone
    drone +

    # approach
    te(z_disp_m, xy_disp_m),
    # s(xyz_acc_mss) +
    # s(z_vel_ms) +
    # s(xb_vel_ms) +
    # s(yb_vel_ms) +

    # environment
    # s(month_aest, bs = "cc", k = 8) +
    # s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    # s(wind_speed_ms) +
    # s(travel_rel_wind_dir_d, bs = "cc") +
    # s(cloud_cover_p),
    data = data_ped_train_pied_stilt,
    family = binomial(),
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

# spec_counts <- data_ped_train %>%
#     group_by(common_name) %>%
#     sample_info() %>%
#     select(contains("count"), common_name) %>%
#     arrange(common_name)

spec_presence <- data_ped_train %>%
    group_by(common_name) %>%
    sample_info() %>%
    mutate(pivot_name = common_name) %>%
    select(common_name, pivot_name) %>%
    add_column(presence = "true") %>%
    pivot_wider(
        names_from = pivot_name,
        names_prefix = "presence_",
        values_from = presence) %>%
    replace(is.na(.), "false")

ref <- data_ped_train %>%
    ungroup() %>%
    mutate_if(is.numeric, min) %>%
    sample_info()

new_data <- function(var1, var2) {
    # get the original variable data so we can check its type later
    var_type1 <- typeof(eval(parse(text = paste0("data_ped_train$", var1))))
    print(var_type1)
    print(var1)
    new_dataframe <- data_ped_train %>%
        ungroup() %>%
        mutate(new_col1 = !!sym(var1)) %>%
        # create new data
        {if (!is.na(var2)) {
            mutate(., new_col2 = !!sym(var2)) %>%
            make_newdata(
                .,
                new_col1 = seq_range(new_col1, n = 100),
                new_col2 = seq_range(new_col2, n = 100)) %>%
                # common_name = unique(common_name)) %>%
            select(-!!sym(var1)) %>%
            rename({{ var1 }} := new_col1) %>%
            select(-!!sym(var2)) %>%
            rename({{ var2 }} := new_col2) %>%
            select(-contains("presence")) %>%
            merge(spec_presence) %>%
            # mutate(transition = as.logical(transition)) %>%
            # select(-contains("count")) %>%
            # merge(spec_counts) %>%
            # mutate_at(
            #     vars(starts_with("count_")),
            #     ~ case_when(
            #         . != count ~ 0,
            #         TRUE ~ .)) %>%
            add_term(
                fit,
                term = paste0(var1, ",", var2),
                exclude = c("s(flock_number)"))}
        else {
            {if (var1 == "common_name") {
                make_newdata(., new_col1 = unique(new_col1)) %>%
                select(-!!sym(var1)) %>%
                rename({{ var1 }} := new_col1) %>%
                select(-contains("presence")) %>%
                merge(spec_presence)}
                # select(-contains("count")) %>%
                # merge(spec_counts) %>%
                # mutate_at(
                #     vars(starts_with("count_")),
                #     ~ case_when(
                #         . != count ~ 0,
                #         TRUE ~ .))}
            # else if (var1 == "xy_disp_m" | var1 == "z_disp_m") {
            #     make_newdata(
            #         .,
            #         new_col1 = seq_range(!!sym(var1), n = 100),
            #         common_name = unique(common_name)) %>%
            #     select(-!!sym(var1)) %>%
            #     rename({{ var1 }} := new_col1) %>%
            #     select(-contains("count")) %>%
            #     merge(spec_counts) %>%
            #     mutate_at(
            #         vars(starts_with("count_")),
            #         ~ case_when(
            #             . != count ~ 0,
            #             TRUE ~ .))}
            else if (var_type1 == "character") {
                make_newdata(., new_col1 = unique(new_col1)) %>%
                select(-!!sym(var1)) %>%
                rename({{ var1 }} := new_col1)}
            else {
                make_newdata(., new_col1 = seq_range(new_col1, n = 100)) %>%
                select(-!!sym(var1)) %>%
                rename({{ var1 }} := new_col1)}} %>%
            mutate(transition = as.logical(transition)) %>%
            add_term(
                fit,
                term = var1,
                exclude = c("s(flock_number)"))}}
    assign(
        paste0(
            var1, "_", var2, "_df"),
            new_dataframe, envir = .GlobalEnv)
}
  
predictors <- data.frame(
    var1 = c(
        "tend",
        "common_name",
        "drone",
        # "count_eastern_curlew",
        # "count_pied_stilt",
        "presence_eastern_curlew",
        # "presence_pied_stilt",
        "flock_number",
        "z_disp_m",
        "xyz_acc_mss",
        # "z_vel_ms",
        # "xb_vel_ms",
        # "yb_vel_ms",
        "month_aest",
        # "hrs_since_low_tide",
        # "temperature_dc",
        "wind_speed_ms",
        # "travel_rel_wind_dir_d",
        "cloud_cover_p"),
    var2 = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        "xy_disp_m",
        NA,
        # NA,
        # NA,
        # NA,
        # NA,
        # NA,
        # NA,
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
    {if (!is.na(var2)) {
        plot <- ggplot(data = filter(dataframe, !!sym(var1) < 500), aes(x = .data[[var2]], y = .data[[var1]], z = fit)) +
        geom_raster(aes(fill = fit)) +
        geom_contour(colour = "black") +
        scale_fill_gradientn(colours = c("green", "red"))
    }
    else {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
        coord_cartesian(ylim = c(-5, 5)) +
        # {if (var1 == "xy_disp_m" | var1 == "z_disp_m") list(
        #     geom_line(),
        #     geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2),
        #     facet_grid(common_name))} +
        {if (var_type1 == "integer" | var_type1 == "character") list(
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)),
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)))} +
        {if (var_type1 == "double") list(
            geom_line(),
            geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2))}}}
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

flight_log <- data_ped_test %>%
    filter(test == 68 & flight == 4) %>%
    filter(common_name == "eastern_curlew") %>%
    mutate(intlen = tend - tstart) %>%
    add_cumu_hazard(fit, exclude = "s(flight_number)") %>%
    add_surv_prob(fit, exclude = "s(flight_number)")

ggplot(
    flight_log,
    aes(x = tend, y = cumu_hazard, ymin = cumu_lower, ymax = cumu_upper)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
coord_cartesian(ylim = c(0, 5))

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

ref_species <- data_ped_train %>%
    group_by(species) %>%
    sample_info() %>%
    mutate(presence_eastern_curlew = case_when(
        common_name == "eastern_curlew" ~ "true",
        TRUE ~ "false"))

ref <- data_ped_train %>%
    ungroup() %>%
    sample_info()

log_simulator <- function(fit, ref_species, ref, altitude_list,
target_species_list, drone_type) {
    df_i <- data.frame()
    dt <- 0.1
    for (i in 1:length(target_birds)) {
        target_species <- target_species_list[i]
        for (a in 1:length(altitude_list)) {
            print(altitude_list[a])
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

            # tend <- seq(1, length(xy_disp_m))
            common_name <- rep(target_species, length(xy_disp_m))
            presence_eastern_curlew <- rep(
                filter(ref_species, common_name == target_species)$presence_eastern_curlew,
                length(xy_disp_m))
            flock_number <- sample(
                unique(data_ped_train$flock_number),
                length(xy_disp_m),
                replace = TRUE)
            drone <- rep(drone_type, length(xy_disp_m))
            z_disp_m <- rep(altitude, length(xy_disp_m))
            xyz_acc_mss <- rep(0, length(xy_disp_m))
            # month_aest <- rep(ref$month_aest, length(xy_disp_m))
            month_aest <- rep(11, length(xy_disp_m))
            wind_speed_ms <- rep(ref$wind_speed_ms, length(xy_disp_m))
            cloud_cover_p <- rep(ref$cloud_cover_p, length(xy_disp_m))

            new_data <- data.frame(
                tend,
                common_name,
                presence_eastern_curlew,
                flock_number,
                drone,
                z_disp_m,
                xy_disp_m,
                xyz_acc_mss,
                month_aest,
                wind_speed_ms,
                cloud_cover_p)

            prediction <- new_data %>%
                mutate(intlen = 1) %>%
                add_surv_prob(fit, exclude = "s(flight_number)")
                # add_cumu_hazard(fit)

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(10:120, n = 20)
target_birds <- unique(data_ped_train$common_name)

survival_data <- log_simulator(fit, ref_species, ref, altitudes, target_birds, "mavic 2 pro")

fid <- survival_data %>%
    filter(surv_prob < 0.5) %>%
    # group_by(common_name, z_disp_m) %>%
    # group_by(common_name, z_disp_m) %>%
    slice(1)

ggplot(fid, aes(x = xy_disp_m, y = z_disp_m)) +
# facet_wrap("common_name") +
geom_line() +
theme(legend.position = "bottom")
