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

packages <- c("ggplot2", "stringr", "readr", "dplyr", "mgcv", "pammtools")
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
data_new2 <- read_csv(choose.files(), guess_max = 1000000)

wor <- data_new2 %>%
    filter(test == 16, flight == 4, common_name == "pied_stilt") %>%
    select(test, flight, common_name, sentinel_flight1, time, behaviour, xy_disp_m, z_disp_m, xb_vel_ms, xyz_acc_mss, drone_obscured)

# prepare train and test data in ped format
prepare_data1 <- function(df) {
    data_clean <- df %>%
        # filter(
        #     common_name == "eastern_curlew" |
        #     common_name == "pied_stilt"
        # ) %>%
        # add approach random effect
        group_by(test, flight) %>%
        mutate(approach = cur_group_id()) %>%
        # add presence
        mutate(
            across(
                contains("count_"),
                ~ case_when(. > 0 ~ TRUE, TRUE ~ FALSE),
                .names = "presence_{col}")) %>%
        rename_with(~str_remove(., "_count")) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # degrade time to s for faster convergence
        # filter(time %% 1 == 0) %>%
        # approach ends if birds take flight
        group_by(test, flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        # degrade data to fit faster, but keep first sentinel flight and flight
        group_by(test, flight, behaviour, sentinel_flight1) %>%
        mutate(time = time * 10) %>%
        mutate(keep = case_when(
            sentinel_flight1 == 1 & behaviour == 0 & row_number() <= 1 ~ 1,
            behaviour == 1 ~ 1,
            time %% 10 == 0 ~ 1,
            TRUE ~ 0)) %>%
        group_by(time) %>%
        mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            approach = as.factor(approach),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight1 = as.factor(sentinel_flight1)
        )

    data_ped <- data_clean %>%
        group_by(test, flight, common_name) %>%
        mutate(
            ped_status = lead(behaviour),
            tstart = time,
            tend = lead(time),
            interval = tend - tstart,
            offset = log(interval)) %>%
        drop_na(ped_status) %>%
        droplevels()
    return(data_ped)
}

prepare_data2 <- function(df) {
    data_clean <- df %>%
        filter(
            common_name == "eastern_curlew" |
            common_name == "pied_stilt"
        ) %>%
        # add approach random effect
        group_by(test, flight) %>%
        mutate(approach = cur_group_id()) %>%
        # add presence
        mutate(
            across(
                contains("count_"),
                ~ case_when(. > 0 ~ "true", TRUE ~ "false"),
                .names = "presence_{col}")) %>%
        rename_with(~str_remove(., "_count")) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # degrade data to seconds for faster convergence
        filter(time %% 1 == 0) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            approach = as.factor(approach)) %>%
        # approach ends if birds take flight
        group_by(test, flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        ungroup()

    data_ped <- data_clean %>%
        mutate(
            ped_status = behaviour,
            tstart = time,
            tend = time + 1,
            interval = tend - tstart,
            offset = log(interval)) %>%
        droplevels()
    return(data_ped)
}

prepare_data3 <- function(df, istrain) {
    data_clean <- df %>%
        filter(
            common_name == "eastern_curlew" |
            common_name == "pied_stilt"
        ) %>%
        # add approach random effect
        group_by(test, flight) %>%
        mutate(approach = cur_group_id()) %>%
        # add presence
        mutate(
            across(
                contains("count_"),
                ~ case_when(. > 0 ~ "true", TRUE ~ "false"),
                .names = "presence_{col}")) %>%
        rename_with(~str_remove(., "_count")) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # degrade data to seconds for faster convergence
        filter(time %% 1 == 0) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            approach = as.factor(approach)) %>%
        # approach ends if birds take flight
        group_by(test, flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        ungroup()

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
        concurrent(
            z_disp_m,
            xy_disp_m,
            xyz_acc_mss,
            xb_vel_ms,
            yb_vel_ms,
            z_vel_ms,
            drone_obscured,
            sentinel_flight,
            behaviour,
            time,
            tz_var = "time"),
        id = "id") %>%
        droplevels()
    return(data_ped)
}

data_ped1 <- prepare_data1(data_new2)
data_ped2 <- prepare_data2(data_new)
data_ped3 <- prepare_data3(data_new)

check1 <- data_ped1 %>%
    select(test, flight, common_name, sentinel_flight1, offset, tstart, tend, time, behaviour, ped_status, xy_disp_m, z_disp_m, xb_vel_ms, xyz_acc_mss, drone_obscured)

check2 <- data_ped2 %>%
    select(test, flight, common_name, sentinel_flight, tstart, tend, time, behaviour, ped_status, xy_disp_m, z_disp_m, xb_vel_ms, xyz_acc_mss, drone_obscured)

check3 <- data_ped3 %>%
    select(test, flight, common_name, sentinel_flight, tstart, tend, time, behaviour, ped_status, xy_disp_m, z_disp_m, xb_vel_ms, xyz_acc_mss, drone_obscured)


check <- data_ped %>%
    # filter(z_disp_m > 80) %>%
    filter(common_name == "eastern_curlew") %>%
    group_by(test, flight, common_name) %>%
    filter(max(as.logical(sentinel_flight)) == 1) %>%
    filter(sentinel_flight == TRUE) %>%
    mutate(max_alt = max(z_disp_m)) %>%
    mutate(min_disp = min(xy_disp_m)) %>%
    mutate(ped_max = max(ped_status)) %>%
    filter(ped_max == 0) %>%
    # slice(1) %>%
    mutate(time = time / 10) %>%
    group_by(test, flight) %>%
    select(test, flight, max_alt, min_disp, sentinel_flight, ped_max, ped_status)
###################
#### Fit Model ####
###################

# fit model
system.time({
    fit <- gam(
        ped_status ~
        # drone
        drone +
        te(xy_disp_m, z_disp_m, k = 3, by = common_name) +
        s(xb_vel_ms, k = 4) +
        # s(z_vel_ms, k = 4) +
        # s(yb_vel_ms, k = 4) +
        s(xyz_acc_mss, k = 4) +
        # environment
        s(tend, k = 4) +
        drone_obscured +
        # s(wind_speed_ms, k = 4) +
        # s(cloud_cover_p, k = 4) +
        # s(temperature_dc, k = 4) +
        # s(hrs_from_high, k = 4) +
        # location +
        # target
        s(approach, bs = "re") +
        sentinel_flight1 +
        common_name,
        data = data_ped1,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

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

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight_log <- data_ped %>%
    filter(test == 153 & flight == 1) %>%
    # filter(tend <= 140) %>%
    # filter(tend <= 46) %>%
    filter(common_name == "pied_stilt") %>%
    mutate(intlen = tend - tstart) %>%
    mutate(sentinel_flight = case_when(tend > 500 ~ TRUE, TRUE ~ FALSE)) %>%
    add_surv_prob(fit, exclude = c("s(approach)"))

ggplot(flight_log, aes(x = (tend / 10), y = surv_prob, ymin = surv_upper, ymax = surv_lower)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
geom_line(aes(y = xy_disp_m / max(xy_disp_m)), colour = "red") +
geom_line(aes(y = z_disp_m / max(z_disp_m)), colour = "green") +
geom_line(aes(y = xyz_acc_mss / max(xyz_acc_mss)), colour = "blue") +
geom_line(aes(y = behaviour), colour = "purple") +
coord_cartesian(ylim = c(0, 1))

# create dataframes varing each explanatory variable one at a time while
# holding others at medium or mode of numerical and character/factor variables

new_data <- function(var1, var2) {
    # get the original variable data so we can check its type later
    print(var1)
    new_dataframe <- data_ped %>%
        ungroup() %>%
        {if (var1 == "tend") {
            make_newdata(., tend = seq_range(tend, n = 100)) %>%
            add_term(fit, term = "tend")}

        else if (var1 == "common_name") {
            make_newdata(., common_name = unique(common_name)) %>%
            add_term(fit, term = "common_name") %>%
            mutate(
                common_name = str_replace(common_name, "_", " "),
                common_name = str_replace(common_name, "_", " "))}

        else if (var1 == "count") {
            make_newdata(., count = seq_range(count, n = 100)) %>%
            add_term(fit, term = "count")}

        else if (var1 == "total_count") {
            make_newdata(., total_count = seq_range(total_count, n = 100)) %>%
            add_term(fit, term = "total_count")}

        else if (var1 == "drone") {
            make_newdata(., drone = unique(drone)) %>%
            add_term(fit, term = "drone") %>%
            mutate(drone = as.character(drone))}

        else if (var1 == "presence_eastern_curlew") {
            make_newdata(
                .,
                presence_eastern_curlew = unique(presence_eastern_curlew)) %>%
            add_term(fit, term = "presence_eastern_curlew")}

        else if (var1 == "month_aest") {
            make_newdata(
                .,
                month_aest = unique(month_aest)) %>%
            add_term(fit, term = "month_aest")}

        # else if (var1 == "xy_disp_m") {
        #     make_newdata(
        #         .,
        #         xy_disp_m = seq_range(xy_disp_m, n = 100),
        #         z_disp_m = seq_range(z_disp_m, n = 100)) %>%
        #     add_term(fit, term = "xy_disp_m,z_disp_m")}
        else if (var1 == "xy_disp_m") {
            make_newdata(., xy_disp_m = seq_range(xy_disp_m, n = 100)) %>%
            add_term(fit, term = "xy_disp_m")}

        else if (var1 == "z_disp_m") {
            make_newdata(., z_disp_m = seq_range(z_disp_m, n = 100)) %>%
            add_term(fit, term = "z_disp_m")}

        else if (var1 == "xyz_acc_mss") {
            make_newdata(., xyz_acc_mss = seq_range(xyz_acc_mss, n = 100)) %>%
            add_term(fit, term = "xyz_acc_mss")}

        else if (var1 == "xb_vel_ms") {
            make_newdata(., xb_vel_ms = seq_range(xb_vel_ms, n = 100)) %>%
            add_term(fit, term = "xb_vel_ms")}

        else if (var1 == "z_vel_ms") {
            make_newdata(., z_vel_ms = seq_range(z_vel_ms, n = 100)) %>%
            add_term(fit, term = "z_vel_ms")}

        else if (var1 == "location") {
            make_newdata(., location = unique(location)) %>%
            add_term(fit, term = "location") %>%
            mutate(location = as.character(location))}

        else if (var1 == "drone_obscured") {
            make_newdata(., drone_obscured = unique(drone_obscured)) %>%
            add_term(fit, term = "drone_obscured")}

        else if (var1 == "wind_speed_ms") {
            make_newdata(
                .,
                wind_speed_ms = seq_range(wind_speed_ms, n = 100)) %>%
            add_term(fit, term = "wind_speed_ms")}

        else if (var1 == "cloud_cover_p") {
            make_newdata(
                .,
                cloud_cover_p = seq_range(cloud_cover_p, n = 100)) %>%
            add_term(fit, term = "cloud_cover_p")}

        else if (var1 == "temperature_dc") {
            make_newdata(
                .,
                temperature_dc = seq_range(temperature_dc, n = 100)) %>%
            add_term(fit, term = "temperature_dc")}

        else if (var1 == "flock_approach") {
            make_newdata(
                .,
                sentinel_flight = unique(sentinel_flight),
                flock_approach = seq_range(flock_approach, n = 100)) %>%
            add_term(fit, term = c("flock_approach", "sentinel_flight"))}

        else if (var1 == "hrs_from_high") {
            make_newdata(
                .,
                hrs_from_high = seq_range(hrs_from_high, n = 100)) %>%
            add_term(fit, term = "hrs_from_high")}}

    assign(
        paste0("df_", var1, "_", var2),
        new_dataframe,
        envir = .GlobalEnv)
}

predictors <- data.frame(
    var1 = c(
        "xy_disp_m",
        "common_name",
        "tend",
        "presence_eastern_curlew",
        "count",
        "count_total",
        "drone",
        "xyz_acc_mss",
        "xb_vel_ms",
        "z_vel_ms",
        "wind_speed_ms",
        "cloud_cover_p",
        "drone_obscured",
        "temperature_dc",
        "month_aest",
        "hrs_from_high",
        "location"),
    var2 = c(
        "z_disp_m",
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
        "na",
        "na",
        "na"))

invisible(mapply(new_data, predictors$var1, predictors$var2))

new_data("xy_disp_m", "na")
new_data("z_disp_m", "na")
new_data("flock_approach", "na")

check <- df_flock_approach_na %>%
    select(flock_approach, sentinel_flight, fit)

###########################
#### Fit Visualisation ####
###########################

plot_fit <- function(var1, var2) {
    dataframe <- eval(parse(text = paste0("df_", var1, "_", var2)))
    var1_type <- typeof(eval(parse(text = paste0("df_", var1, "_", var2, "$", var1))))
    height <- 10
    width <- 10
    title <- paste0("plot_", var1, "_", var2, ".png")
    # if (var1 == "xy_disp_m") {
    #     plot <- ggplot(
    #             data = dataframe,
    #             aes(x = .data[[var1]], y = .data[[var2]], z = fit)) +
    #         geom_raster(aes(fill = fit)) +
    #         geom_contour(colour = "black") +
    #         scale_fill_gradientn(colours = c("green", "red")) +
    #         xlab("Horizontal Distance [m]") +
    #         ylab("Altitude [m]")}
    if(var1_type == "double") {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
        geom_line() +
        coord_cartesian(ylim = c(-4, 4)) +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2)}
    else if(var1_type == "character" | var1_type == "integer") {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
            coord_cartesian(ylim = c(-4, 4))}
    if (var1 == "xyz_acc_mss") plot <- plot + xlab("Acceleration [m/s/s]")
    if (var1 == "xb_vel_ms") plot <- plot + xlab("Approach Velocity [m/s]")
    if (var1 == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (var1 == "cloud_cover_p") plot <- plot + xlab("Cloud Cover [%]")
    if (var1 == "wind_speed_ms") plot <- plot + xlab("Wind Speed [ms]")
    if (var1 == "temperature_dc") plot <- plot + xlab("Temperature [C]")
    if (var1 == "drone_obscured") plot <- plot + xlab("Drone Obscured")
    if (var1 == "presence_eastern_curlew") plot <- plot + xlab("Eastern Curlew Presence")
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
    if (var1_type == "character") {
        height <- 15
        width <- 12.5
        plot <- plot +
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 0.95))}
    if (var1 == "flock_number") {
        plot <- plot + theme(axis.text.x = element_blank())}

    ggsave(title, plot, height = height, width = width)
}

plot_fit("xy_disp_m", "na")
mapply(plot_fit, predictors$var1, predictors$var2)

##################################################
#### Flight Initiation Distance Visualisation ####
##################################################
prepare_test <- function(df) {
    data_clean <- df %>%
        filter(test == 157, flight == 1, common_name == "whimbrel") %>%
        # 1 time interval is 0.1s
        mutate(time = time * 10) %>%
        # filter(time %% 1 == 0) %>%
        # mutate(time = time / 50) %>%
        filter(time < 1470) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight))

    data_ped <- data_clean %>%
        group_by(test, flight, common_name) %>%
        mutate(
            ped_status = lead(behaviour),
            tstart = time,
            tend = lead(time),
            interval = tend - tstart,
            offset = log(interval)) %>%
        drop_na(ped_status) %>%
        ungroup() %>%
        droplevels()
    return(data_ped)
}

ref <- data_ped1 %>%
    ungroup() %>%
    sample_info()

test_flight <- prepare_test(data_new)

log_simulator <- function(fit, altitude_list, species_list, drone_name) {
    df_i <- data.frame()
    for (x in 1:length(species_list)) {
        species <- species_list[x]

        ref_species <- data_ped1 %>%
            ungroup() %>%
            filter(common_name == as.character(species)) %>%
            sample_info()

        for (y in 1:length(altitude_list)) {
            altitude <- altitude_list[y]

            flight_ascent <- test_flight %>%
                filter(z_disp_m < altitude - 4)

            flight_approach <- test_flight %>%
                slice(round(470):n()) %>%
                mutate(z_disp_m = z_disp_m - (120 - altitude))

            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    xy_disp_m,
                    z_disp_m,
                    xb_vel_ms,
                    z_vel_ms,
                    xyz_acc_mss) %>%
                mutate(
                    common_name = species,
                    tend = row_number(),
                    # sentinel_flight = FALSE,
                    # sentinel_flight = 0,
                    sentinel_flight1 = FALSE,
                    sentinel_presence = FALSE,
                    flock_approach = 15,
                    presence_eastern_curlew = FALSE,
                    count = ref_species$count,
                    count_total = ref_species$count,
                    approach = ref_species$approach,
                    drone = drone_name,
                    drone_obscured = FALSE,
                    wind_speed_ms = ref$wind_speed_ms,
                    cloud_cover_p = ref$cloud_cover_p,
                    temperature_dc = ref$temperature_dc,
                    hrs_from_high = ref$hrs_from_high,
                    month_aest = ref$month_aest,
                    location = ref$location,
                    altitude = altitude)

            prediction <- flight_log_new %>%
                mutate(intlen = 1) %>%
                add_surv_prob(
                    fit,
                    exclude = c("s(approach)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(10:120, by = 5)
# target_birds <- c("pied_oys""pied_stilt", "eastern_curlew")

# altitudes <- seq_range(10:120, by = 10)
target_birds <- unique(data_ped1$common_name)

survival_data <- log_simulator(fit, altitudes, target_birds, "mavic 2 pro")

advancing <- survival_data %>%
    mutate(z_disp_m = round(z_disp_m)) %>%
    filter(z_disp_m == altitude) %>%
    mutate(
        common_name = str_replace(common_name, "_", " "),
        common_name = str_replace(common_name, "_", " "))

plot <- ggplot(
    data = advancing,
    aes(x = xy_disp_m, y = z_disp_m, z = surv_prob)) +
    geom_contour_filled(binwidth = 0.1) +
    # geom_contour(colour = "black", binwidth = 0.1, size = 2) +
    scale_fill_brewer(
        type = "div",
        palette = 8,
        direction = 1,
        aesthetics = "fill") +
    facet_wrap("common_name", ncol = 3, nrow = 4) +
    theme_bw() +
    scale_x_continuous(limits = c(0, 200), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Horizontal Distance [m]") +
    ylab("Altitude [m]") +
    labs(fill = "Flight Probability") +
    theme(
        panel.spacing = unit(5, "lines"),
        strip.text = element_text(size = 40, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "in"),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.15, "in"),
        axis.text = element_text(size = 60),
        axis.title = element_text(size = 60, face = "bold"),
        legend.position = c(.67, .15),
        legend.key.size = unit(0.25, "in"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.box.background = element_rect(color = "black", size = 1),
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40, face = "bold")) +
    guides(fill = guide_legend(nrow = 2))

ggsave("flight_initiation_distance.png", plot, height = 25, width = 25)

############################
#### General Statistics ####
############################

total_appraoches <- data_ped_train %>%
    group_by(test, flight) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(total_appraoches)

approaches_per_species <- data_ped_train %>%
    group_by(id) %>%
    slice(1) %>%
    group_by(common_name) %>%
    summarise(count = n())

View(approaches_per_species)

approaches_per_site <-  data_ped_train %>%
    group_by(test, flight) %>%
    slice(1) %>%
    group_by(location) %>%
    summarise(count = n())

View(approaches_per_site)

approaches_per_drone <- data_ped_train %>%
    group_by(flight, test) %>%
    slice(1) %>%
    group_by(drone) %>%
    summarise(count = n())

View(approaches_per_drone)


#### quick Plot

data_plot <- data_new %>%
    filter(drone != "spibis") %>%
    filter(is.na(notes)) %>%
    select(-notes) %>%
    filter(approach_type == "advancing") %>%
    filter(common_name == "eastern_curlew") %>%
    filter(behaviour == 1) %>%
    group_by(flight, test) %>%
    slice(1)

plot <- ggplot(data_plot, aes(x = xy_disp_m, y = z_disp_m, colour = drone)) +
    theme_bw() +
    ylab("Altitude") +
    xlab("Distance") +
    theme(
        legend.position = c(.80, .30),
        strip.text = element_text(size = 40, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "in"),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.15, "in"),
        axis.text = element_text(size = 60),
        axis.title = element_text(size = 60, face = "bold"),
        legend.key.size = unit(0.25, "in"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.box.background = element_rect(color = "black", size = 1),
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40, face = "bold")) +
    scale_x_continuous(limits = c(0, 400), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 130), expand = c(0, 0)) +
    coord_fixed(ratio = 1) +
    geom_point(size = 8)

ggsave("eastern_curlew_FID.png", plot, height = 25, width = 25)

#### Quick Plot

#### Actual Data ####
plot_data <- data_ped %>%
    filter(common_name == "pied_stilt")

check <- data_ped %>%
    group_by(test, flight, common_name) %>%
    mutate(ped_status = max(ped_status)) %>%
    select(test, flight, common_name, ped_status) %>%
    slice(1)

ggplot(plot_data, aes(xy_disp_m, z_disp_m, colour = ped_status)) +
geom_point()

flights <- data_ped %>%
    group_by(flight, test, common_name) %>%
    filter(ped_status == 1) %>%
    slice(1) %>%
    select(test, flight, common_name, ped_status, sentinel_flight)

check <- data_ped %>%
    filter(test == 16, flight == 1, common_name == "bar_tailed_godwit")
