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


####
# prepare train and test data in ped format
prepare_data <- function(df, istrain) {
    data_clean <- df %>%
        # exclude low tide data
        filter(is.na(notes)) %>%
        select(-notes) %>%
        # filter out spibis
        filter(drone != "spibis") %>%
        # filter out single manly approach
        filter(location != "manly") %>%
        # calculate sentinal count
        mutate(sentinal_count = case_when(
            sentinal_susceptibility == FALSE ~ count)) %>%
        group_by(test, flight) %>%
        mutate(sentinal_count = mean(sentinal_count, na.rm = TRUE)) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # degrade data to seconds for faster convergence
        filter(time %% 1 == 0) %>%
        # specify factors
        mutate(
            flock_number = as.factor(flock_number),
            sentinal_susceptibility = as.factor(sentinal_susceptibility),
            drone = as.factor(drone),
            location = as.factor(location),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured))

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
        concurrent(
            z_disp_m,
            xy_disp_m,
            xyz_acc_mss,
            xb_vel_ms,
            yb_vel_ms,
            z_vel_ms,
            drone_obscured,
            behaviour,
            tz_var = "time"),
        id = "id") %>%
        droplevels()
    return(data_ped)
}

data_ped_train <- prepare_data(data_new, TRUE)
data_ped_test <- prepare_data(data_new, FALSE)

###################
#### Fit Model ####
###################

# fit model
fit <- gam(
    ped_status ~
    s(tend) +

    # target
    s(common_name, bs = "fs") +
    s(sentinal_susceptibility, bs = "fs") +
    s(sentinal_count) +
    # s(flock_number, bs = "re") +

    # drone
    s(drone, bs = "fs") +

    # approach
    te(xy_disp_m, z_disp_m) +
    s(xb_vel_ms) +
    s(xyz_acc_mss) +

    # environment
    # s(drone_obscured, bs = "fs") +
    s(wind_speed_ms) +
    s(cloud_cover_p) +
    s(location, bs = "fs"),
    data = data_ped_train,
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
    print(var1)
    new_dataframe <- data_ped_train %>%
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

        else if (var1 == "flock_number") {
            make_newdata(., flock_number = unique(flock_number)) %>%
            add_term(fit, term = "flock_number")}

        else if (var1 == "sentinal_susceptibility") {
            make_newdata(
                .,
                sentinal_susceptibility = unique(sentinal_susceptibility)) %>%
            add_term(fit, term = "sentinal_susceptibility")}

        else if (var1 == "count") {
            make_newdata(., count = seq_range(count, n = 100)) %>%
            add_term(fit, term = "count")}

        else if (var1 == "sentinal_count") {
            make_newdata(
                .,
                sentinal_count = seq_range(sentinal_count, n = 100)) %>%
            add_term(fit, term = "sentinal_count")}

        else if (var1 == "total_count") {
            make_newdata(., total_count = seq_range(total_count, n = 100)) %>%
            add_term(fit, term = "total_count")}

        else if (var1 == "drone") {
            make_newdata(., drone = unique(drone)) %>%
            add_term(fit, term = "drone") %>%
            mutate(drone = as.character(drone))}

        else if (var1 == "xy_disp_m") {
            make_newdata(
                .,
                xy_disp_m = seq_range(xy_disp_m, n = 100),
                z_disp_m = seq_range(z_disp_m, n = 100)) %>%
            add_term(fit, term = "xy_disp_m,z_disp_m")}

        else if (var1 == "xyz_acc_mss") {
            make_newdata(., xyz_acc_mss = seq_range(xyz_acc_mss, n = 100)) %>%
            add_term(fit, term = "xyz_acc_mss")}

        else if (var1 == "xb_vel_ms") {
            make_newdata(., xb_vel_ms = seq_range(xb_vel_ms, n = 100)) %>%
            add_term(fit, term = "xb_vel_ms")}

        else if (var1 == "yb_vel_ms") {
            make_newdata(., yb_vel_ms = seq_range(yb_vel_ms, n = 100)) %>%
            add_term(fit, term = "yb_vel_ms")}

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

        else if (var1 == "hrs_since_low_tide") {
            make_newdata(
                .,
                hrs_since_low_tide = seq_range(hrs_since_low_tide, n = 100)) %>%
            add_term(fit, term = "hrs_since_low_tide")}}

    assign(
        paste0("df_", var1, "_", var2),
        new_dataframe,
        envir = .GlobalEnv)
}

predictors <- data.frame(
    var1 = c(
        "xy_disp_m",
        "tend",
        "common_name",
        "sentinal_susceptibility",
        "sentinal_count",
        # "flock_number",
        "drone",
        "xyz_acc_mss",
        "xb_vel_ms",
        # "yb_vel_ms",
        # "z_vel_ms",
        "wind_speed_ms",
        "cloud_cover_p",
        # "drone_obscured",
        # "temperature_dc",
        # "hrs_since_low_tide",
        "location"),
    var2 = c(
        "z_disp_m",
        NA,
        # NA,
        # NA,
        # NA,
        NA,
        # NA,
        NA,
        NA,
        # NA,
        # NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA))

invisible(mapply(new_data, predictors$var1, predictors$var2))

###########################
#### Fit Visualisation ####
###########################

plot_fit <- function(var1, var2) {
    dataframe <- eval(parse(text = paste0("df_", var1, "_", var2)))
    var1_type <- typeof(eval(parse(text = paste0("df_", var1, "_", var2, "$", var1))))
    height <- 10
    width <- 10
    title <- paste0("plot_", var1, "_", var2, ".png")
    if (var1 == "xy_disp_m") {
        dataframe <- dataframe %>%
            filter(!!sym(var1) >= 0, !!sym(var1) <= 500, !!sym(var2) >= -1)
        plot <- ggplot(
                data = dataframe,
                aes(x = .data[[var1]], y = .data[[var2]], z = fit)) +
            geom_raster(aes(fill = fit)) +
            geom_contour(colour = "black") +
            scale_fill_gradientn(colours = c("green", "red")) +
            xlab("Horizontal Distance [m]") +
            ylab("Altitude [m]")}
    else if(var1_type == "double") {
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
    if (var1 == "drone_obscured") plot <- plot + xlab("Drone Obscured")
    if (var1 == "flock_number") plot <- plot + xlab("Flock")
    if (var1 == "sentinal_susceptibility") plot <- plot + xlab("Sentinal Presence")
    if (var1 == "sentinal_count") plot <- plot + xlab("Sentinal Abundance")
    plot <- plot +
        theme_bw() +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "in"),
            axis.ticks = element_line(size = 2),
            axis.ticks.length = unit(.15, "in"),
            axis.text = element_text(size = 40),
            axis.title = element_text(size = 40, face = "bold"),
            # legend.position = c(.85, .72),
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
plot_fit("xy_disp_m", "z_disp_m")
mapply(plot_fit, predictors$var1, predictors$var2)

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight_log <- data_ped_test %>%
    filter(test == 120 & flight == 1) %>%
    mutate(common_name = "eastern_curlew") %>%
    mutate(intlen = tend - tstart) %>%
    # mutate(sentinal_susceptibility = FALSE) %>%
    add_surv_prob(fit)

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

#### NEW LOG SIMULATOR ####
flight_log <- data_ped_test %>%
    filter(test == 157 & flight == 1) %>%
    filter(common_name == "whimbrel") %>%
    mutate(intlen = tend - tstart)

flight_approach <- flight_log %>%
    filter(z_disp_m > 1)


#### NEW LOG SIMULATOR ####
# use middle value for all variables, except species, altitude, distance, drone

ref <- data_ped_train %>%
    sample_info()

ref_spec <- data_ped_train %>%
    group_by(common_name) %>%
    sample_info()

log_simulator <- function(fit, altitude_list, target_species_list, drone_type) {
    df_i <- data.frame()
    dt <- 1
    for (i in 1:length(target_birds)) {
        target_species <- target_species_list[i]
        for (a in 1:length(altitude_list)) {
            z_disp_m <- c(0)
            z_disp_m_i <- z_disp_m[1]
            xy_disp_m <- c(300)
            xy_disp_m_i <- xy_disp_m[1]
            max_xyz_acc_mss <- 2
            max_z_vel_ms <- 2
            max_xb_vel_ms <- 6
            xyz_acc_mss <- c(2)
            xyz_acc_mss_i <- xyz_acc_mss[1]
            z_vel_ms <- c(0)
            z_vel_ms_i <- z_vel_ms[1]
            xb_vel_ms <- c(0)
            xb_vel_ms_i <- xb_vel_ms[1]
            tend <- c(0)
            tend_i <- tend[1]

            while (xy_disp_m_i > 0) {
                z_stop <- z_vel_ms_i**2 / 2 * abs(max_xyz_acc_mss)
                xy_stop <- xb_vel_ms_i**2 / 2 * abs(max_xyz_acc_mss)
                if (z_disp_m_i < altitude_list[a]) {
                    if ((altitude_list[a] - z_disp_m_i) > z_stop) {
                        if (z_vel_ms_i < max_z_vel_ms) {
                            xyz_acc_mss_i <- max_xyz_acc_mss}
                        else {
                            xyz_acc_mss_i <- 0}}
                    else {
                        xyz_acc_mss_i <- -max_xyz_acc_mss}
                    z_vel_ms_i <- z_vel_ms_i + xyz_acc_mss_i * dt
                    z_disp_m_i <- z_disp_m_i + z_vel_ms_i * dt}
                else {
                    if (xy_disp_m_i > xy_stop) {
                        z_vel_ms_i <- 0
                        if (xb_vel_ms_i < max_xb_vel_ms) {
                            xyz_acc_mss_i <- max_xyz_acc_mss}
                        else {
                            xyz_acc_mss_i <- 0}}
                    else {
                        xyz_acc_mss_i <- -max_xyz_acc_mss}
                    xy_disp_m_i <- xy_disp_m_i - xb_vel_ms_i * dt
                    xb_vel_ms_i <- xb_vel_ms_i + xyz_acc_mss_i * dt}

                tend_i <- tend_i + dt
                xy_disp_m <- append(xy_disp_m, xy_disp_m_i)
                z_disp_m <- append(z_disp_m, z_disp_m_i)
                xb_vel_ms <- append(xb_vel_ms, xb_vel_ms_i)
                z_vel_ms <- append(z_vel_ms, z_vel_ms_i)
                xyz_acc_mss <- append(xyz_acc_mss, abs(xyz_acc_mss_i))
                tend <- append(tend, tend_i)}

            common_name <- rep(target_species, length(xy_disp_m))

            sentinal_susceptibility <- rep(FALSE, length(xy_disp_m))

            sentinal_count <- rep(
                (filter(ref_spec, common_name == target_species))$count,
                length(xy_disp_m))

            flock_number <- sample(
                unique(data_ped_train$flock_number),
                length(xy_disp_m),
                replace = TRUE)

            drone <- rep(drone_type, length(xy_disp_m))

            yb_vel_ms <- rep(0, length(xy_disp_m))

            location <- sample(
                unique(data_ped_train$location),
                length(xy_disp_m),
                replace = TRUE)

            drone_obscured <- rep(FALSE, length(xy_disp_m))

            wind_speed_ms <- rep(ref$wind_speed_ms, length(xy_disp_m))

            cloud_cover_p <- rep(ref$cloud_cover_p, length(xy_disp_m))

            temperature_dc <- rep(ref$temperature_dc, length(xy_disp_m))

            month_aest <- rep(5, length(xy_disp_m))

            hrs_since_low_tide <- rep(ref$hrs_since_low_tide, length(xy_disp_m))

            altitude <- rep(altitude_list[a], length(xy_disp_m))

            new_data <- data.frame(
                tend,
                common_name,
                sentinal_susceptibility,
                sentinal_count,
                flock_number,
                drone,
                z_disp_m,
                xy_disp_m,
                xb_vel_ms,
                yb_vel_ms,
                z_vel_ms,
                xyz_acc_mss,
                location,
                drone_obscured,
                wind_speed_ms,
                cloud_cover_p,
                temperature_dc,
                hrs_since_low_tide,
                month_aest,
                altitude)

            prediction <- new_data %>%
                mutate(intlen = 1) %>%
                add_surv_prob(
                    fit,
                    exclude = c("s(flock_number)", "s(location)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- c(120)
target_birds <- c("eastern_curlew")
# altitudes <- seq_range(0:120, n = 20)
# target_birds <- unique(data_ped_train$common_name)

survival_data <- log_simulator(fit, altitudes, target_birds, "mavic 2 pro")

advancing <- survival_data %>%
    filter(altitude == z_disp_m) %>%
    mutate(
        common_name = str_replace(common_name, "_", " "),
        common_name = str_replace(common_name, "_", " "))

plot <- ggplot(
    data = advancing,
    aes(x = xy_disp_m, y = z_disp_m, z = surv_prob)) +
    geom_contour_filled(binwidth = 0.1) +
    geom_contour(colour = "black", binwidth = 0.1, size = 2) +
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
