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

packages <- c("tidyr", "ggplot2", "stringr", "readr", "dplyr", "mgcv", "pammtools")
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
prepare_data <- function(df) {
    data_clean <- df %>%
        filter(common_name == "eastern_curlew" | common_name == "pied_stilt" | common_name == "bar_tailed_godwit" | common_name == "whimbrel") %>%
        select(-count, -id, -col_id) %>%
        # only take first instance of flight for flock
        group_by(test, flight) %>%
        pivot_wider(
            names_from = common_name,
            names_prefix = "behaviour_",
            values_from = behaviour) %>%
        ungroup() %>%
        mutate(flock_behaviour = rowSums(select(., starts_with("behaviour_")), na.rm = TRUE)) %>%
        mutate(flock_behaviour = case_when(flock_behaviour > 0 ~ 1, TRUE ~ 0)) %>%
        select_at(vars(-starts_with("behaviour_"))) %>%
        # degrade data to seconds for faster convergence
        filter(time %% 3 == 0) %>%
        mutate(time = time / 3) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            flock_number = as.factor(flock_number),
            drone_obscured = as.factor(drone_obscured)) %>%
        # approach ends if birds take flight
        group_by(test, flight, flock_behaviour) %>%
        filter(flock_behaviour == 0 | row_number() <= 1)

    data_ped <- data_clean %>%
        mutate(
            ped_status = flock_behaviour,
            tstart = time,
            tend = time + 1,
            interval = tend - tstart,
            offset = log(interval)) %>%
        droplevels()
    return(data_ped)
}

data_ped <- prepare_data(data_new)

###################
#### Fit Model ####
###################

# fit model
system.time({
    fit <- gam(
        ped_status ~
        # drone
        s(drone, bs = "fs") +
        te(xy_disp_m, z_disp_m, k = 4) +
        s(xb_vel_ms, k = 4) +
        s(z_vel_ms, k = 4) +
        s(xyz_acc_mss, k = 4) +
        # environment
        s(tend, k = 4) +
        s(drone_obscured, bs = "fs") +
        s(wind_speed_ms, k = 4) +
        s(cloud_cover_p, k = 4) +
        s(temperature_dc, k = 4) +
        s(hrs_from_high, k = 4) +
        s(month_aest, bs = "cc", k = 4) +
        s(location, bs = "fs") +
        # target
        s(flock_number, bs = "re") +
        te(count_bar_tailed_godwit, count_eastern_curlew, count_pied_stilt),
        data = data_ped,
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

        else if (var1 == "count_eastern_curlew") {
            make_newdata(
                .,
                count_eastern_curlew = seq_range(count_eastern_curlew, n = 100),
                count_pied_stilt = seq_range(count_pied_stilt, n = 100),
                count_bar_tailed_godwit = seq_range(count_bar_tailed_godwit, n = 100)) %>%
            add_term(fit, term = "count_eastern_curlew,count_pied_stilt,count_bar_tailed_godwit")}

        else if (var1 == "drone") {
            make_newdata(., drone = unique(drone)) %>%
            add_term(fit, term = "drone") %>%
            mutate(drone = as.character(drone))}

        else if (var1 == "month_aest") {
            make_newdata(
                .,
                month_aest = unique(month_aest)) %>%
            add_term(fit, term = "month_aest")}

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
        "count_eastern_curlew",
        "flock_number", 
        "tend",
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
        "count_pied_stilt",
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
        plot <- ggplot(
                data = dataframe,
                aes(x = .data[[var1]], y = .data[[var2]], z = fit)) +
            geom_raster(aes(fill = fit)) +
            geom_contour(colour = "black") +
            scale_fill_gradientn(colours = c("green", "red")) +
            xlab("Horizontal Distance [m]") +
            ylab("Altitude [m]")}
    if (var1 == "count_eastern_curlew") {
        plot <- ggplot(
                data = dataframe,
                aes(x = .data[[var1]], y = .data[[var2]], z = fit)) +
            geom_raster(aes(fill = fit)) +
            geom_contour(colour = "black") +
            scale_fill_gradientn(colours = c("green", "red")) +
            xlab("Eastern Curlew Abundance") +
            ylab("Pied Stilt Abundance")}
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
plot_fit("count_eastern_curlew", "count_pied_stilt")
mapply(plot_fit, predictors$var1, predictors$var2)

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight_log <- data_ped %>%
    filter(test == 157 & flight == 1) %>%
    filter(tend <= 145) %>%
    mutate(count_eastern_curlew = 100) %>%
    mutate(count_pied_stilt = 0) %>%
    mutate(count_bar_tailed_godwit = 100) %>%
    mutate(intlen = tend - tstart) %>%
    add_surv_prob(fit, exclude = c("s(flock_number)", "s(location)"))

ggplot(flight_log, aes(x = tend, y = surv_prob, ymin = surv_upper, ymax = surv_lower)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
geom_line(aes(y = xy_disp_m / max(xy_disp_m)), colour = "red") +
geom_line(aes(y = z_disp_m / max(z_disp_m)), colour = "green") +
geom_line(aes(y = xyz_acc_mss / max(xyz_acc_mss)), colour = "blue") +
coord_cartesian(ylim = c(0, 1))

##################################################
#### Flight Initiation Distance Visualisation ####
##################################################

ref <- data_ped %>%
    ungroup() %>%
    sample_info()

test_flight <- data_ped %>%
    ungroup() %>%
    filter(test == 157 & flight == 1) %>%
    # filter(tend < 140) %>%
    filter(tend < 46) %>%
    arrange(time)

log_simulator <- function(fit, altitude_list, species_list, drone_name) {
    df_i <- data.frame()
    for (x in 1:length(species_list)) {
        species <- species_list[x]

        for (y in 1:length(altitude_list)) {
            altitude <- altitude_list[y]

            flight_ascent <- test_flight %>%
                filter(z_disp_m < altitude - 4)

            flight_approach <- test_flight %>%
                slice(16:n()) %>%
                # slice(47:n()) %>%
                mutate(z_disp_m = z_disp_m - (120 - altitude))

            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    xy_disp_m,
                    z_disp_m,
                    xb_vel_ms,
                    z_vel_ms,
                    xyz_acc_mss,
                    time) %>%
                arrange(time) %>%
                mutate(
                    common_name = species,
                    tend = row_number(),
                    count_eastern_curlew = 0,
                    count_pied_stilt = 100,
                    count_bar_tailed_godwit = 0,
                    flock_number = ref$flock_number,
                    drone = drone_name,
                    drone_obscured = FALSE,
                    wind_speed_ms = ref$wind_speed_ms,
                    cloud_cover_p = ref$cloud_cover_p,
                    temperature_dc = ref$temperature_dc,
                    hrs_from_high = ref$hrs_from_high,
                    month_aest = 6,
                    location = ref$location,
                    altitude = altitude)

            prediction <- flight_log_new %>%
                mutate(intlen = 1) %>%
                add_surv_prob(
                    fit,
                    exclude = c("s(flock_number)", "s(location)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(10:120, by = 10)
# target_birds <- unique(data_ped$common_name)
# altitudes <- c(120)
target_birds <- c("pied_stilt")

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