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
data <- read_csv(choose.files(), guess_max = 1000000)

# prepare train and test data in ped format
prepare_data <- function(df) {
    data_clean <- df %>%
        # filter(common_name == "pied_stilt") %>%
        # add migrating for migratory shorebirds
        mutate(migrating = case_when(
            (month_aest == 4 | month_aest == 5) &
            (common_name == "eastern_curlew" |
            common_name == "whimbrel" |
            common_name == "bar_tailed_godwit" |
            common_name == "great_knot") ~ TRUE,
            TRUE ~ FALSE)) %>%
        # filter out species for which we don't have much data
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 30) %>%
        # round sentinel flight to last second
        mutate(time_round = floor(time)) %>%
        group_by(test, flight, common_name, time_round) %>%
        mutate(sentinel_flight = as.logical(max(sentinel_flight))) %>%
        # degade log to once every second
        filter(time %% 1 == 0) %>%
        # approach ends if birds take flight
        group_by(test, flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        # degrade data to every 5 seconds to fit faster
        # but keep first sentinel flight and flight
        group_by(test, flight, behaviour, sentinel_flight) %>%
        mutate(keep = case_when(
            sentinel_flight == 1 & behaviour == 0 & row_number() <= 1 ~ 1,
            behaviour == 1 ~ 1,
            time %% 5 == 0 ~ 1,
            TRUE ~ 0)) %>%
        group_by(time) %>%
        mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            approach = as.factor(approach),
            flock = as.factor(flock),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            migrating = as.factor(migrating)
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
        ungroup() %>%
        droplevels()
    return(data_ped)
}

data_ped <- prepare_data(data)

check <- data_ped %>%
    group_by(flight, test) %>%
    mutate(
        ped_max = max(ped_status),
        sentinel_max = max(as.logical(sentinel_flight)),
    ) %>%
    slice(1) %>%
    select(flight, test, ped_max, sentinel_max)

###################
#### Fit Model ####
###################

# fit model
system.time({
    fit <- gam(
        ped_status ~
        # drone
        drone +
        te(xy_disp_m, z_disp_m, by = common_name, k = 5) +
        # te(xy_disp_m, z_disp_m, k = 5) +
        s(xb_vel_ms, k = 5) +
        s(z_vel_ms, k = 5) +
        s(xyz_acc_mss, k = 5) +
        # environment
        s(tend, k = 5) +
        drone_obscured +
        migrating * common_name +
        # migrating +
        # target
        # s(flock, bs = "re") +
        # s(flock, by = common_name, bs = "re") +
        # s(approach, bs = "re") +
        sentinel_flight,
        data = data_ped,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

summary(fit)

# save model
save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))

# load model
fit <- readRDS(choose.files())

#########################
#### Analysis of fit ####
#########################

# create dataframes investigating fit of each model parameter individually
new_data <- function(var1, var2) {
    print(paste(var1, var2))
    df_i <- data.frame()
    var1_type <- typeof(eval(parse(text = paste0("data_ped$", var1))))
    # the below creates a dataframe varying the specified variables then
    # determines the contribution of that variable to the fit of the model
    new_dataframe <- data_ped %>%
        ungroup() %>%
        mutate(na = 0) %>%
        mutate(new_col1 = !!sym(var1)) %>%
        mutate(new_col2 = !!sym(var2)) %>%
    {
        if (var2 == "na") {
            if (var1_type == "double") {
                make_newdata(
                    .,
                    new_col1 = seq_range(!!sym(var1), n = 100)) %>%
                select(-!!sym(var1)) %>%
                rename({{var1}} := new_col1) %>%
                add_term(., fit, term = var1)
            }
            else {
                make_newdata(., new_col1 = unique(!!sym(var1))) %>%
                select(-!!sym(var1)) %>%
                rename({{var1}} := new_col1) %>%
                add_term(., fit, term = var1)
            }
        }
        else {
            make_newdata(
                .,
                new_col1 = seq_range(!!sym(var1), n = 100),
                new_col2 = seq_range(!!sym(var2), n = 100)) %>%
            select(-!!sym(var1)) %>%
            rename({{var1}} := new_col1) %>%
            select(-!!sym(var2)) %>%
            rename({{var2}} := new_col2) %>%
            add_term(., fit, term = paste0(var1, ",", var2))
        }
    }
    df_i <- bind_rows(df_i, new_dataframe)
    assign(
        paste0("fit_", var1, "_", var2),
        df_i,
        envir = .GlobalEnv)
}

predictors <- data.frame(
    var1 = c(
        "xy_disp_m",
        "tend",
        "common_name",
        "approach",
        "flock",
        "drone",
        "xyz_acc_mss",
        "xb_vel_ms",
        "z_vel_ms",
        "drone_obscured",
        "sentinel_flight",
        "migrating"),
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
        "na"))

new_data("tend", "na")

invisible(mapply(new_data, predictors$var1, predictors$var2))

###########################
#### Fit Visualisation ####
###########################

plot_fit <- function(var1, var2) {
    dataframe <- eval(parse(text = paste0("fit_", var1, "_", var2)))
    var1_type <- typeof(
        eval(parse(text = paste0("fit_", var1, "_", var2, "$", var1))))
    height <- 10
    width <- 10
    title <- paste0("plot_", var1, "_", var2, ".png")
    if (var2 == "na") {
        if (var1_type == "double") {
            plot <- (
                ggplot(
                    data = dataframe,
                    aes(
                        .data[[var1]],
                        y = fit,
                        colour = common_name,
                        group = common_name,
                        colour_by)) +
                geom_line() +
                coord_cartesian(ylim = c(-4, 4)) +
                geom_ribbon(
                    aes(ymin = ci_lower, ymax = ci_upper, fill = common_name),
                    alpha = 0.2))
        }
        else {
            plot <- (
                ggplot(
                    data = dataframe,
                    aes(
                        .data[[var1]],
                        y = fit,
                        group = common_name,
                        colour = common_name)) +
                geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
                coord_cartesian(ylim = c(-4, 4)))
        }
    }
    else {
        dataframe <- dataframe %>%
        filter(!!sym(var1) >= 0, !!sym(var1) <= 500)
        plot <- ggplot(
                data = dataframe,
                aes(x = .data[[var1]], y = .data[[var2]], z = fit)) +
            geom_raster(aes(fill = fit)) +
            geom_contour(colour = "black") +
            facet_wrap("common_name") +
            scale_fill_gradientn(colours = c("green", "red")) +
            xlab("Horizontal Distance [m]") +
            ylab("Altitude [m]")
    }
    if (var1 == "xyz_acc_mss") plot <- plot + xlab("Acceleration [m/s/s]")
    if (var1 == "xb_vel_ms") plot <- plot + xlab("Approach Velocity [m/s]")
    if (var1 == "z_vel_ms") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (var1 == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (var1 == "drone_obscured") plot <- plot + xlab("Drone Obscured")
    if (var1 == "flock") plot <- plot + xlab("Flock")
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
    if (var1 == "flock") {
        plot <- plot + theme(axis.text.x = element_blank())}

    ggsave(title, plot, height = height, width = width)
}
plot_fit("tend", "na")

mapply(plot_fit, predictors$var1, predictors$var2)

##################################################
#### Flight Initiation Distance Visualisation ####
##################################################
prepare_test <- function(df) {
    data_clean <- df %>%
        filter(test == 157, flight == 1, common_name == "whimbrel") %>%
        # 1 time interval is 0.1s
        mutate(time = time * 10) %>%
        filter(time < 1470) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            approach = as.factor(approach),
            flock = as.factor(flock),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            migrating = as.factor(migrating)
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
        ungroup() %>%
        droplevels()
    return(data_ped)
}

ref <- data_ped %>%
    ungroup() %>%
    sample_info()

test_flight <- prepare_test(data)

log_simulator <- function(fit, altitude_list, species_list, drone_name) {
    df_i <- data.frame()
    for (x in 1:length(species_list)) {
        species <- species_list[x]

        ref_species <- data_ped %>%
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
                    yb_vel_ms,
                    xyz_acc_mss) %>%
                mutate(
                    tend = row_number(),
                    common_name = species,
                    approach = ref_species$approach,
                    flock = ref_species$flock,
                    drone = drone_name,
                    drone_obscured = FALSE,
                    migrating = FALSE,
                    sentinel_flight = FALSE,
                    altitude = altitude)

            prediction <- flight_log_new %>%
                mutate(intlen = 1) %>%
                add_surv_prob(
                    fit,
                    exclude = c("s(flock)", "s(approach)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(0:120, by = 5)
# target_birds <- c("pied_oys""pied_stilt", "eastern_curlew")

# altitudes <- seq_range(10:120, by = 10)
target_birds <- unique(data_ped$common_name)

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

data_plot <- data %>%
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
