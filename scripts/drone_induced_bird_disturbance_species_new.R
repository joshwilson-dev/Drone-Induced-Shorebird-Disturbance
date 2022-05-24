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

# clear environment
rm(list = ls())

# define required packages
packs <- c("tidyr", "ggplot2", "stringr", "readr", "dplyr", "mgcv", "pammtools")
new_packs <- packs[!(packs %in% installed.packages()[, "Package"])]

# install required packages if user consents
if (length(new_packs)) {
    pack_consent <- readline(
        prompt <- (paste("Install", new_packs, " y/n?\n")))
    if (tolower(pack_consent) == "y") {
        install.packages(new_packs)
        }
    else print(paste("This code cannot be run without", new_packs))
}

# load packages
lapply(packs, require, character.only = TRUE)

##########################
#### Data Preparation ####
##########################

# import data
data <- read_csv(choose.files(), guess_max = 1000000)

# prepare train and test data in ped format
prepare_data <- function(df) {
    data_clean <- df %>%
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
            flock = as.factor(flock),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            migrating = as.factor(migrating))

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

# split ped dataframe into each species
data_ped_by_species <- data_ped %>%
    split(., with(., common_name))

# make split dataframe individual dataframes
for (i in 1:length(data_ped_by_species)) {
    assign(
        paste0("data_ped_", unique(data_ped_by_species[[i]]$common_name)),
        data_ped_by_species[[i]]
    )
}

# get list of species
common_names <- unique(data_ped$common_name)

###################
#### Fit Model ####
###################

# fit and save a model for each species
fit_model <- function (common_name) {
    print(paste("Fitting:", common_name))
    data <- eval(parse(text = paste0("data_ped_", common_name)))

    formula <- ped_status ~
        # drone
        drone +
        te(xy_disp_m, z_disp_m, k = 5) +
        s(xb_vel_ms, k = 5) +
        s(z_vel_ms, k = 5) +
        s(xyz_acc_mss, k = 5) +
        # environment
        s(tend, k = 5) +
        drone_obscured +
        # target
        s(flock, bs = "re") +
        sentinel_flight
    # migration only relevant for migratory shorebirds
    if (
        common_name == "eastern_curlew" |
        common_name == "whimbrel" |
        common_name == "bar_tailed_godwit" |
        common_name == "great_knot") {
        formula <- update(formula, ~ . + migrating)
    }

    fit <- gam(
        formula,
        data = data,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset
    )

    assign(paste0("fit_", common_name), fit, envir = .GlobalEnv)
    saveRDS(fit, paste0("fit_", common_name, ".rds"))
    print(summary(fit))
}

invisible(mapply(fit_model, common_names))

#########################
#### Analysis of fit ####
#########################

# create dataframes investigating fit of each model parameter individually
new_data <- function(var1, var2) {
    print(paste(var1, var2))
    df_i <- data.frame()
    for (i in 1:length(common_names)) {
        species <- common_names[i]
        print(species)
        data <- eval(parse(text = paste0("data_ped_", species)))
        var1_type <- typeof(
            eval(parse(text = paste0("data_ped_", species, "$", var1))))
        fit <- eval(parse(text = paste0("fit_", species)))
        # the below creates a dataframe varying the specified variables then
        # determines the contribution of that variable to the fit of the model
        new_dataframe <- data %>%
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
    }
    assign(
        paste0("fit_", var1, "_", var2),
        df_i,
        envir = .GlobalEnv)
}

predictors <- data.frame(
    var1 = c(
        "xy_disp_m",
        "tend",
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
        "na"))

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

mapply(plot_fit, predictors$var1, predictors$var2)

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight_log <- data_ped %>%
    filter(test == 157 & flight == 1) %>%
    filter(tend <= 145) %>%
    mutate(intlen = tend - tstart) %>%
    mutate(presence_eastern_curlew = "false") %>%
    mutate(count_eastern_curlew = 0) %>%
    add_surv_prob(fit_australian_pelican, exclude = "s(flock)")

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

test_flight <- data_ped %>%
    ungroup() %>%
    filter(test == 157 & flight == 1) %>%
    filter(common_name == "whimbrel") %>%
    filter(tend < 1470)

log_simulator <- function(altitude_list, drone_name) {
    df_i <- data.frame()
    for (i in 1:length(common_names)) {
        species <- common_names[i]
        print(species)
        data <- eval(parse(text = paste0("data_ped_", species)))
        ref_species <- data %>%
            ungroup() %>%
            sample_info()
        fit <- eval(parse(text = paste0("fit_", species)))
        for (y in 1:length(altitude_list)) {
            altitude <- altitude_list[y]

            flight_log <- test_flight

            flight_ascent <- flight_log %>%
                filter(z_disp_m < altitude - 4)

            flight_approach <- flight_log %>%
                slice(47:n()) %>%
                mutate(z_disp_m = z_disp_m - (120 - altitude))

            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    xy_disp_m,
                    z_disp_m,
                    z_vel_ms,
                    xb_vel_ms,
                    xyz_acc_mss) %>%
                mutate(
                    tend = row_number(),
                    common_name = species,
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
                    exclude = c("s(flock)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(0:120, by = 5)
survival_data <- log_simulator(altitudes, "mavic 2 pro")

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

#### OVERLAY WITH RAW DATA ####

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