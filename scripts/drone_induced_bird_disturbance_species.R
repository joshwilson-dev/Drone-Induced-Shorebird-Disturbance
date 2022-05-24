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
data <- read_csv(choose.files(), guess_max = 1000000)

# prepare train and test data in ped format
prepare_data <- function(df) {
    data_clean <- df %>%
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
            flock_number = as.factor(flock_number),
            drone = as.factor(drone),
            location = as.factor(location),
            common_name = as.factor(common_name),
            drone_obscured = as.factor(drone_obscured),
            presence_eastern_curlew = as.factor(presence_eastern_curlew)) %>%
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

data_ped <- prepare_data(data)

data_ped_by_species <- data_ped %>%
    split(., with(., common_name))

for (i in 1:length(data_ped_by_species)) {
    assign(
        paste0("data_ped_", unique(data_ped_by_species[[i]]$common_name)),
        data_ped_by_species[[i]]
    )
}

common_names <- unique(data_ped$common_name)

###################
#### Fit Model ####
###################

fit_model <- function (common_name) {
    print(paste("Fitting:", common_name))
    data <- eval(parse(text = paste0("data_ped_", common_name)))

    formula <- ped_status ~
        # drone
        s(drone, bs = "fs") +
        te(xy_disp_m, z_disp_m, k = 4) +
        s(xb_vel_ms, k = 4) +
        s(z_vel_ms, k = 4) +
        s(xyz_acc_mss, k = 4) +
        # environment
        s(tend) +
        s(drone_obscured, bs = "fs") +
        s(wind_speed_ms, k = 4) +
        s(cloud_cover_p, k = 4) +
        s(temperature_dc, k = 4) +
        s(hrs_from_high, k = 4) +
        s(month_aest, bs = "cc", k = 4) +
        s(location, bs = "fs") +
        # target
        s(count, k = 4) +
        s(count_total, k = 4) +
        s(flock_number, bs = "re")

    if (common_name != "eastern_curlew") {
        formula <- update(formula, ~ . + s(presence_eastern_curlew, bs = "fs"))
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

# create dataframes varing each explanatory variable one at a time while
# holding others at medium or mode of numerical and character/factor variables
new_data <- function(var1, var2) {
    # get the original variable data so we can check its type later
    print(paste(var1, var2))

    df_i <- data.frame()
    for (i in 1:length(common_names)) {
        species <- common_names[i]
        print(species)
        data <- eval(parse(text = paste0("data_ped_", species)))
        var1_type <- typeof(eval(parse(text = paste0("data_ped_", species, "$", var1))))
        fit <- eval(parse(text = paste0("fit_", species)))
        new_dataframe <- data %>%
            ungroup() %>%
            mutate(na = 0) %>%
            mutate(new_col1 = !!sym(var1)) %>%
            mutate(new_col2 = !!sym(var2)) %>%
        {
            if (var2 == "na") {
                if (var1_type == "double") {
                    make_newdata(., new_col1 = seq_range(!!sym(var1), n = 100)) %>%
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

common_names <- c("australian_pelican", "pied_stilt")

predictors <- data.frame(
    var1 = c(
        "xy_disp_m",
        "tend",
        "presence_eastern_curlew",
        "count",
        "count_total",
        "flock_number",
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

###########################
#### Fit Visualisation ####
###########################

plot_fit <- function(var1, var2) {
    dataframe <- eval(parse(text = paste0("fit_", var1, "_", var2)))
    var1_type <- typeof(eval(parse(text = paste0("fit_", var1, "_", var2, "$", var1))))
    # fit <- eval(parse(text = paste0("fit_", var1, "_", var2, "$fit")))
    height <- 10
    width <- 10
    title <- paste0("plot_", var1, "_", var2, ".png")
    if (var1 == "xy_disp_m") {
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
            ylab("Altitude [m]")}
    else if(var1_type == "double") {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit, colour = common_name, group = common_name, colour_by)) +
        geom_line() +
        coord_cartesian(ylim = c(-4, 4)) +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = common_name), alpha = 0.2)}
    else if(var1_type == "character" | var1_type == "integer") {
        plot <- ggplot(data = dataframe, aes(.data[[var1]], y = fit, group = common_name, colour = common_name)) +
            geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
            coord_cartesian(ylim = c(-4, 4))}
    if (var1 == "xyz_acc_mss") plot <- plot + xlab("Acceleration [m/s/s]")
    if (var1 == "xb_vel_ms") plot <- plot + xlab("Approach Velocity [m/s]")
    if (var1 == "z_vel_ms") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (var1 == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (var1 == "cloud_cover_p") plot <- plot + xlab("Cloud Cover [%]")
    if (var1 == "wind_speed_ms") plot <- plot + xlab("Wind Speed [ms]")
    if (var1 == "drone_obscured") plot <- plot + xlab("Drone Obscured")
    if (var1 == "flock_number") plot <- plot + xlab("Flock")
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

plot_fit("xy_disp_m", "z_disp_m")

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
    add_surv_prob(fit_australian_pelican, exclude = c("s(flock_number)", "s(location)"))

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
    filter(tend < 140)

ref <- data_ped %>%
    ungroup() %>%
    sample_info()

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
                    presence_eastern_curlew = case_when(
                        species == "eastern_curlew" ~ "true",
                        TRUE ~ "false"),
                    count = ref_species$count,
                    count_total = ref_species$count,
                    flock_number = ref_species$flock_number,
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
                    exclude = c("s(flock_number)", "s(location)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(10:120, by = 10)

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