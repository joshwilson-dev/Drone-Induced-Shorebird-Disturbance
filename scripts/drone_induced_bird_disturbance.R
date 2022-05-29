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
        # dropping things
        mutate(sentinel_flight = case_when(
            sentinel_flight == "australian_pelican" ~ "null",
            sentinel_flight == "eastern_curlew_partial" ~ "null",
            sentinel_flight == "masked_lapwing" ~ "null",
            sentinel_flight == "gull_billed_tern" ~ "null",
            TRUE ~ sentinel_flight
        )) %>%
        filter(common_name != "australian_pelican") %>%
        # add column to highlight if it's an eastern curlew
        # or if the sentinel was eastern curlew
        mutate(eastern_curlew = case_when(
            common_name == "eastern_curlew" ~ TRUE,
            sentinel_flight == "eastern_curlew" ~ TRUE,
            TRUE ~ FALSE)) %>%
        mutate(sentinel_flight = case_when(
            sentinel_flight != "null" ~ "true",
            TRUE ~ "false"
        )) %>%
        # filter out species for which we don't have much data
        # id is identifier for each flight, species
        group_by(flight, common_name) %>%
        mutate(id = cur_group_id()) %>%
        group_by(common_name) %>%
        mutate(approaches_species = n_distinct(id)) %>%
        filter(approaches_species > 10) %>%
        # approach ends if birds take flight
        group_by(flight, common_name, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        # degrade data to every 5 seconds to fit faster
        # but keep first sentinel flight and flight
        group_by(flight, behaviour, sentinel_flight) %>%
        mutate(keep = case_when(
            sentinel_flight != "null" & behaviour == 0 & row_number() <= 1 ~ 1,
            behaviour == 1 ~ 1,
            time_since_launch %% 5 == 0 ~ 1,
            TRUE ~ 0)) %>%
        group_by(time_since_launch) %>%
        mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1) %>%
        # specify factors
        mutate(
            drone = as.factor(drone),
            location = as.factor(location),
            flight = as.factor(flight),
            flock = as.factor(flock),
            common_name = str_replace(common_name, "_", " "),
            common_name = str_replace(common_name, "_", " "),
            common_name = factor(common_name, levels = c(
                "eastern curlew",
                "bar tailed godwit",
                "whimbrel",
                "gull billed tern",
                "great knot",
                "caspian tern",
                "pied stilt",
                "pied oystercatcher",
                "black swan")),
            drone_obscured = as.factor(drone_obscured),
            sentinel_flight = as.factor(sentinel_flight),
            eastern_curlew = as.factor(eastern_curlew),
            migrating = as.factor(migrating)
        )

    # create ped parameters
    data_ped <- data_clean %>%
        group_by(flight, common_name) %>%
        mutate(
            ped_status = lead(behaviour),
            tstart = time_since_launch,
            tend = lead(time_since_launch),
            interval = tend - tstart,
            offset = log(interval)) %>%
        drop_na(ped_status) %>%
        ungroup() %>%
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
        drone +
        te(xy_disp_m, z_disp_m, by = eastern_curlew, k = 3) +
        s(xb_vel_ms, k = 3) +
        s(z_vel_ms, k = 3) +
        s(xyz_acc_mss, k = 3) +
        # environment
        s(tend, k = 3) +
        drone_obscured +
        s(wind_speed_ms, k = 3) +
        s(cloud_cover_p, k = 3) +
        s(hrs_from_high, k = 3) +
        s(temperature_dc, k = 3) +
        location +
        # flock
        migrating +
        common_name +
        s(flight, bs = "re") +
        # s(count, k = 3) +
        # s(flock_count, k = 3) +
        # s(sentinel_count, k = 3) +
        sentinel_flight,
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

summary(fit)

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
                eastern_curlew = unique(eastern_curlew),
                new_col1 = seq_range(!!sym(var1), n = 100),
                new_col2 = seq_range(!!sym(var2), n = 100)) %>%
            select(-!!sym(var1)) %>%
            rename({{var1}} := new_col1) %>%
            select(-!!sym(var2)) %>%
            rename({{var2}} := new_col2) %>%
            add_term(., fit, term = c("eastern_curlew", paste0(var1, ",", var2)))
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
        "drone",
        "xy_disp_m",
        "xb_vel_ms",
        "z_vel_ms",
        "xyz_acc_mss",
        "tend",
        "drone_obscured",
        "wind_speed_ms",
        "cloud_cover_p",
        "hrs_from_high",
        "temperature_dc",
        "location",
        "migrating",
        "common_name",
        "flight",
        "sentinel_flight"),
    var2 = c(
        "na",
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
        "na"))

invisible(mapply(new_data, predictors$var1, predictors$var2))
fit_common_name_na <- fit_common_name_na %>%
    filter(!is.na(common_name))
fit_sentinel_flight_na <- fit_sentinel_flight_na %>%
    mutate(sentinel_flight = as.logical(sentinel_flight))
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
                ggplot(data = dataframe, aes(.data[[var1]], y = fit)) +
                geom_line() +
                coord_cartesian(ylim = c(-10, 10)) +
                geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
                ylab("Contribution"))
        }
        else {
            plot <- (
                ggplot( data = dataframe, aes(.data[[var1]], y = fit)) +
                coord_cartesian(ylim = c(-10, 10)) +
                geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
                ylab("Contribution"))
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
            facet_wrap("eastern_curlew") +
            scale_fill_gradientn(colours = c("green", "red")) +
            xlab("Horizontal Distance [m]") +
            ylab("Altitude [m]")
    }

    if (var1 == "xb_vel_ms") plot <- plot + xlab("Approach Velocity [m/s]")
    if (var1 == "z_vel_ms") plot <- plot + xlab("Ascent Velocity [m/s]")
    if (var1 == "xyz_acc_mss") plot <- plot + xlab("Acceleration [m/s/s]")
    if (var1 == "tend") plot <- plot + xlab("Time Since Launch [s]")
    if (var1 == "drone_obscured") plot <- plot + xlab("Drone Obscured")
    if (var1 == "wind_speed_ms") plot <- plot + xlab("Wind Speed [m/s]")
    if (var1 == "cloud_cover_p") plot <- plot + xlab("Cloud Cover [%]")
    if (var1 == "hrs_from_high") plot <- plot + xlab("Time From High Tide [hr]")
    if (var1 == "temperature_dc") plot <- plot + xlab("Temperature (\u00B0C)")
    if (var1 == "migrating") plot <- plot + xlab("Preparing for Migration")
    if (var1 == "flight") plot <- plot + xlab("Flight")
    if (var1 == "sentinel_flight") plot <- plot + xlab("Sentinel Flight")

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
    if (var1 == "common_name" | var1 == "drone" | var1 == "location") {
        height <- 15
        width <- 12.5
        plot <- plot +
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_text(
                    angle = 90,
                    vjust = 0.5,
                    hjust = 0.95))}
    if (var1 == "flight") {
        plot <- plot + theme(axis.text.x = element_blank())}
    ggsave(title, plot, height = height, width = width)
}

mapply(plot_fit, predictors$var1, predictors$var2)

##################################################
#### Flight Initiation Distance Visualisation ####
##################################################

ref <- data_ped %>%
    ungroup() %>%
    sample_info()

test_flight <- read_csv(choose.files()) %>%
    filter(time_since_launch %% 1 == 0)

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
                slice(round(47):n()) %>%
                mutate(z_disp_m = z_disp_m - (120 - altitude))

            flight_log_new <- rbind(flight_ascent, flight_approach) %>%
                select(
                    xy_disp_m,
                    z_disp_m,
                    xb_vel_ms,
                    z_vel_ms,
                    xyz_acc_mss) %>%
                mutate(
                    tend = row_number(),
                    common_name = species,
                    flight = ref_species$flight,
                    cloud_cover_p = ref$cloud_cover_p,
                    hrs_from_high = 0,
                    wind_speed_ms = ref$wind_speed_ms,
                    temperature_dc = ref$temperature_dc,
                    location = ref$location,
                    drone = drone_name,
                    eastern_curlew = case_when(
                        common_name == "eastern curlew" ~ TRUE,
                        TRUE ~ FALSE),
                    drone_obscured = FALSE,
                    migrating = FALSE,
                    sentinel_flight = "false",
                    altitude = altitude)

            prediction <- flight_log_new %>%
                mutate(intlen = 1) %>%
                add_surv_prob(fit, exclude = c("s(flight)", "s(location)"))

            df_i <- bind_rows(df_i, prediction)
        }
    }
    return (df_i)
}

altitudes <- seq_range(0:120, by = 10)
target_birds <- unique(data_ped$common_name)[1:9]
# target_birds <- c("eastern curlew")

survival_data <- log_simulator(fit, altitudes, target_birds, "mavic 2 pro")

advancing <- survival_data %>%
    mutate(z_disp_m = round(z_disp_m)) %>%
    filter(z_disp_m == altitude)

flights <- data_ped %>%
    group_by(flight, common_name) %>%
    mutate(
        ped_max = max(ped_status),
        z_disp_m = abs(z_disp_m),
        xy_disp_m = abs(xy_disp_m)
    ) %>%
    filter(sentinel_flight == "false") %>%
    filter(ped_max == 1) %>%
    filter(ped_status == 1) %>%
    slice(1) %>%
    select(flight, ped_max, drone, z_disp_m, xy_disp_m, common_name)

no_flights <- data_ped %>%
    filter(!is.na(approach)) %>%
    group_by(flight, approach, common_name) %>%
    mutate(ped_max = max(ped_status)) %>%
    filter(ped_max == 0) %>%
    mutate(
        max_z_disp_m = abs(max(z_disp_m)),
        min_xy_disp_m = abs(min(xy_disp_m))
    ) %>%
    slice(1) %>%
    select(flight, approach, ped_max, drone, max_z_disp_m, min_xy_disp_m, common_name) %>%
    rename(z_disp_m = max_z_disp_m, xy_disp_m = min_xy_disp_m) %>%
    filter(!is.na(common_name))

raw_data <- bind_rows(flights, no_flights) %>%
    arrange(ped_max, decreasing = TRUE) %>%
    mutate(
        ped_max = case_when(
            ped_max == 1 ~ "Flight",
            TRUE ~ "No Flight"))

plot <- ggplot() +
    geom_contour_filled(data = advancing, aes(x = xy_disp_m, y = z_disp_m, z = surv_prob), binwidth = 0.1) +
    # geom_contour(data = advancing, colour = "black", binwidth = 0.5, size = 2) +
    scale_fill_brewer(
        type = "div",
        palette = 8,
        direction = 1,
        aesthetics = "fill") +
    scale_color_manual(values = c("red", "green")) +
    # geom_point(data = filter(raw_data, common_name == "eastern curlew", drone == "inspire 2"), aes(x = xy_disp_m, y = z_disp_m, colour = factor(ped_max)), size = 10) +
    # geom_vline(xintercept = 475, colour = "green", size = 5, linetype = "dashed") +
    geom_point(data = filter(raw_data, drone == "mavic 2 pro" | drone == "mavic mini" | drone == "phantom 4 pro"), aes(x = xy_disp_m, y = z_disp_m, colour = factor(ped_max)), size = 10) +
    facet_wrap("common_name") +
    theme_bw() +
    # scale_x_continuous(limits = c(200, 500), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 300), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Horizontal Distance [m]") +
    ylab("Altitude [m]") +
    labs(fill = "Predicted Flight Probability") +
    labs(colour = "Raw Data") +
    theme(
        panel.spacing = unit(5, "lines"),
        strip.text = element_text(size = 60, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "in"),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.15, "in"),
        axis.text = element_text(size = 60),
        axis.title = element_text(size = 80, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "in"),
        legend.title.align = 0.5,
        legend.text.align = 0.5,
        legend.box = "horizontal",
        legend.margin = margin(0, 2, 0, 2, unit = "in"),
        # legend.margin = margin(0, 0, 0, 0, unit = "in"),
        legend.text = element_text(size = 50),
        legend.title = element_text(size = 60, face = "bold")) +
        guides(
            colour = guide_legend(nrow = 2, title.position = "top", title.hjust = 0.5),
            fill = guide_legend(nrow = 2, title.position = "top", title.hjust = 0.5))

# ggsave("inspire2-ec-flight_initiation_distance.png", plot, height = 30, width = 30)
ggsave("flight_initiation_distance.png", plot, height = 40, width = 40)

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################
# unique(data_ped$flight)
# flight_log <- data_ped %>%
#     filter(flight == 160) %>%
#     mutate(drone = "mavic 2 pro") %>%
#     mutate(intlen = tend - tstart) %>%
#     mutate(sentinel_flight = "null") %>%
#     add_surv_prob(fit, exclude = "s(flock)")

# check <- flight_log %>%
#     select(tstart, tend, intlen, z_disp_m, xy_disp_m, xb_vel_ms, xyz_acc_mss, drone, migrating, drone_obscured, sentinel_flight)

# ggplot(flight_log, aes(x = tend, y = surv_prob, ymin = surv_upper, ymax = surv_lower)) +
# geom_ribbon(alpha = 0.3) +
# geom_line() +
# geom_line(aes(y = xy_disp_m / max(xy_disp_m)), colour = "red") +
# geom_line(aes(y = z_disp_m / max(z_disp_m)), colour = "green") +
# geom_line(aes(y = xyz_acc_mss / max(xyz_acc_mss)), colour = "blue") +
# geom_line(aes(y = behaviour), colour = "purple") +
# coord_cartesian(ylim = c(0, 1))

############################
#### General Statistics ####
############################

total_appraoches <- data_ped %>%
    group_by(flight) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(total_appraoches)

approaches_per_species <- data_ped %>%
    group_by(flight, common_name) %>%
    slice(1) %>%
    group_by(common_name) %>%
    summarise(count = n())

View(approaches_per_species)

approaches_per_site <-  data_ped_train %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(location) %>%
    summarise(count = n())

View(approaches_per_site)

approaches_per_drone <- data_ped_train %>%
    group_by(flight) %>%
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
    group_by(flight) %>%
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
    group_by(flight, common_name) %>%
    mutate(ped_status = max(ped_status)) %>%
    select(flight, common_name, ped_status) %>%
    slice(1)

ggplot(plot_data, aes(xy_disp_m, z_disp_m, colour = ped_status)) +
geom_point()

flights <- data_ped %>%
    group_by(flight, common_name) %>%
    filter(ped_status == 1) %>%
    slice(1) %>%
    select(flight, common_name, ped_status, sentinel_flight)
