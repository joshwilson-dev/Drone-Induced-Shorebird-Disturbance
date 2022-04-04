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

# Install Packages
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

# Import Packages
lapply(packages, require, character.only = TRUE)

# Import Data
data <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

data_clean <- data %>%
    # degrade data to log once a second for faster convergence
    filter(time %% 1 == 0) %>%
    # test ends if birds take flight
    group_by(test, flight, species, behaviour) %>%
    filter(behaviour == 0 | row_number() <= 1) %>%
    # add end time and status
    group_by(id) %>%
    mutate(
        end_time = max(time),
        status = max(behaviour))

data_fit <- data_clean %>%
    group_by(id) %>%
    filter(row_number() == 1)

data_fit_seq <- data_clean %>%
    group_by(id) %>%
    filter(row_number() != n())

ped <- as_ped(
    data = list(data_fit, data_fit_seq),
    formula = Surv(end_time, status) ~ . + concurrent(
        z_disp_m,
        xy_disp_m,
        z_vel_ms,
        xb_vel_ms,
        yb_vel_ms,
        xyz_acc_mss,
        hrs_since_low_tide,
        travel_rel_wind_dir_d,
        tz_var = "time"),
    id = "id") %>%
    mutate(location = as.factor(location)) %>%
    select(-notes)

###################
#### Fit Model ####
###################

fit <- gam(
    ped_status ~
    s(tend) +

    # target
    common_name +
    s(count_eastern_curlew) +
    s(flock_number, bs = "re") +
    s(count) +

    # drone
    drone +

    # approach
    ti(z_disp_m, xy_disp_m) +
    s(z_disp_m) +
    s(xy_disp_m) +
    s(z_vel_ms) +
    s(xb_vel_ms) +
    s(yb_vel_ms) +
    s(xyz_acc_mss) +

    # environment
    s(location, bs = "re") +
    s(month_aest, bs = "cc", k = 7) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(travel_rel_wind_dir_d, bs = "cc") +
    s(cloud_cover_p),
    data = ped,
    family = poisson(),
    offset = offset)

save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(mod, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
# fit <- readRDS(choose.files())

#########################
#### Analysis of fit ####
#########################

summary(fit)

# Create New Data

# medium or mode of numerical and character/factor variables
ref <- ped %>%
    ungroup() %>%
    sample_info()

# make a dataframe using medium/mode for all values except chosen variable
new_data <- function(variable) {
    # get the original variable data so we can ceck its type later
    var_data <- eval(parse(text = paste0("data_fit$", variable)))
    print(variable)
    new_dataframe <- data_fit %>%
        ungroup() %>%
        mutate(new_col = !!sym(variable)) %>%
        # create new data
        {if (is.numeric(var_data)) {
            make_newdata(., new_col = seq_range(!!sym(variable), n = 10))}
        else if (is.character(var_data) | is.logical(var_data)) {
            make_newdata(., new_col = unique(!!sym(variable)))}} %>%
        select(-!!sym(variable)) %>%
        rename({{ variable }} := new_col) %>%
        # predict fit for new data
        add_term(
            fit,
            term = variable,
            exclude = c("s(flock_number)", "s(location)"),
            reference = ref)
    assign(paste0(variable, "_df"), new_dataframe, envir = .GlobalEnv)
}

predictors <- c(
    "common_name",
    "count_eastern_curlew",
    "flock_number",
    "count",
    "drone",
    "z_disp_m",
    "xy_disp_m",
    "z_vel_ms",
    "xb_vel_ms",
    "yb_vel_ms",
    "xyz_acc_mss",
    "location",
    "month_aest",
    "hrs_since_low_tide",
    "temperature_dc",
    "wind_speed_ms",
    "travel_rel_wind_dir_d",
    "cloud_cover_p")

mapply(new_data, predictors)

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(variable) {
    var_data <- eval(parse(text = paste0(variable, "_df$", variable)))
    fit <- eval(parse(text = paste0(variable, "_df$fit")))
    dataframe <- eval(parse(text = paste0(variable, "_df")))
    plot <- ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
    coord_cartesian(ylim = c(-5, 5)) +
    {if(is.character(var_data) | is.logical(var_data)) list(
        geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)),
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)))} +
    {if(is.numeric(var_data)) list(
        geom_line(),
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2))}
    assign(paste0(variable, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors)
do.call("grid.arrange", c(lapply(paste(predictors, "plot", sep = "_"), get)))

#####################################################
#### Visualise Cumulative Hazard for Real Flight ####
#####################################################

flight <- data %>%
    filter(test == 138 & flight == 1) %>%
    filter(time %% 1 == 0) %>%
    mutate(tend == time + 1) %>%
    add_cumu_hazard(fit) %>%
    add_surv_prob(fit)

p_cumu <-
ggplot(
    flight,
    aes(x = tend, y = cumu_hazard, ymin = cumu_lower, ymax = cumu_upper)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
theme(legend.position = "bottom")

p_surv <- p_cumu + aes(y = surv_prob, ymin = surv_lower, ymax = surv_upper)

gridExtra::grid.arrange(p_cumu, p_surv, nrow = 1L)

##################################################
#### Flight Initiation Distance Visualisation ####
##################################################

# use middle value for all variables, except species, altitude, distance, drone

ref_species <- ped %>%
    group_by(species) %>%
    sample_info()

ref <- ped %>%
    ungroup() %>%
    sample_info()

log_simulator <- function(fit, ref_species, ref, altitude_list,
target_species_list, drone_type) {
    df_i <- data.frame()
    for (i in seq_len(target_species_list)) {
        target_species <- target_species_list[i]
        for (a in seq_len(altitude_list)) {
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
            flock_number <- sample(
                unique(ped$flock_number),
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
                unique(ped$location),
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
                add_cumu_hazard(new_data, fit) %>%
                select(
                    tend,
                    cumu_hazard,
                    cumu_upper,
                    cumu_lower,
                    common_name,
                    z_disp_m,
                    xy_disp_m)

            df_i <- bind_rows(df_i, prediction)
        }
    }
    assign("cum_haz_data", df_i, envir = .GlobalEnv)
}

altitudes <- seq(0:120, by = 10)
target_birds <- unique(ped$common_name)

log_simulator(fit, ref_species, ref, altitudes, target_birds, "mavic 2 pro")

# visualise
ggplot(cum_haz_data, aes(x = xy_disp_m, y = z_disp_m, col = cumu_hazard)) +
facet_wrap(common_name) +
geom_line() +
theme(legend.position = "bottom")