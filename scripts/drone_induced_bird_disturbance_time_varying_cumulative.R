################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Analysis
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

# set theme
theme_set(theme_bw())

# Import Data
data <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

prepare_data <- function(df) {
    tdc_df <- df %>%
        # degrade data to once every second for memory and fast convergence
        filter(time %% 1 == 0) %>%
        # test ends if birds take flight
        group_by(test, flight, species, behaviour) %>%
        filter(behaviour == 0 | row_number() <= 1) %>%
        # add end time and status
        group_by(id) %>%
        mutate(
            end_time = max(time),
            status = max(behaviour)) %>%
        select(
            id,
            test,
            flight,
            time,
            end_time,
            status,
            common_name,
            count_eastern_curlew,
            flock_number,
            drone,
            xy_disp_m,
            z_disp_m,
            xyz_acc_mss_2savg)

    # getting the initial conditions and the time to event
    event_df <- tdc_df %>%
        group_by(id) %>%
        slice(n()) %>%
        ungroup()

    ped_cum <- as_ped(
        data = list(event_df, tdc_df),
        formula = Surv(end_time, status) ~
        . +
        cumulative(
            latency(time),
            xyz_acc_mss_2savg,
            ll_fun = function(t, tz) t >= tz & t <= tz + 10,
            tz_var = "time"),
        id = "id")

    ped_cum$time_latency <- ped_cum$time_latency * ped_cum$LL

    ped_cum <- ped_cum %>%
        select(-xy_disp_m, -z_disp_m)

    ped_con <- tdc_df %>%
        rename(tend = time) %>%
        select(tend, z_disp_m, xy_disp_m, id)

    ped <- merge(ped_cum, ped_con)

    return(ped)
}

data_ped <- prepare_data(data)

View(head(data_ped, 1000))
names(data_ped)

###################
#### Fit Model ####
###################

mod <- gam(
    ped_status ~
    s(tend) +

    # target
    common_name +
    s(count_eastern_curlew) +
    s(flock_number, bs = "re") +
    # s(count) +

    # drone
    drone +

    # approach
    # te(time2_latency, xy_disp_m_time2, by = LL_time2) +
    # te(time2_latency, z_disp_m_time2, by = LL_time2) +
    s(z_disp_m) +
    s(xy_disp_m) +
    te(time_latency, xyz_acc_mss_2savg, by = LL),
    # te(z_vel_ms, xy_disp_m) +
    # s(z_vel_ms) +
    # s(xb_vel_ms) +
    # s(yb_vel_ms) +

    # environment
    # s(month_aest, bs = "cc", k = 5) +
    # s(hrs_since_low_tide, bs = "cc") +
    # s(temperature_dc) +
    # s(wind_speed_ms) +
    # s(travel_rel_wind_dir_d, bs = "cc") +
    # s(cloud_cover_p),
    data = data_ped,
    method = "REML",
    family = poisson(),
    offset = offset)

save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(mod, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
mod <- readRDS(choose.files())

summary(mod)

#########################
#### Analysis of fit ####
#########################

gg_tensor(mod, ci = TRUE) + xlab("latency (s)")

###########################
#### Cumulative Hazard ####
###########################

log_raw <- data %>%
    filter(test == 157) %>%
    filter(flight == 3) %>%
    filter(common_name == "whimbrel") %>%
    mutate(tend = time)

log_cum_haz <- data_ped %>%
    filter(test == 157) %>%
    filter(flight == 3) %>%
    filter(common_name == "whimbrel") %>%
    mutate(intlen = tend - tstart) %>%
    add_cumu_hazard(mod, exclude = "s(flock_number)")

log_surv_prob <- data_ped  %>%
    filter(test == 157) %>%
    filter(flight == 3) %>%
    filter(common_name == "whimbrel") %>%
    mutate(intlen = tend - tstart) %>%
    add_surv_prob(mod, exclude = "s(flock_number)")

ggplot() +
geom_ribbon(data = log_surv_prob, aes(x = tend, ymin = surv_upper, ymax = surv_lower), alpha = 0.3) +
geom_line(data = log_surv_prob, aes(x = tend, y = surv_prob)) +
geom_line(data = log_raw, aes(x = tend, y = xy_disp_m / max(xy_disp_m)), colour = "red") +
geom_line(data = log_raw, aes(x = tend, y = z_disp_m / max(z_disp_m)), colour = "green") +
geom_line(data = log_raw, aes(x = tend, y = xyz_acc_mss_2savg / max(xyz_acc_mss_2savg)), colour = "blue")

ggplot(
    flight_log,
    aes(x = tend, y = cumu_hazard, ymin = cumu_lower, ymax = cumu_upper)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
coord_cartesian(ylim = c(0, 1))

ggplot(
    flight_log,
    aes(x = tend, y = surv_prob, ymin = surv_upper, ymax = surv_lower)) +
geom_ribbon(alpha = 0.3) +
geom_line() +
coord_cartesian(ylim = c(0, 1))

gg_tensor(mod, ci = TRUE) + xlab("latency (s)")
