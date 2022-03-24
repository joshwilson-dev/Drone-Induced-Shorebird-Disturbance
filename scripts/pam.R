################
#### Header ####
################

# Title: Shorebird Disturbance Analysis: FID
# Author: Josh Wilson
# Date: 08-08-2021

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("tidyverse", "mgcv", "visreg")
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
data_clean <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################
data_pam <- data_clean %>%
    # set start time to takeoff
    group_by(test, flight, species) %>%
    mutate(video_time_s = round(video_time_s - first(video_time_s), 1)) %>%
    # id is identifier for each test, flight, species
    group_by(test, flight, species) %>%
    mutate(id = cur_group_id()) %>%
    # end of test is when birds fly, or drone lands or recording finishes
    filter(!is.na(xy_disp_m)) %>%
    filter(!is.na(video_time_s)) %>%
    group_by(test, flight, species, behaviour) %>%
    filter(behaviour == 0 | row_number() <= 1) %>%
    # filter out some drones to make model converge faster
    filter(drone == "mavic 2 pro") %>%
    filter(common_name == "eastern curlew") %>%
    # set up dataset for pam
    group_by(test, flight, species) %>%
    mutate(
        tstart = video_time_s * 10,
        tend = lead(video_time_s * 10),
        interval = tend - tstart,
        offset = log(interval),
        ped_status = case_when(
            tend == last(video_time_s * 10) ~ lead(behaviour),
            TRUE ~ behaviour)) %>%
    drop_na(tend) %>%
    # keep only useful columns
    select(
        - notes
    )

summary(data_pam)

pam <- gam(
    ped_status ~
    s(tend) +

    ## target
    # common_name +
    s(count) +
    s(flock_number, bs = "re") +

    ## drone
    # drone +
    s(z_disp_m) +
    s(xy_disp_m) +
    s(z_vel_ms) +
    s(xy_vel_ms) +
    s(z_acc_mss) +
    s(xy_acc_mss) +

    ## environment
    location +
    s(month_aest, bs = "cc", k = 7) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(rel_wind_dir_d) +
    s(cloud_cover_p),
    data = data_pam,
    family = poisson(),
    offset = offset)

summary(pam)

############################
#### Data Visualisation ####
############################

ref <- sample_info(ungroup(data_pam))

# Effect of xy_disp_m
xy_disp_m_df <- data_pam %>%
    ungroup() %>%
    make_newdata(xy_disp_m = seq_range(xy_disp_m, n = 100)) %>%
    add_term(
        pam,
        term = "xy_disp_m",
        exclude = c("s(flock_number)"),
        reference = ref)

# Effect of z_disp_m
z_disp_m_df <- data_pam %>%
    ungroup() %>%
    make_newdata(z_disp_m = seq_range(z_disp_m, n = 100)) %>%
    add_term(
        pam,
        term = "z_disp_m",
        exclude = c("s(flock_number)"),
        reference = ref)

# Effect of wind_speed_ms
wind_speed_ms_df <- data_pam %>%
    ungroup() %>%
    make_newdata(wind_speed_ms = seq_range(wind_speed_ms, n = 100)) %>%
    add_term(
        pam,
        term = "wind_speed_ms",
        exclude = c("s(flock_number)"),
        reference = ref)

# Effect of rel_wind_dir_d
rel_wind_dir_d_df <- data_pam %>%
    ungroup() %>%
    make_newdata(rel_wind_dir_d = seq_range(rel_wind_dir_d, n = 100)) %>%
    add_term(
        pam,
        term = "rel_wind_dir_d",
        exclude = c("s(flock_number)"),
        reference = ref)

# Effect of cloud_cover_p
cloud_cover_p_df <- data_pam %>%
    ungroup() %>%
    make_newdata(cloud_cover_p = seq_range(cloud_cover_p, n = 100)) %>%
    add_term(
        pam,
        term = "cloud_cover_p",
        exclude = c("s(flock_number)"),
        reference = ref)

# visualise
p_term <- ggplot(data = NULL, aes(y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
    theme(aspect.ratio = 1) +
    ylim(-7, 7)
gridExtra::grid.arrange(
    p_term %+% xy_disp_m_df + aes(x = xy_disp_m),
    p_term %+% z_disp_m_df + aes(x = z_disp_m),
    p_term %+% wind_speed_ms_df + aes(x = wind_speed_ms),
    p_term %+% rel_wind_dir_d_df + aes(x = rel_wind_dir_d),
    p_term %+% cloud_cover_p_df + aes(x = cloud_cover_p),
    nrow = 2L)
