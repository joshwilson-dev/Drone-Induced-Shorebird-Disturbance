################
#### Header ####
################

# Title: Simple Drone Log Simulator
# Author: Josh Wilson
# Date: 31-03-2022

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("tidyverse", "rlang")
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

#######################
#### Log Simulator ####
#######################

time_step <- function(disp_m_i, vel_ms_i, acc_mss_i,
max_disp_m, max_vel_ms, max_acc_mss, dt) {
    disp_m_i_label <- as_label(enexpr(disp_m_i))
    vel_ms_i_label <- as_label(enexpr(vel_ms_i))
    acc_mss_i_label <- as_label(enexpr(acc_mss_i))

    distance_to_stop <- vel_ms_i**2 / 2 * abs(max_acc_mss)
    if (abs(max_disp_m - disp_m_i)  <= distance_to_stop) {
        acc_mss_i <- -max_acc_mss
    }
    else if (round(abs(vel_ms_i), 1) >= max_vel_ms) {
        acc_mss_i <- 0
    }
    else acc_mss_i <- max_acc_mss
    disp_m_i <- disp_m_i + vel_ms_i * dt / 10
    vel_ms_i <- vel_ms_i + acc_mss_i * dt / 10
    assign(disp_m_i_label, disp_m_i, envir = parent.frame())
    assign(vel_ms_i_label, vel_ms_i, envir = parent.frame())
    assign(acc_mss_i_label, acc_mss_i, envir = parent.frame())
}

log_simulator <- function(
    target_species = "eastern curlew",
    drone_specification = "mavic 2 pro",
    max_xyz_acc_mss = 2,
    max_z_vel_ms = 2,
    max_xb_vel_ms = 5,
    start_xy_disp_m = 300,
    start_z_disp_m = 0,
    max_z_disp_m = 60,
    test_location = "toorbul",
    species_list = c("eastern curlew", "pied stilt"),
    species_count = c(188, 113),
    num_month_aest = 2,
    hrs_after_low_tide = 6,
    temp_dc = 25,
    wind_vel_ms = 5,
    wind_dir_d = 180,
    cloud_p = 50) {
        video_time_ms <- c(0)
        z_disp_m <- c(start_z_disp_m)
        xy_disp_m <- c(start_xy_disp_m)
        z_vel_ms <- c(0)
        xb_vel_ms <- c(0)
        yb_vel_ms <- c(0)
        xyz_acc_mss <- c(0)

        # initial conditions
        dt <- 5
        video_time_ms_i <- video_time_ms[1]
        z_disp_m_i <- z_disp_m[1]
        z_vel_ms_i <- z_vel_ms[1]
        xb_vel_ms_i <- xb_vel_ms[1]
        xy_disp_m_i <- 0
        xyz_acc_mss_i <- xyz_acc_mss[1]
        drone_state <- "takeoff"
        while (drone_state != "landed") {
            if (drone_state == "takeoff") {
                time_step(z_disp_m_i, z_vel_ms_i, xyz_acc_mss_i, max_z_disp_m, max_z_vel_ms, max_xyz_acc_mss, dt)
                if (z_disp_m_i >= max_z_disp_m | video_time_ms_i > 4000) {
                    drone_state <- "approach"
                    z_vel_ms_i <- 0
                }
            }
            else if (drone_state == "approach") {
                time_step(xy_disp_m_i, xb_vel_ms_i, xyz_acc_mss_i, start_xy_disp_m, max_xb_vel_ms, max_xyz_acc_mss, dt)
                if (xy_disp_m_i >= start_xy_disp_m | video_time_ms_i > 4000) {
                    drone_state <- "hover"
                    start_hover <- video_time_ms_i
                    xyz_acc_mss_i <- 0
                    xb_vel_ms_i <- 0
                }
            }
            else if (drone_state == "hover") {
                if (video_time_ms_i - start_hover >= 50) {
                    drone_state <- "return"
                }
            }
            else if (drone_state == "return") {
                time_step(xy_disp_m_i, xb_vel_ms_i, xyz_acc_mss_i, 0, max_xb_vel_ms, -max_xyz_acc_mss, dt)
                if (xy_disp_m_i <= 0 | video_time_ms_i > 4000) {
                    drone_state <- "landing"
                    xb_vel_ms_i <- 0
                }
            }
            else if (drone_state == "landing") {
                time_step(z_disp_m_i, z_vel_ms_i, xyz_acc_mss_i, 0, max_z_vel_ms, -max_xyz_acc_mss, dt)
                if (z_disp_m_i <= 0 | video_time_ms_i > 4000) {
                    drone_state <- "landed"
                    z_vel_ms_i <- 0
                    xyz_acc_mss_i <- 0
                }
            }
            print(paste("drone_state", drone_state))
            print(paste("video_time", video_time_ms_i))
            print(paste("altitude", z_disp_m_i))
            print(paste("z_velocity", z_vel_ms_i))
            print(paste("acceleration", xyz_acc_mss_i))
            print(paste("xb_velocity", xb_vel_ms_i))
            print(paste("xy_disp", xy_disp_m_i))
            video_time_ms_i <- video_time_ms_i + dt
            # update matrix
            # video_time_ms <- append(video_time_ms, video_time_ms_i)
            # z_disp_m <- append(common_name, common_name[1])
            # xy_disp_m <- append(common_name, common_name[1])
            # z_vel_ms <- append(common_name, common_name[1])
            # xb_vel_ms <- append(common_name, common_name[1])
            # yb_vel_ms <- append(common_name, common_name[1])
            # xyz_acc_mss <- append(common_name, common_name[1])
        }
    # common_name <- rep(target_species),
    # count <- c(species_count[match(target_species, species_list)]),
    # flock_number <- sample(1:45),
    # drone <- c(drone_specification),
    # location <- c(test_location),
    # month_aest <- c(num_month_aest),
    # hrs_since_low_tide <- c(hrs_after_low_tide),
    # temperature_dc <- c(temp_dc),
    # wind_speed_ms <- c(wind_vel_ms),
    # travel_rel_wind_dir_d <- c(rel_wind_dir_d),
    # cloud_cover_p <- c(cloud_p)
}

log_simulator()
