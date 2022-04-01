prepare_flight_log <- function(
    flight_log,
    drone_specification = "mavic 2 pro",
    target_species = "eastern curlew",
    target_lat = -27.047458,
    target_lon = 153.109222,
    hours_since_low_tide = 7.0173,
    temperature_celcius = 23.8,
    wind_speed_mps = 5.6,
    wind_dir_d = 14,
    cloud_cover_p = 50,
    species_list = c("eastern curlew", "pied stilt"),
    species_count = c(188, 113),
    time_zone = "australia/queensland") {
        flight_log_prep <- flight_log %>%
            mutate(
                video_time_ms = row_number(),
                common_name = target_species,
                eastern_curlew_presence = case_when(
                    "eastern curlew" %in% tolower(species_list) ~ TRUE,
                    TRUE ~ FALSE),
                count = species_count[match(target_species, species_list)],
                flock_number = sample(unique(data_fit$flock_number), size = 1),
                drone = drone_specification,
                lat = target_lat,
                long = target_lon,
                drone_latitude_d = latitude,
                drone_longitude_d = longitude,
                z_disp_m = `height_above_takeoff(meters)`,
                xy_disp_m = (
                    6371009 * sqrt(((pi / 180) *
                    (lat - drone_latitude_d))^2 +
                    ((cos((pi / 180) *
                    (lat + drone_latitude_d) / 2) * (pi / 180) *
                    (long - drone_longitude_d))) ^ 2)),
                bearing_d  = (
                    (180 / pi) * atan2(
                    cos((pi / 180) * lat) *
                    sin((pi / 180) * (long - drone_longitude_d)),
                    cos((pi / 180) * drone_latitude_d) *
                    sin((pi / 180) * lat) -
                    sin((pi / 180) * drone_latitude_d) *
                    cos((pi / 180) * lat) *
                    cos((pi / 180) * (long - drone_longitude_d)))),
                z_vel_ms = `zSpeed(m/s)`,
                x_vel_ms = `xSpeed(m/s)`,
                y_vel_ms = `ySpeed(m/s)`,
                xb_vel_ms = (
                    x_vel_ms * cos((pi / 180) * bearing_d) +
                    y_vel_ms * sin((pi / 180) * bearing_d)),
                yb_vel_ms = abs(
                    y_vel_ms * cos((pi / 180) * bearing_d) -
                    x_vel_ms * sin((pi / 180) * bearing_d)),
                z_acc_mss = (
                    z_vel_ms -
                    lag(z_vel_ms, default = first(z_vel_ms))) /
                    0.1,
                x_acc_mss = (
                    x_vel_ms -
                    lag(x_vel_ms, default = first(x_vel_ms))) /
                    0.1,
                y_acc_mss = (
                    y_vel_ms -
                    lag(y_vel_ms, default = first(y_vel_ms))) /
                    0.1,
                xyz_acc_mss = (z_acc_mss**2 + x_acc_mss**2 + y_acc_mss**2)**0.5,
                location = "toorbul",
                datetime_local = format(
                    as_datetime(`datetime(utc)`),
                    tz = time_zone,
                    usetz = TRUE),
                month_aest = month(datetime_local),
                hrs_since_low_tide = hours_since_low_tide,
                temperature_dc = temperature_celcius,
                wind_speed_ms = wind_speed_mps,
                travel_dir_d = ((180 / pi) * atan2(y_vel_ms, x_vel_ms)) %% 360,
                travel_rel_wind_dir_d = (wind_dir_d - travel_dir_d) %% 360,
                cloud_cover_p = 9)
            return(flight_log_prep)
}
