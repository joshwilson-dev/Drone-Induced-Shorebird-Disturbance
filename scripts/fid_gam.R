#### fid analysis ####

# degrade data to one entry column and filter for just mavic 2 pro disturbance
data_fid <- data_aug %>%

    # only retain disturbance manoeuvre flights
    filter(approach.type == "advancing") %>%

    # remove species without enough data
    filter(species == "eastern curlew") %>%

    # remove all except alarm and flight
    filter(behaviour == 1) %>%

    #remove all drones except mavic 2 pro
    filter(drone == "mavic 2 pro")

# fit GAM

# Main Effects
# Question: What is the flight initiation distance for eastern curlew?

# drone:
# does the fid vary with drone type?

# altitude:
# does the fid vary with drone altitude?

gam_fid <- gam(
    dronebirddistance ~
    s(height_above_takeoff.meters.),
    data = data_fid,
    family = "gaussian",
    method = "REML")

summary(gam_fid)

# Creating new data

altitude_fid <- seq(30, 110, by = 1)

new_data_fid <- expand.grid(height_above_takeoff.meters. = altitude_fid)

pred_fid <- predict.gam(
    gam_fid,
    new_data_fid,
    trans = binomial()$linkinv,
    type = "response",
    se.fit = TRUE)

fid_results <- new_data_fid %>%
    mutate(prediction = pred_fid$fit) %>%
    mutate(upper = pred_fid$fit + (2 * pred_fid$se.fit)) %>%
    mutate(lower = pred_fid$fit - (2 * pred_fid$se.fit))

# Visualising Results

# effect of altitude for mavic 2 data
ggplot() +
    theme_set(theme_bw()) +
    geom_ribbon(
        data = fid_results,
        aes(
            height_above_takeoff.meters.,
            ymin = lower,
            ymax = upper,
            colour = "orange",
            fill = "orange"),
        alpha = 0.1) +
    geom_line(
        data = fid_results,
        aes(
            height_above_takeoff.meters.,
            prediction,
            colour = "orange"),
        size = 1.2) +
    geom_point(
        data = filter(data_fid, drone == "mavic 2 pro"),
        aes(height_above_takeoff.meters.,
            dronebirddistance,
            colour = "orange")) +
    coord_flip(ylim = c(0, 260), xlim = c(0, 120)) +
    labs(x = "Drone Altitude (m)", y = "Flight Initiation Distance (m)") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none",
        aspect.ratio = 0.45) +
    scale_x_continuous(
        minor_breaks = round(seq(0, 140, 20)),
        breaks = round(seq(0, 140, by = 20), 1),
        expand = c(0, 0)) +
    scale_y_continuous(
        minor_breaks = round(seq(-400, 400, 20)),
        breaks = round(seq(-400, 400, by = 20), 1),
        expand = c(0, 0))
