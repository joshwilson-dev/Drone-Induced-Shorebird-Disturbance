
#### takeoff analysis ####
data_fit <- data_aug %>%

    # keep only flights where the drone was advancing
    filter(approach.type == "ascending") %>%

    # drop the landed behaviour state
    filter(behaviour != 2) %>%

    # keep only the maximum behaviour state
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(flightcode, species) %>%
    filter(behaviour == max(behaviour)) %>%

    # filter out species without enough data
    # group_by(species) %>%
    # filter(n() > 25) %>%
    filter(species == "eastern curlew") %>%

    # filter out drones without much data
    filter(
        drone != "mavic mini",
        drone != "phantom 4 pro",
        drone != "phantom pro 4")

    # to fix
    # drop_na(drone) %>%
    # filter(drone != "mavic pro 2") %>%
    # filter(flightcode != "311") %>%
    # filter(flightcode != "411")


# fit GAM

# Main Effects
# Question: at what altitude does flight not occur?

# drone:
# does the behaviour vary with drone type?

# altitude:
# does the behaviour vary with drone altitude?

# species:
# does the behaviour vary with species

gam_fit <- gam(
    behaviour
    ~ s(dronebirddistance)
    + drone,
    data = data_fit,
    family = "binomial",
    method = "REML")

summary(gam_fit)

# Creating new data

dronebirddistance_fit = seq(75, 750, by = 1)
drone_fit <- unique(data_fit$drone)

new_data_fit <- expand.grid(
    dronebirddistance = dronebirddistance_fit,
    drone = drone_fit)

pred_fit <- predict.gam(
    gam_fit,
    new_data_fit,
    trans = binomial()$linkinv,
    type = "response",
    se.fit = TRUE)

fit_results <- new_data_fit %>%
    mutate(prediction = pred_fit$fit) %>%
    mutate(upper = pred_fit$fit + (2 * pred_fit$se.fit)) %>%
    mutate(lower = pred_fit$fit - (2 * pred_fit$se.fit))

# Visualising Results
ggplot() +
    theme_set(theme_bw()) +
    geom_line(
        data = fit_results,
        aes(dronebirddistance, prediction, group = drone, colour = drone),
        size = 1.2) +
    geom_ribbon(
        data = fit_results,
        aes(
            dronebirddistance,
            ymin = lower,
            ymax = upper,
            group = drone,
            fill = drone,
            colour = drone),
        alpha = 0.1) +
    geom_rug(
        aes(dronebirddistance,group = drone, colour = drone),
        inherit.aes = FALSE,
        transform(
            filter(data_fit, behaviour == 0),
            expl.name = dronebirddistance),
        sides = "b",
        size = 1) +
    geom_rug(
        aes(dronebirddistance, group = drone, colour = drone),
        inherit.aes = FALSE,
        transform(
            filter(data_fit, behaviour == 1),
            expl.name = dronebirddistance),
        sides = "t",
        size = 1) +
    labs(
        x = "Drone Takeoff Distance (m)",
        y = "Probability of Inducing Bird Flight",
        fill = "Drone",
        colour = "Drone") +
    coord_cartesian(xlim = c(75, 750)) +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(fill = alpha("blue", 0))) +
    scale_x_continuous(
        expand = c(0, 0),
        breaks = round(seq(0, 800, by = 100), 1)) +
    scale_y_continuous(expand = c(0, 0))
