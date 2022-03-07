################
#### Header ####
################

# Title: Shorebird Disturbance Analysis
# Author: Josh Wilson
# Date: 08-08-2021

###############
#### Setup ####
###############
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

# Clear Environment
rm(list = ls())

# Import Data
data <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

data_fia <- data %>%
    # keep only flights where the drone was advancing
    filter(approach_type == "advancing") %>%
    # degrade data into first instance of maximum behaviour per approach type
    mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered = TRUE)) %>%
    group_by(test, flight, species) %>%
    filter(behaviour == max(behaviour)) %>%
    slice(1)

#################
#### Fit gam ####
#################

# Question: at what altitude does flight not occur?

# Main Effects

# altitude:
# does the behaviour vary with drone altitude?

# species:
# does the behaviour vary with species

# eastern curlew presence:
# does the behaviour vary with the presence of eastern curlew

# life stage
# does the behaviour vary with life stage

# Interactions

# altitude - species:
# does the relationship between altitude and behaviour vary between species

# altitude - drone:
# does the relationship between altitude and behaviour depend on the drone

gam_fia <- gam(
    behaviour
    ~ species +
    s(height_above_takeoff.meters.) +
    eastern.curlew.presence,
    data = data_fia,
    family = "binomial",
    method = "REML",
    select = T)
summary(gam_fia)

###########################
#### Visualise Results ####
###########################

altitude_fia <- seq(0, 120, by = 1)
species_fia <- unique(data_fia$species)
eastern_curlew_prescence_fia <- unique(data_fia$eastern.curlew.presence)

new_data_fia <- expand.grid(
    height_above_takeoff.meters. = altitude_fia,
    species = species_fia,
    eastern.curlew.presence = eastern_curlew_prescence_fia,
    month = month_fia)

pred_fia <- predict.gam(gam_fia,
                    new_data_fia,
                    trans = binomial()$linkinv,
                    type = "response",
                    se.fit = TRUE)

results_fia <- new_data_fia %>%
    mutate(prediction = pred_fia$fit) %>%
    mutate(upper = pred_fia$fit + (2 * pred_fia$se.fit)) %>%
    mutate(lower = pred_fia$fit - (2 * pred_fia$se.fit))

# species sensitivity vs height above takeoff

ggplot() +
    theme_set(theme_bw()) +
    geom_line(
        data = filter(results_fia, eastern.curlew.presence == FALSE),
        aes(
            height_above_takeoff.meters.,
            prediction,
            group = species,
            colour = species),
        size = 1.2) +
    geom_ribbon(
        data = filter(results_fia, eastern.curlew.presence == FALSE),
        aes(
            height_above_takeoff.meters.,
            ymin = lower,
            ymax = upper,
            group = species,
            colour = species,
            fill = species),
        alpha = 0.05) +
    facet_wrap(~species) +
    geom_rug(
        aes(height_above_takeoff.meters., group = species, colour = species),
        inherit.aes = FALSE,
        transform(
            filter(data_fia, behaviour == 0),
            expl.name = height_above_takeoff.meters.),
        sides = "b") +
    geom_rug(
        aes(height_above_takeoff.meters., group = species, colour = species),
        inherit.aes = FALSE,
        transform(
            filter(data_fia, behaviour == 1),
            expl.name = height_above_takeoff.meters.),
        sides = "t") +
    labs(
        x = "Drone Altitude Above Birds (m)",
        y = "Probability of Inducing Bird Flight",
        fill = "Eastern Curlew Precense",
        colour = "Eastern Curlew Precense") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.text.x = element_text(size = 10, face = "bold"))

# species sensitivity vs eastern curlew presence
ggplot() +
    theme_set(theme_bw()) +
    geom_rug(
        data =
        filter(
            data_fia,
            behaviour == 0 &
            species != "eastern curlew" &
            height_above_takeoff.meters. > 40),
        aes(eastern.curlew.presence,
            as.numeric(behaviour),
            colour = eastern.curlew.presence),
        sides = "b",
        position = "jitter",
        size = 1) +
    geom_rug(
        data =
        filter(
            data_fia,
            behaviour == 1 &
            species != "eastern curlew" &
            height_above_takeoff.meters. > 40),
        aes(
            eastern.curlew.presence,
            as.numeric(behaviour),
            colour = eastern.curlew.presence),
        sides = "t",
        position = "jitter",
        size = 1) +
    geom_boxplot(
        data =
        filter(
            results_fia,
            species != "eastern curlew" &
            height_above_takeoff.meters. > 40),
        aes(
            eastern.curlew.presence,
            as.numeric(prediction),
            fill = eastern.curlew.presence),
        alpha = 0.4) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(
        x = "Eastern Curlew Presence",
        y = "Probability of Inducing Bird Flight") +
    theme(
        axis.text = element_text(face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "none")



#### PLAYING AROUND ####
summary(data)
data_play <- data_fia %>%

# Remove all drones except inspire 2
filter(drone == "inspire 2") %>%
filter(behaviour == 0)