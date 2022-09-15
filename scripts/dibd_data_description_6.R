################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Data Description
# Author: Josh Wilson
# Date: 13-06-2022
# References:
# https://cran.r-project.org/web/packages/mgcv/mgcv.pdf
# https://adibender.github.io/pammtools/

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Specify required packages
packages <- c("readr", "dplyr", "ggplot2")
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

# import data
data <- read_csv("data/dibd_prep.csv")

############################
#### General Statistics ####
############################

total_appraoches <- data %>%
    group_by(flight) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(total_appraoches)

approaches_per_species <- data %>%
    group_by(flight, species) %>%
    slice(1) %>%
    group_by(species) %>%
    summarise(Approaches = n())

View(approaches_per_species)

approaches_per_species_plot <- ggplot(
    data = approaches_per_species,
    aes(x = reorder(species, -Approaches), y = Approaches)) +
    geom_bar(stat = "identity") +
    theme(
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab("Species")

ggsave(
    "plots/approaches_per_species.png",
    approaches_per_species_plot,
    height = 4,
    width = 4)

approaches_with_sentinel <- data %>%
    group_by(flight, species) %>%
    filter(response == 1 & sum_response > 1) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(approaches_with_sentinel)

approaches_without_sentinel <- data %>%
    group_by(flight, species) %>%
    filter(sum_response >= 1) %>%
    filter(max(response) == 0) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(approaches_without_sentinel)

approaches_per_site <-  data_ped %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(location) %>%
    summarise(count = n())

View(approaches_per_site)

approaches_per_drone <- data_ped %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(specification) %>%
    summarise(count = n())

View(approaches_per_drone)

sd(data$temperature)