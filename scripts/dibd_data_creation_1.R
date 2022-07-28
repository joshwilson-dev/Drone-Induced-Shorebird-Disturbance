################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Model Training
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
packages <- c("tidyr", "readr", "dplyr", "ggplot2")
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

# import and check data
data <- read_csv("data/dibd_data.csv")

######################
#### Prepare Data ####
######################

# prepare data in ped format
prepare_data <- function(df) {
    data_ped <- df %>%
        # degrade data but keep flight
        mutate(keep = case_when(
            response == 1 ~ 1,
            time_since_launch %% 3 == 0 ~ 1,
            TRUE ~ 0)) %>%
        # group_by(time_since_launch) %>%
        # mutate(keep = case_when(max(keep) == 1 ~ 1, TRUE ~ 0)) %>%
        filter(keep == 1) %>%
        # create ped parameters
        group_by(flight, species) %>%
        mutate(
            ped_status = lead(response),
            tstart = time_since_launch,
            tend = lead(time_since_launch),
            interval = tend - tstart,
            offset = log(interval)) %>%
        drop_na(ped_status) %>%
        ungroup() %>%
        # group data when eastern curlew took flight
        mutate(species = case_when(
            grepl("with curlew", species) ~ "eastern curlew sentinel",
            TRUE ~ species)) %>%
        filter(species != "eastern curlew sentinel") %>%
        droplevels()
    return(data_ped)
}

data_ped <- prepare_data(data)

# save ped data as new csv
write.csv(
    data_ped,
    "data/dibd_ped_data.csv",
    row.names = FALSE)

# plots for tmbf report
# % of flights where eastern curlew took flight and other species didn't
check <- data %>%
    filter(
        species != "eastern curlew" &
        species != "whimbrel" &
        species != "bar tailed godwit" &
        species != "gull billed tern" &
        species != "great knot" &
        species != "caspian tern" &
        species != "pied stilt" &
        species != "pied oystercatcher" &
        species != "black swan") %>%
    group_by(flight, species) %>%
    filter(response == max(response)) %>%
    slice(1) %>%
    group_by(species) %>%
    select(flight, species, test, approach, response, time_since_launch) %>%
    count(species, response, sort = TRUE) %>%
    mutate(response = case_when(response == 1 ~ "Flight", TRUE ~ "No Flight")) %>%
    pivot_wider(names_from = response, values_from = n) %>%
    mutate(Flight_Percentage = (100 * Flight) / (Flight + `No Flight`))

plot <- data %>%
    filter(
        species != "eastern curlew" &
        species != "whimbrel" &
        species != "bar tailed godwit" &
        species != "gull billed tern" &
        species != "great knot" &
        species != "caspian tern" &
        species != "pied stilt" &
        species != "pied oystercatcher" &
        species != "black swan") %>%
    filter(time_since_launch %% 20 == 0 | response == 1) %>%
    mutate(response = case_when(
        response == 1 ~ "Flight",
        TRUE ~ "No Flight"
    ))

p <- ggplot() +
scale_color_manual(values = c("red", "blue")) +
geom_point(data = plot, aes(x = distance_x, y = distance_z, colour = factor(response)), size = 10) +
# facet_wrap("species") +
scale_x_continuous(limits = c(0, 500), expand = c(0, 0)) +
scale_y_continuous(limits = c(0, 120), expand = c(0, 0)) +
xlab("Horizontal Distance [m]") +
ylab("Altitude [m]") +
labs(fill = "Probability of Birds Having Taken Flight") +
labs(colour = "Raw Data") +
ggtitle("350mm Quadcopter Induced Shorebird Flight Probability with Eastern Curlew") +
coord_fixed(ratio = 1) +
theme(
    plot.title = element_text(hjust = 0.5, size = 80),
    panel.spacing = unit(5, "lines"),
    strip.text = element_text(size = 50, face = "bold"),
    plot.margin = margin(1, 1, 1, 1, "in"),
    axis.ticks = element_line(size = 2),
    axis.ticks.length = unit(.15, "in"),
    axis.text = element_text(size = 80, colour = "black"),
    axis.title = element_text(size = 80, face = "bold"),
    legend.position = "bottom",
    legend.key.size = unit(1, "in"),
    legend.title.align = 0.5,
    legend.text.align = 0,
    legend.box = "horizontal",
    legend.margin = margin(1, 2, 0, 2, unit = "in"),
    legend.text = element_text(size = 60),
    legend.title = element_text(size = 60, face = "bold"),
    legend.key.height = unit(5, "cm"),
    # transparent background
    panel.background = element_rect(fill = "transparent", colour = NA_character_),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA_character_),
    legend.background = element_rect(fill = "transparent", colour = NA_character_),
    legend.box.background = element_rect(fill = "transparent", colour = NA_character_),
    legend.key = element_rect(fill = "transparent")) +
    guides(
        colour = guide_legend(
            nrow = 1,
            title.position = "top",
            title.hjust = 0.5),
        fill = guide_legend(
            nrow = 2,
            title.position = "top",
            title.hjust = 0.5),
        linetype = guide_legend(
            nrow = 3,
            title.position = "top",
            title.hjust = 0.5))
ggsave("plots/trial.png", p, height = 40, width = 60, limitsize = FALSE)
