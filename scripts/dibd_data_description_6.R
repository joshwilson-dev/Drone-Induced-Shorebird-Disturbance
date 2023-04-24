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
packages <- c("readr", "dplyr", "ggplot2", "tidyr")
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

approaches <- data %>%
    group_by(flight, species, count) %>%
    slice(1) %>%
    group_by(species, scientific_name) %>%
    summarise(`Total Approaches` = n())

total <- data %>%
    mutate(date_aest = as.Date(datetime_aest, tz = "australia/queensland")) %>%
    group_by(date_aest, location, species) %>%
    mutate(total = max(count)) %>%
    slice(1) %>%
    group_by(species, scientific_name) %>%
    summarise(`Total Count` = sum(total))

disturbances <- data %>%
    mutate(date_aest = as.Date(datetime_aest, tz = "australia/queensland")) %>%
    group_by(date_aest, location, species) %>%
    mutate(disturbances = max(response)) %>%
    slice(1) %>%
    group_by(species, scientific_name) %>%
    summarise(`Disturbed Approaches` = sum(disturbances))

disturbed <- data %>%
    mutate(date_aest = as.Date(datetime_aest, tz = "australia/queensland")) %>%
    group_by(date_aest, location, species) %>%
    mutate(disturbed = max(response) * max(count)) %>%
    slice(1) %>%
    group_by(species, scientific_name) %>%
    summarise(`Disturbed Count` = sum(disturbed))

no_sentinels <- data %>%
    filter(response == 0 | sum_response == 1) %>%
    mutate(date_aest = as.Date(datetime_aest, tz = "australia/queensland")) %>%
    group_by(date_aest, location, species) %>%
    mutate(disturbances = max(response)) %>%
    slice(1) %>%
    group_by(species, scientific_name) %>%
    summarise(`Disturbed No Sentinel Approaches` = sum(disturbances))

no_sentinel <- data %>%
    filter(response == 0 | sum_response == 1) %>%
    mutate(date_aest = as.Date(datetime_aest, tz = "australia/queensland")) %>%
    group_by(date_aest, location, species) %>%
    mutate(sentinel = max(response) * max(count)) %>%
    slice(1) %>%
    group_by(species, scientific_name) %>%
    summarise(`Disturbed No Sentinel Count` = sum(sentinel))

summary <- approaches %>%
    merge(disturbances) %>%
    merge(no_sentinels) %>%
    merge(total) %>%
    merge(disturbed) %>%
    merge(no_sentinel) %>%
    mutate(scientific_name = paste0(
        toupper(substring(scientific_name, 1, 1)),
        substring(scientific_name, 2)))

View(summary)

write.csv(summary, "data/approach_summary.csv", row.names = FALSE)

approaches_without_sentinel <- data %>%
    group_by(flight, species) %>%
    filter(sum_response >= 1) %>%
    filter(max(response) == 0) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(count = n())

View(approaches_without_sentinel)

approaches_per_site <-  data %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(location) %>%
    summarise(count = n())

View(approaches_per_site)

approaches_per_drone <- data %>%
    group_by(flight) %>%
    slice(1) %>%
    group_by(stimulus) %>%
    summarise(count = n())

View(approaches_per_drone)

modus <- function(var) {
  # calculate modus
  freqs <- table(var)
  mod   <- names(freqs)[which.max(freqs)]
  # factors should be returned as factors with all factor levels
  if (is.factor(var)) {
    mod <- factor(mod, levels = levels(var))
  }
  return(mod)
}

num <- summarize_if(data, .predicate = is.numeric, ~mean(., na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "variable") %>%
    rename("mean" = value)

sd <- summarize_if(data, .predicate = is.numeric, ~sd(., na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = "variable") %>%
    rename("sd" = value)

fac <- summarise_all(select_if(data, ~!is.numeric(.x)), modus) %>%
    pivot_longer(cols = everything(), names_to = "variable") %>%
    rename("mode" = value)

reference_data <- num %>%
    merge(sd) %>%
    bind_rows(fac) %>%
    filter(!grepl("count_|response_", variable)) %>%
    filter(
        variable != "bird_latitude",
        variable != "bird_longitude",
        variable != "flight",
        variable != "interval",
        variable != "offset",
        variable != "response",
        variable != "sum_response",
        variable != "wind_direction_deg",
        variable != "scientific_name",
        variable != "life_stage",
        variable != "activity",
        variable != "age",
        variable != "datetime_aest")
View(reference_data)

write.csv(reference_data, "data/reference_data.csv", row.names = FALSE)
