
#### Ethics Analysis ####

# number of approaches
approaches <- data_aug %>%
    # flights not in 2021
    filter(datetime_aest > as.Date("2021-01-01")) %>%
    filter(datetime_aest < as.Date("2022-01-01")) %>%
    # remove flights where birds took off in control
    filter(approach.type != "control") %>%
    # group by flight
    group_by(flightcode) %>%
    # take first entry
    slice(1)

# number of disturbances
disturbance_approaches <- data_aug %>%
    # flights not in 2021
    filter(datetime_aest > as.Date("2021-01-01")) %>%
    filter(datetime_aest < as.Date("2022-01-01")) %>%
    # only keep flights where drone was approaching
    filter(
        approach.type == "ascending" |
        approach.type == "advancing") %>%
    # filter for birds fligt
    filter(behaviour == 2) %>%
    # remove disturbances from alternate sources
    mutate(notes = case_when(
        notes == "alternate disturbance" ~ 1,
        TRUE ~ 2)) %>%
    filter(notes == 2) %>%
    # group by flight
    group_by(flightcode) %>%
    # take first entry
    slice(1)

# total birds disturbed
birds_disturbed <- data_aug  %>%
    # filter for birds flight
    filter(behaviour == 2) %>%
    # group each species for each flight
    group_by(flightcode, species) %>%
    # take first entry
    slice(1) %>%
    # only keep flights where drone was approaching
    filter(
        approach.type == "ascending" |
        approach.type == "advancing") %>%
    # remove disturbances from alternate sources
    mutate(notes = case_when(
        notes == "alternate disturbance" ~ 1,
        TRUE ~ 2)) %>%
    filter(notes == 2) %>%
    # flights not in 2021
    filter(datetime_aest > as.Date("2021-01-01")) %>%
    filter(datetime_aest < as.Date("2022-01-01")) %>%
    # summarise
    group_by(species) %>%
    summarise(sum = sum(count))
(sum(birds_disturbed$sum) - 4000)

# species
species <- data_aug %>%
    # filter out species without enough data
    group_by(species) %>%
    filter(n() > 5) %>%
    # take first entry to get unique species
    slice(1)
