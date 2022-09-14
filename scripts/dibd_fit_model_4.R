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
packages <- c("readr", "dplyr", "mgcv", "ggplot2")
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
data <- read_csv("data/dibd_prep.csv") %>%
    # remove data points where another species already took flight
    filter(response == 0 | sum_response == 1) %>%
    # filter out species that never took flight
    group_by(species) %>%
    filter(max(response) == 1) %>%
    # specify factors
    mutate(
        stimulus = as.factor(stimulus),
        flight = as.factor(flight),
        species = as.factor(species),
        location = as.factor(location),
        obscured = as.factor(obscured))

##############################
#### Train and Save Model ####
##############################

# fit model (took ~ 5hrs on Intel(R) Core(TM) i9-10900X CPU @ 3.70GHz 3.70 GHz)
system.time({
    fit <- gam(
        response ~
        # stimulus
        stimulus +
        s(ground_distance, k = 3) +
        s(altitude, by = species, k = 3) +
        s(approach_velocity, k = 3) +
        s(perpendicular_velocity, k = 3) +
        s(ascent_velocity, k = 3) +
        s(acceleration, k = 3) +
        # environment
        s(tend, k = 3) +
        obscured +
        s(wind_speed, k = 3) +
        s(cloud_cover, k = 3) +
        s(hours_from_high_tide, k = 3) +
        s(temperature, k = 3) +
        location +
        # target
        species +
        s(count, k = 3) +
        s(flight, bs = "re"),
        data = data,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

# save model
saveRDS(fit, "models/model.rds")
# fit <- readRDS("models/model.rds")

# analyse model
summary(fit)
# get mean or mode of variables
ref_data <- data %>%
    ungroup() %>%
    sample_info()
# get inverse link
ilink <- family(fit)$linkinv
# get terms
terms <- attributes(fit$terms)$term.labels

# create plots
for (term in terms) {
    print(term)
    # check if term is continuous or categorical
    # and get sequence from min to max, or all unique categories
    if (term == "altitude") {
        altitude <- seq(min(data$altitude), max(data$altitude))
        species <- rep(levels(data$species), each = length(altitude))
        # remove the changing variables from the reference data
        ref <- select(ref_data, -altitude, -species)
        # create the new data
        newdata <- data.frame(altitude, species, ref)}
    else {
        if (typeof(data[[term]]) == "double") {
            assign(
                term,
                seq(
                    min(data[["count"]], na.rm = T),
                    max(data[["count"]], na.rm = T)))}
        else {assign(term, levels(data[[term]]))}
        # remove the term column from the refence data
        ref <- select(ref_data, -term)
        # create the new data
        newdata <- data.frame(get(term), ref) %>%
            rename(!!term := get.term.)}
    # predict from new data excluding location and
    # flight to make predictions general
    pred <- predict(
        fit,
        newdata,
        se.fit = TRUE,
        type = "link")
    # transform prediction to response scale
    prediction <- cbind(newdata, pred) %>%
        mutate(
            upr = ilink(fit + (2 * se.fit)),
            lwr = ilink(fit - (2 * se.fit)),
            fit = ilink(fit))
    # generate plot
    if (typeof(data[[term]]) == "double") {
        plot <- ggplot(data = prediction, aes_string(x = term, y = "fit")) +
            geom_line() +
            geom_ribbon(
                aes(ymin = lwr, ymax = upr),
                alpha = 0.5) +
            theme(panel.background = element_rect(fill = "white")) +
            ylab("Hazard Rate") +
            facet_wrap("species") +
            ylim(0, 0.25)
    }
    else {
        plot <- ggplot(data = prediction, aes_string(x = term, y = "fit")) +
            geom_pointrange(aes(ymin = lwr, ymax = upr)) +
            theme(panel.background = element_rect(fill = "white")) +
            ylab("Hazard Rate") +
            ylim(0, 0.25)
    }
    # save plot
    plot_name <- paste0("plots/", term, ".png")
    ggsave(plot_name, plot, height = 5, width = 5)
}
