################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Tool
# Author: Josh Wilson
# Date: 31-03-2022
# Reference: https://cran.r-project.org/web/packages/mgcv/mgcv.pdf

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("tidyverse", "mgcv", "lubridate")
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

# Import function to prepare the flightlog
source(choose.files())

# import fitted model
fit <- readRDS(choose.files())

# Import data that the model was fitted with
data_fit <- read_csv(choose.files())
unique(data_fit$flock_number)
# Import your example flight log and prepare
flight_log <- read_csv(choose.files())
prepared_flight_log <- prepare_flight_log(flight_log)

###########################
#### Survival Function ####
###########################

cum_haz <- tapply(fitted(fit), data_fit$id, sum)
censor_id <- tapply(data_fit$behaviour, data_fit$id, sum)
mart <- censor_id - cum_haz
deviance <- sign(mart) * sqrt(-2 * (mart + censor_id * log(cum_haz)))
event_time <- sort(unique(data_fit$video_time_ms))

flight_data <- function(prepared_flight_log) {
    survival_log <- prepared_flight_log %>%
        filter(video_time_ms %in% event_time == TRUE)
    predictions <- predict(
        fit,
        newdata = survival_log,
        type = "lpmatrix",
        exclude = c("s(flock_number)"))
    time_estimate <- drop(predictions %*% coef(fit))
    hazard <- cumsum(exp(time_estimate))
    jacob <- apply(exp(time_estimate) * predictions, 2, cumsum)
    standard_error <- diag(jacob %*% vcov(fit) %*% t(jacob))^.5

    data_plot <- data.frame(
        time = c(0, survival_log$video_time_ms),
        probability = c(0, 1 - exp(-hazard)),
        lower = c(0, 1 - exp(-hazard - standard_error)),
        upper = c(0, 1 - exp(-hazard + standard_error)))

    plot <- ggplot(data_plot, aes(x = time)) +
        geom_point(aes(y = probability)) +
        geom_line(aes(y = upper)) +
        geom_line(aes(y = lower)) +
        ylab("Probability of Flight") +
        xlab("time (ms)") +
        coord_cartesian(ylim = c(0, 1))
    return(plot)
}

plot <- flight_data(prepared_flight_log)
plot

ggplot(data_plot, aes(x = -xy_disp_m, colour = flight)) +
    geom_point(aes(y = z_disp_m)) +
    ylab("Probability of Flight") +
    xlab("time (ms)") +
    guides(colour = "none")
