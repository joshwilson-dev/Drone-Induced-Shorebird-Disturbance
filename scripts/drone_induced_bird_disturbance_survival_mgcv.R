
   
################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Analysis
# Author: Josh Wilson
# Date: 23-03-2022
# Reference: https://cran.r-project.org/web/packages/mgcv/mgcv.pdf

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("tidyverse", "mgcv", "visreg", "pammtools", "gridExtra")
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

# Import Data
data <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################

prepare_data <- function(df, istrain) {
    data_fit <- df %>%
        # degrade data to log once a second for faster convergence
        filter(time %% 1 == 0) %>%
        # convert to factors
        mutate(
            time = as.factor(time),
            drone = as.factor(drone),
            common_name = as.factor(common_name)) %>%
        # keep only useful columns
        select(- notes)

    data_train <- data_fit %>%
        group_by(test, flight, species, behaviour) %>%
        # test ends if birds take flight
        filter(behaviour == 0 | row_number() <= 1) %>%
        # remove row unless a flight event occurred in at least one approach
        group_by(time) %>%
        filter(!all(behaviour == 0)) %>%
        droplevels()

    data_test <- data_fit %>%
        filter(time %in% data_train$time)

    if (istrain) return(data_train)
    else return(data_test)
}

data_test <- prepare_data(data, FALSE)
data_train <- prepare_data(data, TRUE)

###################
#### Fit Model ####
###################

fit <- bam(
    behaviour ~
    time - 1 +

    # target
    s(common_name, bs = "fs") +
    s(count_eastern_curlew) +
    s(flock_number, bs = "re") +
    s(count) +

    # drone
    s(drone, bs = "fs") +

    # approach
    ti(z_disp_m, xy_disp_m) +
    s(z_disp_m) +
    s(xy_disp_m) +
    ti(z_vel_ms, xy_disp_m) +
    s(z_vel_ms) +
    s(xb_vel_ms) +
    s(yb_vel_ms) +
    s(xyz_acc_mss) +

    # environment
    s(month_aest, bs = "cc", k = 4) +
    s(hrs_since_low_tide, bs = "cc") +
    s(temperature_dc) +
    s(wind_speed_ms) +
    s(travel_rel_wind_dir_d, bs = "cc") +
    s(cloud_cover_p),

    family = poisson(),
    data = data_train)
    # method = "REML")

################################
#### Save/Load Fitted Model ####
################################

save_prefix <- "drone-induced-bird-disturbance-gam-"
saveRDS(fit, paste0(save_prefix, format(Sys.time(), "%d-%m-%y_%H-%M"), ".rds"))
fit <- readRDS(choose.files())

#########################
#### Analysis of fit ####
#########################
summary(fit)

# Create New Data

# medium or mode of numerical and character/factor variables
ref <- data_fit %>%
    ungroup() %>%
    sample_info()

# make a dataframe using medium/mode for all values except chosen variable
new_data <- function(variable) {
    # get the original variable data so we can ceck its type later
    var_data <- eval(parse(text = paste0("data_fit$", variable)))
    print(variable)
    new_dataframe <- data_fit %>%
        ungroup() %>%
        mutate(new_col = !!sym(variable)) %>%
        # create new data
        {if (is.numeric(var_data)) {
            make_newdata(., new_col = seq_range(!!sym(variable), n = 10))}
        else if (is.character(var_data) | is.logical(var_data)) {
            make_newdata(., new_col = unique(!!sym(variable)))}} %>%
        select(-!!sym(variable)) %>%
        rename({{ variable }} := new_col) %>%
        # predict fit for new data
        add_term(
            fit,
            term = variable,
            exclude = c("s(flock_number)"),
            reference = ref)
    assign(paste0(variable, "_df"), new_dataframe, envir = .GlobalEnv)
}

predictors <- c(
    "video_time_ms",
    "common_name",
    "eastern_curlew_presence",
    "count",
    "flock_number",
    "drone",
    "z_disp_m",
    "xy_disp_m",
    "z_vel_ms",
    "xb_vel_ms",
    "yb_vel_ms",
    "xyz_acc_mss",
    "location",
    "month_aest",
    "hrs_since_low_tide",
    "temperature_dc",
    "wind_speed_ms",
    "travel_rel_wind_dir_d",
    "cloud_cover_p")

mapply(new_data, predictors)

###########################
#### Fit Visualisation ####
###########################

plot_p_term <- function(variable) {
    var_data <- eval(parse(text = paste0(variable, "_df$", variable)))
    fit <- eval(parse(text = paste0(variable, "_df$fit")))
    dataframe <- eval(parse(text = paste0(variable, "_df")))
    plot <- ggplot(data = dataframe, aes(.data[[variable]], y = fit)) +
    coord_cartesian(ylim = c(-5, 5)) +
    {if(is.character(var_data) | is.logical(var_data)) list(
        geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)),
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)))} +
    {if(is.numeric(var_data)) list(
        geom_line(),
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2))}
    assign(paste0(variable, "_plot"), plot, envir = .GlobalEnv)
}

mapply(plot_p_term, predictors)
do.call("grid.arrange", c(lapply(paste(predictors, "plot", sep = "_"), get)))

###########################
#### Survival Function ####
###########################

cum_haz <- tapply(fitted(fit), data_train$id, sum)
censor_id <- tapply(data_train$behaviour, data_train$id, sum)
mart <- censor_id - cum_haz
deviance <- sign(mart) * sqrt(-2 * (mart + censor_id * log(cum_haz)))
event_time <- sort(unique(data_train$time))

flight_log <- data_test %>%
    filter(test == 138 & flight == 1) %>%
    filter(common_name == "eastern_curlew")

flight_data <- function(flight_log) {
    predictions <- predict(
        fit,
        newdata = flight_log,
        type = "lpmatrix")
        # exclude = c("s(flock_number)"))
    time_estimate <- drop(predictions %*% coef(fit))
    hazard <- cumsum(exp(time_estimate))
    jacob <- apply(exp(time_estimate) * predictions, 2, cumsum)
    standard_error <- diag(jacob %*% vcov(fit) %*% t(jacob))^.5

    data_plot <- data.frame(
        time = c(0, as.integer(levels(droplevels(flight_log$time)))),
        probability = c(0, 1 - exp(-hazard)),
        lower = c(0, 1 - exp(-hazard - standard_error)),
        upper = c(0, 1 - exp(-hazard + standard_error)))
    print(data_plot)
    plot <- ggplot(data_plot, aes(x = time)) +
        geom_point(aes(y = probability)) +
        geom_line(aes(y = upper)) +
        geom_line(aes(y = lower)) +
        ylab("Probability of Flight") +
        xlab("time (s)") +
        coord_cartesian(ylim = c(0, 1))
    return(plot)
}

plot <- flight_data(flight_log)
plot

ggplot(data_plot, aes(x = -xy_disp_m, colour = flight)) +
    geom_point(aes(y = z_disp_m)) +
    ylab("Probability of Flight") +
    xlab("time (ms)") +
    guides(colour = "none")