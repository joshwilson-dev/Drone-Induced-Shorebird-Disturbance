################
#### Header ####
################

# Title: Drone Induced Bird Disturbance Model Training
# Author: Josh Wilson
# Date: 24-04-2023
# References:
# https://cran.r-project.org/web/packages/mgcv/mgcv.pdf
# https://adibender.github.io/pammtools/

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Specify required packages
packages <- c("stringr", "readr", "dplyr", "mgcv", "ggplot2")
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
    # specify factors
    mutate(
        stimulus = as.factor(stimulus),
        flight = as.factor(flight),
        species = as.factor(species),
        location = as.factor(location),
        behind_trees = as.factor(behind_trees))

##############################
#### Train and Save Model ####
##############################

# fit model (took ~ 5hrs on Intel(R) Core(TM) i9-10900X CPU @ 3.70GHz 3.70 GHz)
system.time({
    fit <- gam(
        response ~
        # stimulus
        stimulus +
        s(distance_x_m, k = 5) +
        s(distance_z_m, by = species, k = 5) +
        s(velocity_x_m.s, k = 5) +
        s(velocity_y_m.s, k = 5) +
        s(velocity_z_m.s, k = 5) +
        s(acceleration_xyz_m.s.s, k = 5) +
        s(repeat_approaches, k = 5) +
        # environment
        s(time_since_launch_s, k = 5) +
        behind_trees +
        s(wind_speed_m.s, k = 5) +
        s(cloud_cover_percent, k = 5) +
        s(time_to_high_tide_hr, k = 5) +
        s(temperature_degC, k = 5) +
        location +
        # target
        species +
        s(body_mass, k = 5) +
        s(count, k = 5) +
        s(flight, bs = "re"),
        data = data,
        family = poisson(),
        method = "REML",
        select = TRUE,
        offset = offset)
})

# save model
saveRDS(fit, "models/model.rds")
fit <- readRDS("models/model.rds")

#######################
#### analyse model ####
#######################

summary(fit)

# get inverse link
ilink <- family(fit)$linkinv

# get mean or mode for each variable
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

num <- summarize_if(data, .predicate = is.numeric, ~mean(., na.rm = TRUE))
fac <- summarise_all(select_if(data, ~!is.numeric(.x)), modus)
ref_data <- bind_cols(num, fac)

included_species <- c(
    "Eastern Curlew",
    "Australian Pelican",
    "Grey-tailed Tattler",
    "Whimbrel",
    "Gull-billed Tern",
    "Royal Spoonbill",
    "Great Knot",
    "Pied Stilt",
    "Masked Lapwing",
    "Caspian Tern",
    "Pied Oystercatcher",
    # "Bar-tailed Godwit",
    "White-faced Heron"
    # "Marsh Sandpiper",
    # "Curlew Sandpiper",
    # "Australian White Ibis",
    # "Black Swan",
    # "Cattle Egret",
    # "Great Egret",
    # "Intermediate Egret",
    # "Little Egret",
    # "Silver Gull",
    # "Terek Sandpiper"
    )

included_locations <- c(
    "Thorneside",
    "Toorbul",
    "Wellington Point",
    "Meldale",
    "Cleveland"
)

# create plots
for (term in attributes(fit$terms)$term.labels) {
    print(term)
    # height and width of plots
    height <- 4
    width <- 4
    ref <- ref_data
    # check if term is continuous or categorical
    # and get sequence from min to max, or all unique categories
    # altitude has an interaction term
    if (typeof(data[[term]]) == "double") {
        term_data <- seq(min(data[[term]]), max(data[[term]]))
        if (term == "velocity_z_m.s") {
            term_data <- term_data * -1
            }
        if (term == "distance_z_m") {
            term_data <- seq(3, 120)
            height <- 6
            width <- 6
            species <- rep(
                levels(factor(included_species)),
                each = length(term_data)
                )
            ref <- select(ref_data, -species)
            ref <- cbind(species, ref)
            }
        assign(term, term_data)
        }
    else {
        if (term == "species") {
            assign(term, levels(factor(included_species)))
            }
        else if (term == "location") {
            assign(term, levels(factor(included_locations)))
        }
        else {
            assign(term, levels(data[[term]]))
            }
        }
    # remove the term column from the refence data
    ref <- select(ref, -!!term)
    # create the new data
    newdata <- data.frame(get(term), ref) %>%
        rename(!!term := get.term.)
    # predict from new data
    pred <- predict(
        fit,
        newdata,
        se.fit = TRUE,
        type = "link")
    # transform prediction to response scale
    prediction <- cbind(newdata, pred) %>%
        mutate(
            iupr = ilink(fit + (2 * se.fit)),
            ilwr = ilink(fit - (2 * se.fit)),
            ifit = ilink(fit))
    # generate plot
    plot <- ggplot(data = prediction, aes_string(x = term, y = "ifit")) +
        theme_bw() +
        {if (typeof(data[[term]]) == "double") list(
            geom_line(),
            geom_ribbon(aes(ymin = ilwr, ymax = iupr), alpha = 0.5))} +
        {if (typeof(data[[term]]) != "double") list(
            geom_pointrange(
                aes_string(
                    x = paste0("reorder(", term, ", -ifit)"),
                    ymin = "ilwr",
                    ymax = "iupr")),
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5)))} +
        {if (term == "distance_z_m") list(
            facet_wrap("species"),
            scale_x_continuous(
                breaks = seq(0, 120, 30)))} +
        ylab("hazard rate") +
        xlab(
            gsub("_", " ",
            gsub("\\.", "/",
            gsub("percent", "%",
            gsub("deg", "\u00B0", term))))) +
        scale_y_continuous(
            breaks = c(10^-12 %o% 10^seq(2, 10, 2)),
            trans = scales::log_trans()) +
        coord_cartesian(ylim = c(10^-11, 0.1))
    # save plot
    plot_name <- paste0("plots/", term, ".png")
    ggsave(plot_name, plot, height = height, width = width)
}
