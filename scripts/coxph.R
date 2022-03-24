################
#### Header ####
################

# Title: Shorebird Disturbance Analysis: FID
# Author: Josh Wilson
# Date: 08-08-2021

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

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

# Import Data
data_clean <- read_csv(choose.files(), guess_max = 1000000)

##########################
#### Data Preparation ####
##########################
data_cox <- data_clean %>%
    # drop control data
    filter(approach_type != "control") %>%
    # set start time to takeoff
    group_by(test, flight, species) %>%
    mutate(video_time_s = round(video_time_s - first(video_time_s), 1)) %>%
    # id is identifier for each test, flight, species
    group_by(test, flight, species) %>%
    mutate(id = cur_group_id()) %>%
    # end of test is when birds fly, or drone lands or recording finishes
    filter(!is.na(xy_disp_m)) %>%
    filter(!is.na(video_time_s)) %>%
    group_by(test, flight, species, behaviour) %>%
    filter(behaviour == 0 | row_number() <= 1) %>%
    # futime is time since from start of test to event
    group_by(test, flight, species) %>%
    mutate(futime =  last(video_time_s) - first(video_time_s)) %>%
    # define factors
    mutate(
        video_time_s = factor(video_time_s),
        common_name = factor(common_name)) %>%
    # filter out some drone to make model converge faster
    filter(drone == "mavic 2 pro") %>%
    filter(common_name == "eastern curlew")
# still NA in acceleration
data_cox_check <- head(data_cox, 10000)
View(data_cox_check)

unique_events <- data_cox  %>%
    group_by(test, flight) %>%
    slice(n()) %>%
    ungroup() %>%
    distinct(video_time_s)

# because i sampled ever 0.1s, every test has same time.
data_cox_aug <- data_cox %>%
    filter(video_time_s %in% unique_events$video_time_s)

check_data_cox_aug <- head(data_cox_aug, 100000)
View(check_data_cox_aug)
#################
#### Fit gam ####
#################
# b <- gam(z ~ tf - 1 + sex + trt + s(sqrt(protime)) + s(platelet)+ s(age)+
# s(bili)+s(albumin), family=poisson,data=pb,method="REML",nthreads=2)

cox_gam <- gam(
    behaviour ~
    video_time_s - 1 +
    s(xy_disp_m) +
    s(z_disp_m),
    family = poisson,
    data = data_cox_aug,
    method = "REML")
summary(cox_gam)

par(mfrow = c(2, 3))
plot(cox_gam, scale = 0)

## compute residuals...
chaz <- tapply(fitted(cox_gam), data_cox_aug$id, sum) ## cum haz by subject
d <- tapply(data_cox_aug$behaviour, data_cox_aug$id, sum) ## censoring indicator
mrsd <- d - chaz ## Martingale
drsd <- sign(mrsd) * sqrt(-2 * (mrsd + d * log(chaz))) ## deviance
## plot survivor function and s.e. band for subject 588
unique(data_cox$id)
di <- data_cox[data_cox$id == 588, ] ## data for subject 588

View(di)
te <- sort(unique(data_cox_aug$futime)) ## event times
View(te)
pd <- di %>%
    filter(video_time_s %in% te)
View(pd)
length(pd)

X <- predict(cox_gam, newdata = pd, type = "lpmatrix")
length(X)
View(X)

X %*% coef(cox_gam)
# something is wrong here??
eta <- drop(X %*% coef(cox_gam))
H <- cumsum(exp(eta))

length(eta)
J <- apply(exp(eta) * X, 2, cumsum)
se <- diag(J %*% vcov(cox_gam) %*% t(J))^.5

length(c(1, exp(-H)))
length(te)

plot(stepfun(te, c(1, exp(-H))), do.points = FALSE, ylim = c(0, 1),
ylab = "S(t)", xlab = "t (s)", main = "", lwd = 2)
lines(stepfun(te, c(1, exp(-H + se))), do.points = FALSE)
lines(stepfun(te, c(1, exp(-H - se))) ,do.points = FALSE)
rug(data_cox$video_time_s[data_cox$id == 25]) ## measurement times