################
#### Header ####
################

# Title: Merging Bird Disturbance Data
# Author: Josh Wilson
# Date: 29-08-2021

###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("dplyr", "readxl")
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

#################
#### Content ####
#################

# Import Data
data_dir <- choose.dir()
flights <- 1:269
dataset <- data.frame()
for (approach in flights) {
    dir <- paste0("flight-", approach)
    file <- paste0(dir, ".xlsx")
    loc <- file.path(data_dir, dir, file)
    data <- read_excel(loc, col_types = "text")
    dataset <- bind_rows(dataset, data)
    print(paste("Read:", loc))
}

# Save Data
write.csv(dataset, "dibd.csv", row.names = FALSE)
