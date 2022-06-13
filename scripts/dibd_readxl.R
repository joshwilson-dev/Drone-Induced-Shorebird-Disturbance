################
#### Header ####
################

# Title: Merging Shorebird Disturbance Data
# Author: Josh Wilson
# Date: 01-09-2021


###############
#### Setup ####
###############

# Clear Environment
rm(list = ls())

# Install Packages
packages <- c("tidyverse", "readxl")
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

#####################
#### import data ####
#####################

# Data to Import
filenames <- c(
    "t003f01", # DONE

    "t004f01", # DONE

    "t005f01", # DONE

    "t006f01", # DONE

    "t007f01", # DONE

    "t008f01", # DONE

    "t009f01", # DONE

    "t010f01", # DONE

    "t011f01", # DONE

    "t012f01", # DONE

    "t013f01", # DONE

    "t014f01", # DONE

    "t015f01", # DONE

    "t016f01", # DONE

    "t017f01", # DONE

    "t018f00", # DONE

    "t019f01", # DONE

    "t020f01", # DONE

    "t021f01", # DONE

    "t022f01", # DONE

    "t024f01", # DONE

    "t025f01", # DONE

    "t026f01", # DONE

    "t027f01", # DONE

    "t028f01", # DONE

    "t029f01", # DONE

    "t030f01", # DONE

    "t031f01", # DONE

    "t032f01", # DONE

    "t033f01", # DONE

    "t034f01", # DONE

    "t035f01", # DONE

    "t036f01", # DONE

    "t037f01", # DONE

    "t038f01", # DONE

    "t039f01", # DONE

    "t040f01", # DONE

    "t041f01", # DONE

    "t042f01", # DONE

    "t043f01", # DONE

    "t044f01", # DONE

    "t045f01", # DONE

    "t046f01", # DONE

    "t047f01", # DONE

    "t048f01", # DONE

    "t049f01", # DONE

    "t050f01", # DONE

    "t051f01", # DONE

    "t052f01", # DONE

    "t053f01", # DONE

    "t054f01", # DONE

    "t055f01", # DONE

    "t056f01", # DONE

    "t057f01", # DONE
    "t057f02", # DONE
    "t057f03", # DONE
    "t057f04", # DONE
    "t057f05", # DONE
    "t057f06", # DONE
    "t057f07", # DONE
    "t057f08", # DONE
    "t057f09", # DONE

    "t058f01", # DONE
    "t058f02", # DONE

    "t059f01", # DONE

    "t060f01", # DONE
    "t060f02", # DONE
    "t060f03", # DONE
    "t060f04", # DONE
    "t060f05", # DONE

    "t061f01", # DONE
    "t061f02", # DONE
    "t061f03", # DONE

    "t062f01", # DONE
    "t062f02", # DONE
    "t062f03", # DONE
    "t062f04", # DONE

    "t063f01", # DONE

    "t064f01", # DONE

    "t065f01", # DONE

    "t066f01", # DONE
    "t066f02", # DONE

    "t067f01", # DONE
    "t067f02", # DONE
    "t067f03", # DONE
    "t067f04", # DONE
    "t067f05", # DONE
    "t067f06", # DONE

    "t068f01", # DONE
    "t068f02", # DONE
    "t068f03", # DONE
    "t068f04", # DONE

    "t069f01", # DONE
    "t069f02", # DONE
    "t069f03", # DONE
    "t069f04", # DONE
    "t069f05", # DONE

    "t070f01", # DONE
    "t070f02", # DONE
    "t070f03", # DONE

    "t071f01", # DONE
    # "t071f02", # DONE TOOK OFF IN CONTROL

    "t072f01", # DONE
    "t072f02", # DONE
    "t072f03", # DONE
    "t072f04", # DONE

    "t073f01", # DONE
    "t073f02", # DONE
    "t073f03", # DONE
    "t073f04", # DONE

    "t074f01", # DONE

    "t075f01", # DONE
    "t075f02", # DONE
    "t075f03", # DONE
    "t075f04", # DONE
    "t075f05", # DONE

    "t076f01", # DONE
    "t076f02", # DONE
    "t076f03", # DONE
    "t076f04", # DONE
    # "t076f05", # DONE TOOK OFF IN CONTROL

    "t077f01", # DONE
    "t077f02", # DONE
    "t077f03", # DONE
    "t077f04", # DONE

    "t078f01", # DONE
    "t078f02", # DONE
    "t078f03", # DONE
    "t078f04", # DONE
    "t078f05", # DONE

    "t079f01", # DONE
    "t079f02", # DONE
    "t079f03", # DONE
    "t079f04", # DONE

    "t080f01", # DONE
    "t080f02", # DONE
    "t080f03", # DONE
    "t080f04", # DONE
    "t080f05", # DONE

    "t081f01", # DONE
    "t081f02", # DONE
    "t081f03", # DONE
    "t081f04", # DONE
    "t081f05", # DONE
    "t081f06", # DONE

    "t082f01", # DONE
    "t082f02", # DONE
    "t082f03", # DONE

    "t083f01", # DONE

    "t084f01", # DONE

    "t085f01", # DONE

    "t086f01", # DONE

    "t087f01", # DONE
    "t087f02", # DONE

    "t088f01", # DONE

    "t089f01", # DONE

    "t090f01", # DONE

    "t091f01", # DONE

    "t092f01", # DONE

    "t093f01", # DONE

    # "t094f01", # DONE ALTERNATE DISTURBANCE

    "t095f01", # DONE

    "t096f01", # DONE

    "t097f01", # DONE

    "t098f01", # DONE

    "t099f01", # DONE

    # "t100f01", # DONE SPIBIS

    # "t101f01", # DRONE FAULT

    # "t102f01", SPIBIS

    # "t103f01", # DONE SPIBIS

    # "t104f01", # DONE LOW TIDE

    # "t105f01", # DONE LOW TIDE

    # "t106f01", # DONE LOW TIDE

    # "t107f01", # DONE LOW TIDE

    # "t108f01", # DONE LOW TIDE

    # "t109f01", # DONE LOW TIDE

    # "t110f01", # DONE LOW TIDE

    # "t111f01", # DONE LOW TIDE

    # "t112f01", # DONE LOW TIDE

    # "t113f01", # DONE LOW TIDE

    # "t114f01", # DONE SPIBIS

    # "t115f01", # DONE SPIBIS

    # "t116f01", # DONE SPIBIS

    # "t117f01", SPIBIS

    "t118f01", # DONE

    "t119f01", # DONE

    "t120f01", # DONE

    "t121f01", # DONE

    "t122f01", # DONE

    "t123f01", # DONE

    "t124f01", # DONE

    "t125f01", # DONE

    "t126f01", # DONE

    "t127f01", # DONE

    "t128f01", # DONE

    "t129f01", # DONE

    "t130f01", # DONE

    "t131f01", # DONE

    "t132f01", # DONE

    "t133f01", # DONE

    "t134f01", # DONE

    "t135f01", # DONE

    "t136f01", # DONE

    "t137f01", # DONE

    "t138f01", # DONE

    "t139f01", # DONE

    "t140f01", # DONE

    # "t141f01", # DONE TOOK OFF IN CONTROL

    "t142f01", # DONE

    "t143f01", # DONE

    "t144f01", # DONE

    "t145f01", # DONE

    "t146f01", # DONE
    # "t146f02", # DONE TOOK OFF IN CONTROL

    # "t147f01", # DONE TOOK OFF IN CONTROL

    # "t148f01", # DONE TOOK OFF IN CONTROL

    "t149f01", # DONE

    "t150f01", # DONE
    "t150f02", # DONE

    "t151f01", # DONE
    "t151f02", # DONE

    "t152f01", # DONE

    "t153f01", # DONE

    "t154f01", # DONE

    "t155f01", # DONE
    "t155f02", # DONE

    "t156f01", # DONE
    "t156f02", # DONE
    "t156f03", # DONE

    "t157f01", # DONE
    "t157f02", # DONE
    "t157f03", # DONE

    "t158f01", # DONE

    "t159f01", # DONE

    "t160f01", # DONE

    "t161f01", # DONE

    "t162f01", # DONE

    "t163f01", # DONE

    # "t164f01", # DONE MANLY

    "t165f01", # DONE

    # "t166f01", DIDN'T LAND

    # "t167f01", DIDN'T LAND

    "t168f01" # DONE
    )

# Import Data
data_dir <- choose.dir()

importdata <- function(filenames) {
    # imports data from excel worksheet at location of the form:
    for (i in seq_len(length(filenames))) {
        filename <- filenames[i]
        test <- substr(filename, 2, 4)
        flight <- substr(filename, 6, 8)
        location <- paste0(
            data_dir,
            "/test-",
            test,
            "/",
            flight,
            "/",
            filename,
            ".xlsx")
        assign(
            filename,
            read_excel(location, col_types = "text"), envir = .GlobalEnv)
        print(paste0("Assigned: ", location))
    }
}
    
importdata(filenames)

########################
#### Merge and Save ####
########################

# Merge Data
data <- bind_rows(mget(filenames))

# Save Data
write.csv(
    data,
    "shorebird-disturbance.csv",
    row.names = FALSE)
