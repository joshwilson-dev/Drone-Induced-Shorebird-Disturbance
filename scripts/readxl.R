# Merging Shorebird Disturbance Data
  # Josh Wilson
    # 01-09-2021

#### Setup ####
# Install Packages
packages <- c("tidyverse")
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
library(tidyverse)
library(readxl)
  
# Clear Environment
rm(list = ls())

#### import data ####
# Data to Import
filenames <- c(
    # PHANTOM 4 PRO
    "t003f00", # DONE
    "t003f01", # DONE
    "t003f02", # DONE
    "t003f03", # DONE
    "t003f04", # DONE
    "t003f05", # DONE
    "t003f06", # DONE
    "t003f07", # DONE
    # "t003f08", # direct descent

    "t004f00", # DONE
    "t004f01", # DONE
    "t004f02", # DONE
    "t004f03", # DONE
    "t004f04", # DONE
    "t004f05", # DONE
    "t004f06", # DONE
    "t004f07", # DONE
    # "t004f08", # direct descent

    "t005f00", # DONE
    "t005f01", # DONE
    "t005f02", # DONE
    "t005f03", # DONE
    "t005f04", # DONE
    "t005f05", # DONE
    "t005f06", # DONE
    "t005f07", # DONE
    # "t005f08", # direct descent

    "t006f01", # DONE
    "t006f02", # DONE
    "t006f03", # DONE
    "t006f04", # DONE
    "t006f05", # DONE
    "t006f06", # DONE
    "t006f07", # DONE

    "t007f00", # DONE
    "t007f01", # DONE
    "t007f02", # DONE
    "t007f03", # DONE
    "t007f04", # DONE
    "t007f05", # DONE
    "t007f06", # DONE

    "t008f00", # DONE
    "t008f01", # DONE
    "t008f02", # DONE
    "t008f03", # DONE
    "t008f04", # DONE
    "t008f05", # DONE
    "t008f06", # DONE

    "t009f00", # DONE
    "t009f01", # DONE
    "t009f02", # DONE
    "t009f03", # DONE
    "t009f04", # DONE
    "t009f05", # DONE
    "t009f06", # DONE

    "t010f00", # DONE
    "t010f01", # DONE
    "t010f02", # DONE
    "t010f03", # DONE
    "t010f04", # DONE

    "t011f00", # DONE
    "t011f01", # DONE
    "t011f02", # DONE
    "t011f03", # DONE
    "t011f04", # DONE

    "t012f00", # DONE
    "t012f01", # DONE
    "t012f02", # DONE
    "t012f03", # DONE
    "t012f04", # DONE
    "t012f05", # DONE

    "t013f00", # DONE
    "t013f01", # DONE
    "t013f02", # DONE
    "t013f03", # DONE
    "t013f04", # DONE
    "t013f05", # DONE

    "t014f00", # DONE
    "t014f01", # DONE
    "t014f02", # DONE
    "t014f03", # DONE
    "t014f04", # DONE
    "t014f05", # DONE

    "t015f00", # DONE
    "t015f01", # DONE
    "t015f02", # DONE
    "t015f03", # DONE
    "t015f04", # DONE
    "t015f05", # DONE
    "t015f06", # DONE

    "t016f00", # DONE
    "t016f01", # DONE
    "t016f02", # DONE
    "t016f03", # DONE
    "t016f04", # DONE

    "t017f00", # DONE
    "t017f01", # DONE
    "t017f02", # DONE
    "t017f03", # DONE
    "t017f04", # DONE
    "t017f05", # DONE
    "t017f06", # DONE

    "t018f00", # DONE
    "t018f00", # DONE

    "t019f00", # DONE
    "t019f01", # DONE
    "t019f02", # DONE
    "t019f03", # DONE
    "t019f04", # DONE
    "t019f05", # DONE
    "t019f06", # DONE

    "t020f00", # DONE
    "t020f01", # DONE
    "t020f02", # DONE
    "t020f03", # DONE
    "t020f04", # DONE

    "t021f00", # DONE
    "t021f01", # DONE
    "t021f02", # DONE
    "t021f03", # DONE
    "t021f04", # DONE
    "t021f05", # DONE
    "t021f06", # DONE

    "t022f00", # DONE
    "t022f01", # DONE
    "t022f02", # DONE
    "t022f03", # DONE
    "t022f04", # DONE
    "t022f05", # DONE
    "t022f06", # DONE

    "t023f00", # DONE

    "t024f00", # DONE
    "t024f01", # DONE
    "t024f02", # DONE
    "t024f03", # DONE
    "t024f04", # DONE
    "t024f05", # DONE
    "t024f06", # DONE
    "t024f07", # DONE

    "t025f00", # DONE
    "t025f01", # DONE
    "t025f02", # DONE
    "t025f03", # DONE
    "t025f04", # DONE
    "t025f05", # DONE
    "t025f06", # DONE

    "t026f00", # DONE
    "t026f01", # DONE
    "t026f02", # DONE
    "t026f03", # DONE
    "t026f04", # DONE
    "t026f05", # DONE
    "t026f06", # DONE

    "t027f00", # DONE
    "t027f01", # DONE
    "t027f02", # DONE
    "t027f03", # DONE
    "t027f04", # DONE
    "t027f05", # DONE
    "t027f06", # DONE

    "t028f00", # DONE
    "t028f01", # DONE
    "t028f02", # DONE
    "t028f03", # DONE
    "t028f04", # DONE
    "t028f05", # DONE
    "t028f06", # DONE
    "t028f07", # DONE

    "t029f00", # DONE
    "t029f01", # DONE
    "t029f02", # DONE
    "t029f03", # DONE
    "t029f04", # DONE
    "t029f05", # DONE
    "t029f06", # DONE
    "t029f07", # DONE

    "t030f00", # DONE
    "t030f01", # DONE
    "t030f02", # DONE
    "t030f03", # DONE
    "t030f04", # DONE
    "t030f05", # DONE
    "t030f06", # DONE
    "t030f07", # DONE

    "t031f00", # DONE
    "t031f01", # DONE
    "t031f02", # DONE
    "t031f03", # DONE
    "t031f04", # DONE
    "t031f05", # DONE
    "t031f06", # DONE

    "t032f00", # DONE
    "t032f01", # DONE
    "t032f02", # DONE
    "t032f03", # DONE
    "t032f04", # DONE
    "t032f05", # DONE
    "t032f06", # DONE
    "t032f07", # DONE

    "t033f00", # DONE
    "t033f01", # DONE
    "t033f02", # DONE
    "t033f03", # DONE
    "t033f04", # DONE
    "t033f05", # DONE
    "t033f06", # DONE
    "t033f07", # DONE

    # INSPIRE 2

    "t034f00", # DONE
    "t034f01", # DONE

    "t035f00", # DONE
    "t035f01", # DONE

    "t036f00", # DONE
    "t036f01", # DONE

    "t037f00", # DONE
    "t037f01", # DONE

    "t038f00", # DONE
    "t038f01", # DONE

    "t039f00", # DONE
    "t039f01", # DONE

    "t040f00", # DONE
    "t040f01", # DONE

    "t041f00", # DONE
    "t041f01", # DONE

    "t042f00", # DONE
    "t042f01", # DONE

    "t043f00", # DONE
    "t043f01", # DONE

    "t044f00", # DONE
    "t044f01", # DONE

    "t045f00", # DONE
    "t045f01", # DONE
    "t045f02", # DONE

    "t046f00", # DONE
    "t046f01", # DONE
    "t046f02", # DONE

    "t047f00", # DONE
    "t047f01", # DONE

    "t048f00", # DONE
    "t048f01", # DONE
    "t048f02", # DONE

    "t049f00", # DONE
    "t049f01", # DONE

    "t050f00", # DONE
    "t050f01", # DONE

    "t051f00", # DONE
    "t051f01", # DONE

    "t052f00", # DONE
    "t052f01", # DONE

    "t053f00", # DONE
    "t053f01", # DONE

    "t054f00", # DONE
    "t054f01", # DONE

    "t056f00", # DONE
    "t056f01", # DONE

    "t057f00", # DONE
    "t057f01", # DONE
    "t057f02", # DONE
    "t057f03", # DONE
    "t057f04", # DONE
    "t057f05", # DONE
    "t057f06", # DONE
    "t057f07", # DONE
    "t057f08", # DONE
    "t057f09", # DONE

    "t058f00", # DONE
    "t058f01", # DONE
    "t058f02", # DONE

    "t059f00", # DONE
    "t059f01", # DONE

    "t060f00", # DONE
    "t060f01", # DONE
    "t060f02", # DONE
    "t060f03", # DONE
    "t060f04", # DONE
    "t060f05", # DONE

    "t061f00", # DONE
    "t061f01", # DONE
    "t061f02", # DONE
    "t061f03", # DONE

    "t062f00", # DONE
    "t062f01", # DONE
    "t062f02", # DONE
    "t062f03", # DONE
    "t062f04", # DONE

    "t063f00", # DONE
    "t063f01", # DONE

    "t064f00", # DONE
    "t064f01", # DONE

    "t065f01", # DONE
    "t065f02", # DONE

    "t066f00", # DONE
    "t066f01", # DONE
    "t066f02", # DONE

    "t067f00", # DONE
    "t067f01", # DONE
    "t067f02", # DONE
    "t067f03", # DONE
    "t067f04", # DONE
    "t067f05", # DONE
    "t067f06", # DONE

    "t068f00", # DONE
    "t068f01", # DONE
    "t068f02", # DONE
    "t068f03", # DONE
    "t068f04", # DONE

    "t069f00", # DONE
    "t069f01", # DONE
    "t069f02", # DONE
    "t069f03", # DONE
    "t069f04", # DONE
    "t069f05", # DONE

    # MAVIC 2 PRO

    "t070f01", # DONE
    "t070f02", # DONE
    "t070f03", # DONE

    "t071f01", # DONE
    "t071f02", # DONE

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
    "t076f05", # DONE

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

# STARTED DOING DIFFERENT RECORD HERE

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

    "t094f01", # DONE

    "t095f01", # DONE

    "t096f01", # DONE

    "t097f01", # DONE

    "t098f01", # DONE

    "t099f01", # DONE

    # "t100f01", # spibis needs weather

    # "t101f01", # spibis

    # "t102f01", # spibis

    # "t103f01", # spibis

    # "t104f01", # low tide

    # "t105f01", # low tide

    # "t106f01", # low tide

    # "t107f01", # low tide

    # "t108f01", # low tide

    # "t109f01", # low tide

    # "t110f01", # low tide

    # "t111f01", # low tide

    # "t112f01", # low tide

    # "t113f01", # low tide

    # "t114f01", # spibis

    # "t115f01", # spibis

    # "t116f01", # spibis

    "t117f01", # DONE

    "t118f01", # DONE

    "t119f01", # DONE

    "t120f01", # DONE

    "t121f01", # DONE

    "t122f01", # DONE

    "t123f01", # DONE

    "t124f01", # completed

    "t125f01", # completed

    "t126f01", # completed

    "t127f01", # completed

    "t128f01", # completed

    "t129f01", # completed

    "t130f01", # completed

    "t131f01", # completed

    "t132f01", # completed

    "t133f01", # completed

    "t134f01", # completed

    "t135f01", # completed

    "t136f01", # completed

    "t137f01", # completed

    "t138f01", # completed

    "t139f01", # completed

    "t140f01", # completed

    "t141f01", # completed

    "t142f01", # completed

    "t143f01", # completed

    "t144f01", # completed

    "t145f01", # completed

    "t146f01", # completed
    "t146f02", # completed

    "t147f01", # completed

    "t148f01", # completed

    "t149f01", # completed

    "t150f01", # completed
    "t150f02", # completed

    "t151f01", # completed
    "t151f02", # completed

    "t152f01", # completed

    "t153f01", # completed

    "t154f01", # completed

    "t155f01", # completed
    "t155f02", # completed

    "t156f01", # completed
    "t156f02", # completed
    "t156f03") # completed

# Import Data
importdata <- function(filenames) {
    # imports data from excel worksheet at location of the form:
    for (i in seq_len(length(filenames))) {
        filename <- filenames[i]
        test <- substr(filename, 2, 4)
        flight <- substr(filename, 6, 8)
        location <- paste0(
            "../data/test-",
            test, "/", flight, "/", filename, ".xlsx")
        assign(
            filename,
            read_excel(location, col_types = "text"), envir = .GlobalEnv)
    }
}
    
importdata(filenames)
    
#### Merge and Save ####
# Merge Data
data <- bind_rows(mget(filenames))
    
# Save Data
write.csv(
    data,
    "../data/shorebird-disturbance.csv",
    row.names = FALSE)
