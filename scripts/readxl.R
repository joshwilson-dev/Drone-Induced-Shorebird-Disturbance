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
    "t003f00", # complete
    "t003f01", # complete
    "t003f02", # complete
    "t003f03", # complete
    "t003f04", # complete
    "t003f05", # complete
    "t003f06", # complete
    "t003f07", # complete
    # "t003f08", # direct descent

    "t004f00", # complete
    "t004f01", # complete
    "t004f02", # complete
    "t004f03", # complete
    "t004f04", # complete
    "t004f05", # complete
    "t004f06", # complete
    "t004f07", # complete
    # "t004f08", # direct descent

    "t005f00", # complete
    "t005f01", # complete
    "t005f02", # complete
    "t005f03", # complete
    "t005f04", # complete
    "t005f05", # complete
    "t005f06", # complete
    "t005f07", # complete
    # "t005f08", # direct descent

    "t006f01", # complete
    "t006f02", # complete
    "t006f03", # complete
    "t006f04", # complete
    "t006f05", # complete
    "t006f06", # complete
    "t006f07", # complete

    "t007f00", # complete
    "t007f01", # complete
    "t007f02", # complete
    "t007f03", # complete
    "t007f04", # complete
    "t007f05", # complete
    "t007f06", # complete

    "t008f00", # complete
    "t008f01", # complete
    "t008f02", # complete
    "t008f03", # complete
    "t008f04", # complete
    "t008f05", # complete
    "t008f06", # complete

    "t009f00", # complete
    "t009f01", # complete
    "t009f02", # complete
    "t009f03", # complete
    "t009f04", # complete
    "t009f05", # complete
    "t009f06", # complete

    "t010f00", # complete
    "t010f01", # complete
    "t010f02", # complete
    "t010f03", # complete
    "t010f04", # complete

    "t011f00", # complete
    "t011f01", # complete
    "t011f02", # complete
    "t011f03", # complete
    "t011f04", # complete

    "t012f00", # complete
    "t012f01", # complete
    "t012f02", # complete
    "t012f03", # complete
    "t012f04", # complete
    "t012f05", # complete

    "t013f00", # complete
    "t013f01", # complete
    "t013f02", # complete
    "t013f03", # complete
    "t013f04", # complete
    "t013f05", # complete

    "t014f00", # complete
    "t014f01", # complete
    "t014f02", # complete
    "t014f03", # complete
    "t014f04", # complete
    "t014f05", # complete

    "t015f00", # complete
    "t015f01", # complete
    "t015f02", # complete
    "t015f03", # complete
    "t015f04", # complete
    "t015f05", # complete
    "t015f06", # complete

    "t016f00", # complete
    "t016f01", # complete
    "t016f02", # complete
    "t016f03", # complete
    "t016f04", # complete

    "t017f00", # complete
    "t017f01", # complete
    "t017f02", # complete
    "t017f03", # complete
    "t017f04", # complete
    "t017f05", # complete
    "t017f06", # complete

    "t018f00", # complete
    "t018f00", # complete

    "t019f00", # complete
    "t019f01", # complete
    "t019f02", # complete
    "t019f03", # complete
    "t019f04", # complete
    "t019f05", # complete
    "t019f06", # complete

    "t020f00", # complete
    "t020f01", # complete
    "t020f02", # complete
    "t020f03", # complete
    "t020f04", # complete

    "t021f00", # complete
    "t021f01", # complete
    "t021f02", # complete
    "t021f03", # complete
    "t021f04", # complete
    "t021f05", # complete
    "t021f06", # complete

    "t022f00", # complete
    "t022f01", # complete
    "t022f02", # complete
    "t022f03", # complete
    "t022f04", # complete
    "t022f05", # complete
    "t022f06", # complete

    "t023f00", # complete

    "t024f00", # complete
    "t024f01", # complete
    "t024f02", # complete
    "t024f03", # complete
    "t024f04", # complete
    "t024f05", # complete
    "t024f06", # complete
    "t024f07", # complete

    "t025f00", # complete
    "t025f01", # complete
    "t025f02", # complete
    "t025f03", # complete
    "t025f04", # complete
    "t025f05", # complete
    "t025f06", # complete

    "t026f00", # complete
    "t026f01", # complete
    "t026f02", # complete
    "t026f03", # complete
    "t026f04", # complete
    "t026f05", # complete
    "t026f06", # complete

    "t027f00", # complete
    "t027f01", # complete
    "t027f02", # complete
    "t027f03", # complete
    "t027f04", # complete
    "t027f05", # complete
    "t027f06", # complete

    "t028f00", # complete
    "t028f01", # complete
    "t028f02", # complete
    "t028f03", # complete
    "t028f04", # complete
    "t028f05", # complete
    "t028f06", # complete
    "t028f07", # complete

    "t029f00", # complete
    "t029f01", # complete
    "t029f02", # complete
    "t029f03", # complete
    "t029f04", # complete
    "t029f05", # complete
    "t029f06", # complete
    "t029f07", # complete

    "t030f00", # complete
    "t030f01", # complete
    "t030f02", # complete
    "t030f03", # complete
    "t030f04", # complete
    "t030f05", # complete
    "t030f06", # complete
    "t030f07", # complete

    "t031f00", # complete
    "t031f01", # complete
    "t031f02", # complete
    "t031f03", # complete
    "t031f04", # complete
    "t031f05", # complete
    "t031f06", # complete

    "t032f00", # no count
    "t032f01", # no count
    "t032f02", # no count
    "t032f03", # no count
    "t032f04", # no count
    "t032f05", # no count
    "t032f06", # no count
    "t032f07", # no count

    "t033f00", # no count
    "t033f01", # no count
    "t033f02", # no count
    "t033f03", # no count
    "t033f04", # no count
    "t033f05", # no count
    "t033f06", # no count
    "t033f07", # no count

    # INSPIRE 2

    "t034f00", # no count unknown bird
    "t034f01", # no count unknown bird

    "t035f00", # no count
    "t035f01", # no count

    "t036f00", # no count
    "t036f01", # no count

    "t037f00", # no count
    "t037f01", # no count

    "t038f00", # no count
    "t038f01", # no count

    "t039f00", # no count
    "t039f01", # no count

    "t040f00", # no count
    "t040f01", # no count

    "t041f00", # no count
    "t041f01", # no count

    "t042f00", # no count
    "t042f01", # no count

    "t043f00", # no count
    "t043f01", # no count

    "t044f00", # no count
    "t044f01", # no count

    "t045f00", # no count
    "t045f01", # no count
    "t045f02", # no count

    "t046f00", # no count
    "t046f01", # no count
    "t046f02", # no count

    "t047f00", # no count
    "t047f01", # no count

    "t048f00", # no count
    "t048f01", # no count
    "t048f02", # no count

    "t049f00", # no count
    "t049f01", # no count

    "t050f00", # no count
    "t050f01", # no count

    "t051f00", # no count
    "t051f01", # no count

    "t052f00", # no count
    "t052f01", # no count

    "t053f00", # no count
    "t053f01", # no count

    "t054f00", # no count
    "t054f01", # no count

    "t056f00", # no count
    "t056f01", # no count

    "t057f00", # no count
    "t057f01", # no count
    "t057f02", # no count
    "t057f03", # no count
    "t057f04", # no count
    "t057f05", # no count
    "t057f06", # no count
    "t057f07", # no count
    "t057f08", # no count
    "t057f09", # no count

    "t058f00", # no count
    "t058f01", # no count
    "t058f02", # no count

    "t059f00", # no count
    "t059f01", # no count

    "t060f00", # no count
    "t060f01", # no count
    "t060f02", # no count
    "t060f03", # no count
    "t060f04", # no count
    "t060f05", # no count

    "t061f00", # no count
    "t061f01", # no count
    "t061f02", # no count
    "t061f03", # no count

    "t062f00", # no count
    "t062f01", # no count
    "t062f02", # no count
    "t062f03", # no count
    "t062f04", # no count

    "t063f00", # no count
    "t063f01", # no count

    "t064f00", # no count
    "t064f01", # no count

    "t065f01", # no count
    "t065f02", # no count

    "t066f00", # no count
    "t066f01", # no count
    "t066f02", # no count

    "t067f00", # complete
    "t067f01", # complete
    "t067f02", # complete
    "t067f03", # complete
    "t067f04", # complete
    "t067f05", # complete
    "t067f06", # complete

    "t068f00", # complete
    "t068f01", # complete
    "t068f02", # complete
    "t068f03", # complete
    "t068f04", # complete

    "t069f00", # no count
    "t069f01", # no count
    "t069f02", # no count
    "t069f03", # no count
    "t069f04", # no count
    "t069f05", # no count

    # MAVIC 2 PRO

    "t070f01", # no count
    "t070f02", # no count
    "t070f03", # no count

    "t071f01", # no count
    "t071f02", # no count

    "t072f01", # no count
    "t072f02", # no count
    "t072f03", # no count
    "t072f04", # no count

    "t073f01", # no count
    "t073f02", # no count
    "t073f03", # no count
    "t073f04", # no count

    "t074f01", # no count

    "t075f01", # no count
    "t075f02", # no count
    "t075f03", # no count
    "t075f04", # no count
    "t075f05", # no count

    "t076f01", # no count
    "t076f02", # no count
    "t076f03", # no count
    "t076f04", # no count
    "t076f05", # no count

    "t077f01", # no count
    "t077f02", # no count
    "t077f03", # no count
    "t077f04", # no count

    "t078f01", # no count
    "t078f02", # no count
    "t078f03", # no count
    "t078f04", # no count
    "t078f05", # no count

    "t079f01", # no count
    "t079f02", # no count
    "t079f03", # no count
    "t079f04", # no count

    "t080f01", # no count
    "t080f02", # no count
    "t080f03", # no count
    "t080f04", # no count
    "t080f05", # no count

    "t081f01", # no count
    "t081f02", # no count
    "t081f03", # no count
    "t081f04", # no count
    "t081f05", # no count
    "t081f06", # no count

    "t082f01", # completed
    "t082f02", # completed
    "t082f03", # completed

    "t083f01", # no count

    "t084f01", # no count

    "t085f01", # no count

    "t086f01", # no count

    "t087f01", # no count
    "t087f02", # no count

    "t088f01", # no count

    "t089f01", # completed

    "t090f01", # completed

    "t091f01", # completed revise count

    "t092f01", # completed revise count

    "t093f01", # completed

    "t094f01", # completed

    "t095f01", # completed

    "t096f01", # completed

    "t097f01", # completed

    "t098f01", # completed

    "t099f01", # completed

    # "t100f01", # spibis

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

    "t117f01", # completed

    "t118f01", # completed

    "t119f01", # completed

    "t120f01", # completed

    "t121f01", # completed

    "t122f01", # completed

    "t123f01", # completed

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
