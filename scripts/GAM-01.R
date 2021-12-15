# Shorebird Disturbance ANalysis
  # Josh Wilson
    # 08-08-2021

#### Setup ####
  # Install Packages
    # install.packages("tidyverse")
    # install.packages("mgcv")
    # install.packages("visreg")
    # install.packages("plot3D")

  # Import Packages
    library(tidyverse)
    library(mgcv)
    library(visreg)

  # Clear Environment
    rm(list=ls())

#### importing data ####
  # specifying column types
    column_types = list(`datetime(aest)` = "T",
                        `test` = "n",
                        `flight` = "n",
                        `video time (seconds)` = "n",
                        `bird lat` = "n",
                        `bird long` = "n",
                        `tide height (m)` = "n",
                        `tide type` = "f",
                        `temperature (degrees celcius)` = "n",
                        `weather description` = "c",
                        `cloud cover (%)` = "n",
                        `visibility (km)` = "n",
                        `wind speed (m/s)` = "n",
                        `wind direction (degrees)` = "n",
                        `humidity (%)` = "n",
                        `feels like (degrees celcius)` = "n",
                        `dew point (degrees celcius)` = "n",
                        `pressure (hPa)` = "n",
                        `rain rate (mm/h)` = "n",
                        `chance (%)` = "n",
                        `sunrise` = "c",
                        `sunset` = "c",
                        `moon description` = "f",
                        `moon visibility (%)` = "n",
                        `drone` = "f",
                        `approach type` = "f",
                        `time(millisecond)` = "n",
                        `datetime(utc)` = "n",
                        `latitude` = "n",
                        `longitude` = "n",
                        `height_above_takeoff(meters)` = "n",
                        `height_above_ground_at_drone_location(meters)` = "c",
                        `ground_elevation_at_drone_location(meters)` = "c",
                        `altitude_above_seaLevel(meters)` = "n",
                        `height_sonar(meters)` = "n",
                        `speed(m/s)` = "n",
                        `distance(meters)` = "n",
                        `satellites` = "n",
                        `gpslevel` = "n",
                        `voltage(v)` = "n",
                        `max_altitude(meters)` = "n",
                        `max_ascent(meters)` = "n",
                        `max_speed(m/s)` = "n",
                        `max_distance(meters)` = "n",
                        `xSpeed(m/s)` = "n",
                        `ySpeed(m/s)` = "n",
                        `zSpeed(m/s)` = "n",
                        `compass_heading(degrees)` = "n",
                        `pitch(degrees)` = "n",
                        `roll(degrees)` = "n",
                        `isPhoto` = "n",
                        `isVideo` = "n",
                        `rc_elevator` = "n",
                        `rc_aileron` = "n",
                        `rc_throttle` = "n",
                        `rc_rudder` = "n",
                        `gimbal_heading(degrees)` = "n",
                        `gimbal_pitch(degrees)` = "n",
                        `battery_percent` = "n",
                        `voltageCell1` = "n",
                        `voltageCell2` = "n",
                        `voltageCell3` = "n",
                        `voltageCell4` = "n",
                        `voltageCell5` = "n",
                        `voltageCell6` = "n",
                        `current(A)` = "n",
                        `battery_temperature(c)` = "n",
                        `altitude(meters)` = "n",
                        `ascent(meters)` = "n",
                        `flycStateRaw` = "n",
                        `flycState` = "f",
                        `message` = "c",
                        `eastern curlew count` = "n",
                        `eastern curlew behaviour` = "f",
                        `gull billed tern count` = "n",
                        `gull billed tern behaviour` = "f",
                        `australian pied oystercatcher count` = "n",
                        `australian pied oystercatcher behaviour` = "f",
                        `grey tailed tattler count` = "n",
                        `grey tailed tattler behaviour` = "f",
                        `masked lapwing count` = "n",
                        `masked lapwing behaviour` = "f",
                        `terek sandpiper count` = "n",
                        `terek sandpiper behaviour` = "f",
                        `silver gull count` = "n",
                        `silver gull behaviour` = "f",
                        `caspian tern count` = "n",
                        `caspian tern behaviour` = "f",
                        `bar tailed godwit count` = "n",
                        `bar tailed godwit behaviour` = "f",
                        `australian white ibis count` = "n",
                        `australian white ibis behaviour` = "f",
                        `white faced heron count` = "n",
                        `white faced heron behaviour` = "f",
                        `great knot count` = "n",
                        `great knot behaviour` = "f",
                        `black winged stilt count` = "n",
                        `black winged stilt behaviour` = "f",
                        `whimbrel count` = "n",
                        `whimbrel behaviour` = "f",
                        `australian pelican count` = "n",
                        `australian pelican behaviour` = "f",
                        `common greenshank count` = "n",
                        `common greenshank behaviour` = "f",
                        `black swan count` = "n",
                        `black swan behaviour` = "f",
                        `royal spoonbill count` = "n",
                        `royal spoonbill behaviour` = "f",
                        `curlew sanpiper count` = "n",
                        `curlew sanpiper behaviour` = "f",
                        `intermediate egret count` = "n",
                        `intermediate egret behaviour` = "f")

  # Import Data
    data <- read_csv("data/shorebird-disturbance-06.csv", col_types=column_types)

#### general data mutations ####
    data_aug <- data %>%
      
      # add flightcode to group by
      mutate(flightcode = paste0(as.character(test), as.character(flight))) %>%
  
      # group by flight code
      group_by(flightcode) %>%
      
      # add a column with distance between drone and birds
      mutate(dronedistance = 6371009 * sqrt(((pi/180)*(`bird lat` - latitude))^2 + ((cos((pi/180)*(`bird lat` + latitude)/2)*(pi/180)*(`bird long` - longitude)))^2)) %>%
      
      # combine x and y drone speed into horizontal speed
      mutate(`horizontalspeed(m/s)` = sqrt(`xSpeed(m/s)`^2 + `ySpeed(m/s)`^2)) %>%
      
      # rename z speed for clarity
      mutate(`verticalspeed(m/s)` = `zSpeed(m/s)`) %>%
    
      # specify approach type based on drone behaviour
      mutate(`approach type` = case_when((is.na(`verticalspeed(m/s)`) & is.na(`horizontalspeed(m/s)`)) ~ 'control',
                                         (`max_distance(meters)` != max(`max_distance(meters)`, na.rm=T) & `verticalspeed(m/s)` < 0 & `horizontalspeed(m/s)` <= 0.5) ~ 'ascending',
                                         (flycStateRaw == 41 | flycStateRaw == 10 ~ 'ascending'),
                                         (TRUE ~ 'advancing'))) %>%
      
      # add column for eastern curlew presence
      mutate(`eastern.curlew.presence` = as.factor(case_when((is.na(`eastern curlew behaviour`)) ~ FALSE,
                                       (TRUE ~ TRUE)))) %>%
    
      # degrade data into first instance where behaviour state changes for a species
      group_by_at(vars(ends_with("behaviour")), .add = TRUE) %>%
      slice(1) %>%
      
      # pivot long so that each species is on a different row
      pivot_longer(cols = ends_with("behaviour") | ends_with("count"),
                   names_to = c("species", ".value"),
                   names_pattern = "(.+) (.+)",
                   names_transform = list(species = as.factor),
                   values_drop_na = TRUE) %>%
      
      # make column names correct format for gam
      rename_all(make.names)
#### FIA analysis ####
    data_FIA
    
#### FID analysis ####
    
    # degrade data to one entry to column and filter for just mavic 2 pro disturbance
    data_FID <- data_aug %>%
      
      # Remove all drones except mavic 2 pro
      filter(drone == 'mavic 2 pro' | drone == 'mavic mini') %>%
      
      # only retain disturbance manoeuvre flights
      filter(approach.type == 'advancing') %>%
      
      # remove all data from when the birds didn't take flight
      filter(behaviour == 'flight') %>% 
      
      # remove flights with eastern curlew present for other species
      filter(!(species != "eastern curlew" & eastern.curlew.presence == TRUE)) %>% 
      
      # remove species without enough data 
      filter(species == "eastern curlew" | species == "bar tailed godwit") 
      
      # remove outlier
      # filter(flightcode != 1501)
    
    
    # Fit GAM
    
    # FID ~ drone + species + altitude + behaviour
    # 
    # Main Effects
    # drone (does the FID vary with drone type?)
    # species (does the FID vary between species?)
    # altitude (does the FID vary with drone altitude?)
    # 
    # Interaction Effects
    # drone -> species (is there a differenc in the impact of drone type on FID between species? e.g. maybe different drones have a large impact on drone distance for curlew, but a small impact on drone distance for godwits)
    # altitude -> species (is there a difference in the impact of drone altitude on FID between species? e.g. maybe altitude has a big impact on drone distance for eastern curlew, but a small impact for bar-tailed godwit?)
    # drone -> altitude (is there a difference in the impact of drone type on FID between different drone altitudes? e.g. maybe drone type has a large impact on FID at high altitudes, but not at low altitudes)
    
    gam_FID <- gam(dronedistance ~
                          drone +
                          species +
                          s(height_above_takeoff.meters.) +
                          s(height_above_takeoff.meters., by = species),
                          # s(height_above_takeoff.meters., by = drone),
                        data = data_FID,
                        family = 'gaussian',
                        method = 'REML')

    summary(gam_FID)
    gam.check(gam_FID, rep = 500)
    
    # Visualising Results
    visreg(gam_FID,
           gg = TRUE,
           xlab = 'Flight Altitude Above Birds (m)',
           ylab = 'Horizontal Distance Between Birds & Drone (m)')
    
    FID_gam_vis <- visreg(gam_FID,
           "height_above_takeoff.meters.",
           by = "species",
           gg = TRUE,
           xlab = 'Flight Altitude Above Birds (m)',
           ylab = 'Horizontal Distance Between Birds & Drone (m)',
           plot=FALSE)
    
    ggplot() +
      theme_set(theme_bw()) +
      geom_ribbon(
        data = FID_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            ymin=visregLwr,
            ymax=visregUpr,
            group = species,
            colour = species,
            fill = species),
        alpha=0.1) +
      geom_line(
        data = FID_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            visregFit,
            group = species,
            colour = species),
        size = 1.2) +
      geom_point(
        data = FID_gam_vis$res,
        aes(height_above_takeoff.meters.,
            visregRes,
            group = species,
            colour = species)) +
      coord_flip(ylim=c(0, 350),
                 xlim=c(0, 120)) +
      labs(x = "Drone Altitude Above Birds (m)",
           y = "Horizontal Flight Initiation Distance (m)",
           fill = "Behaviour",
           colour = "Behaviour") +
      ggtitle("Mavic 2 Pro Induced Disturbance") +
      theme(
        axis.text = element_text(face="bold",
                                 color="black"),
        axis.title=element_text(size=14,
                                face="bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=16,
                                  face="bold",
                                  hjust = 0.5),
        legend.title=element_text(size=14,
                                  face="bold"),
        aspect.ratio = 0.35) +
      scale_x_continuous(minor_breaks = round(seq(0, 140, 20)),
                         breaks = round(seq(0, 140, by = 20),1),
                         expand = c(0.01, 0)) +
      scale_y_continuous(minor_breaks = round(seq(-400, 400, 20)),
                         breaks = round(seq(-400, 400, by = 20),1),
                         expand = c(0.01, 0))
    
#### takeoff analysis ####

#### PLAYING AROUND ####
  
  data_play <- data_aug %>%
    
    # Remove all drones except inspire 2
    filter(flightcode == 1361)
    
  
    