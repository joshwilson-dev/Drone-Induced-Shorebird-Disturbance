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
                        `tide type` = "c",
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
                        `moon description` = "c",
                        `moon visibility (%)` = "n",
                        `drone` = "c",
                        `approach type` = "c",
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
                        `flycState` = "c",
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
      
    # seperate datetime into date and time
    mutate(`date(aest)` = as.Date(`datetime(aest)`), .after = `datetime(aest)`) %>% 
    mutate(`time(aest)` = format(`datetime(aest)`,"%H:%M:%OS"), .after = `date(aest)`) %>% 
    
    # add flightcode to group by
    mutate(flightcode = paste0(as.character(test), as.character(flight))) %>%
  
    # group by flight code
    group_by(flightcode) %>%
    
    # add a column with distance between drone and birds
    mutate(dronedistance = 6371009 * sqrt(((pi/180)*(`bird lat` - latitude))^2 + ((cos((pi/180)*(`bird lat` + latitude)/2)*(pi/180)*(`bird long` - longitude)))^2)
           ,.after = `time(aest)`) %>%
      
    # add a column with the bearing between the drone and the birds
    mutate(`bearing(deg)` = (180/pi)*(atan2(abs((pi/180)*longitude - (pi/180)*`bird long`), log(tan((pi/180)*`bird lat`/2 + pi/4)/tan((pi/180)*latitude/2 + pi/4))))
           ,.after = `time(aest)`) %>% 
    
    # add a column with the direction of travel in drone coordinates
    mutate(`rel.direction.of.travel(deg)` = atan(`ySpeed(m/s)`/`xSpeed(m/s)`)*180/pi
           ,.after = `time(aest)`) %>%
    
    # add a column with the direction of travel in global coordinates
    mutate(`gbl.direction.of.travel(deg)` = case_when((`xSpeed(m/s)` < 0) ~ (`compass_heading(degrees)` + `rel.direction.of.travel(deg)`)%%360,
                                                      (`xSpeed(m/s)` >= 0) ~ (`compass_heading(degrees)` + 180 + `rel.direction.of.travel(deg)`)%%360,
                                                      TRUE ~ `compass_heading(degrees)`), .after = `time(aest)`) %>%
    
    # add a column for the angle between direction of travel and bearing to birds
    mutate(`horizontal.angle.of.approach(deg)` = abs(`bearing(deg)` - `gbl.direction.of.travel(deg)`)
           ,.after = `time(aest)`) %>%
    mutate(`horizontal.angle.of.approach(deg)` = case_when(`horizontal.angle.of.approach(deg)` > 180 ~ 360 - `horizontal.angle.of.approach(deg)`,
                                                           TRUE ~ `horizontal.angle.of.approach(deg)`)
           ,.after = `time(aest)`) %>%
    
    # combine x and y drone speed into horizontal speed
    mutate(`horizontalspeed(m/s)` = sqrt(`xSpeed(m/s)`^2 + `ySpeed(m/s)`^2)
           ,.after = `time(aest)`) %>% #squaring the terms gets rid of +ve or -ve
    
    # rename z speed for clarity
    mutate(`verticalspeed(m/s)` = `zSpeed(m/s)`
           ,.after = `time(aest)`) %>%
  
    # specify approach type based on drone behaviour
    mutate(`approach type` = case_when((is.na(`verticalspeed(m/s)`) & is.na(`horizontalspeed(m/s)`)) ~ 'control',
                                       (`max_distance(meters)` != max(`max_distance(meters)`, na.rm=T) & `verticalspeed(m/s)` < 0 & `horizontalspeed(m/s)` <= 0.5) ~ 'ascending',
                                       (flycStateRaw == 41 | flycStateRaw == 10 ~ 'ascending'),
                                       (TRUE ~ 'advancing')),.after = `time(aest)`) %>%
    
    # add column for eastern curlew presence
    mutate(`eastern.curlew.presence` = as.factor(case_when((is.na(`eastern curlew behaviour`)) ~ FALSE,
                                     (TRUE ~ TRUE)))) %>% 
      
    # degrade data into first instance where behaviour state changes for a species
    group_by_at(vars(ends_with("behaviour")), .add = TRUE) %>%
    slice(1) %>%
    
    # pivot long so that each species is on a different row
    pivot_longer(cols = ends_with("behaviour"),
        names_to = "species",
        names_pattern = "(.*).behaviour",
        values_to = "behaviour",
        values_drop_na = TRUE) %>%
    mutate(species = as.factor(species)) %>% 
      
    # make column names correct format for gam
    rename_all(make.names)
    
#### simplified mavic 2 pro advancing analysis ####
    
    # degrade data to one entry to column and filter for just mavic 2 pro disturbance
    data_m2p_dima <- data_aug %>%
      
      # Remove all drones except mavic 2 pro
      # filter(drone == 'mavic 2 pro') %>% 
      
      # only retain disturbance manoeuvre flights
      filter(approach.type == 'advancing') %>%
      
      # remove all data from when the birds didn't take flight
      filter(behaviour == 'flight') %>% 
      
      # remove flights with eastern curlew present for other species
      filter(!(species != "eastern curlew" & eastern.curlew.presence == TRUE))
    
    
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
    # 
    
    m2p_dima_gam <- gam(dronedistance ~
                          drone +
                          species +
                          s(height_above_takeoff.meters.),
                          # eastern.curlew.presence +
                          # behaviour +
                          # s(height_above_takeoff.meters., by = species) +
                          # s(height_above_takeoff.meters., by = behaviour),
                        data = data_m2p_dima,
                        family = 'gaussian',
                        method = 'REML')

    summary(m2p_dima_gam)
    gam.check(m2p_dima_gam, rep = 500)
    
    # Visualising Results
    visreg(m2p_dima_gam,
           gg = TRUE,
           xlab = 'Flight Altitude Above Birds (m)',
           ylab = 'Horizontal Distance Between Birds & Drone (m)')
    
    m2p_dima_gam_vis <- visreg(m2p_dima_gam,
           "height_above_takeoff.meters.",
           by = "behaviour",
           gg = TRUE,
           xlab = 'Flight Altitude Above Birds (m)',
           ylab = 'Horizontal Distance Between Birds & Drone (m)',
           plot=FALSE)
    
    ggplot() +
      theme_set(theme_bw()) +
      geom_ribbon(
        data = m2p_dima_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            ymin=visregLwr,
            ymax=visregUpr,
            group = behaviour,
            colour = behaviour,
            fill = behaviour),
        alpha=0.1) +
      geom_line(
        data = m2p_dima_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            visregFit,
            group = behaviour,
            colour = behaviour),
        size = 1.2) +
      geom_point(
        data = m2p_dima_gam_vis$res,
        aes(height_above_takeoff.meters.,
            visregRes,
            group = behaviour,
            colour = behaviour)) +
      coord_flip(ylim=c(-350, 350),
                 xlim=c(0, 120)) +
      geom_ribbon(
        data = m2p_dima_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            ymin=-visregLwr,
            ymax=-visregUpr,
            group = behaviour,
            colour = behaviour,
            fill = behaviour),
        alpha=0.1) +
      geom_line(
        data = m2p_dima_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            -visregFit,
            group = behaviour,
            colour = behaviour),
        size = 1.2) +
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
      scale_x_continuous(minor_breaks = round(seq(20, 120, 20)),
                         breaks = round(seq(20, 120, by = 20),1),
                         expand = c(0.01, 0)) +
      scale_y_continuous(minor_breaks = round(seq(-400, 400, 20)),
                         breaks = round(seq(-400, 400, by = 20),1),
                         expand = c(0.01, 0))
    
#### simplified mavic 2 pro takeoff analysis ####

  # degrade data to one entry to column and filter for just mavic 2 pro takeoff
  data_m2p_tkof <- data_aug %>%
      
      # Remove all drones except mavic
      filter(drone == 'mavic 2 pro') %>%
      
      # filter out disturbance manouevre flights
      filter(approach.type == 'takeoff') %>%
      
      # if the birds took flight, remove all the non-flight data from that group so we can do the next step
      filter(case_when(any(eastern.curlew.behaviour == 1) ~ eastern.curlew.behaviour > 0 , TRUE ~ eastern.curlew.behaviour == 0)) %>% 
      
      # take the first entry
      slice(1)

  # Initial Visualisation
  ggplot(data = data_m2p_tkof, aes(dronedistance, eastern.curlew.behaviour)) +
    geom_point() +
    geom_smooth(method='gam')

  # Fit GAM
  m2p_tkof_gam <- gam(eastern.curlew.behaviour ~ s(dronedistance),
                      data = data_m2p_tkof,
                      family = 'gaussian',
                      method = 'REML')
  summary(m2p_tkof_gam)
  gam.check(m2p_tkof_gam, rep = 500)

  # Visualising Results
    # GAM results
    visreg(m2p_tkof_gam, partial = TRUE)
    
    # What was the drone doing when the birds took flight?
    data_m2p_tkof_dist = filter(data_m2p_tkof, eastern.curlew.behaviour == 1)

    height = data_m2p_tkof_dist$height_above_takeoff.meters.
    distance = data_m2p_tkof_dist$dronedistance
    n_bins = 5
    
    hist(height)
    
    ##  Create cuts:
    height_bins <- cut(height, n_bins)
    distance_bins <- cut(distance, n_bins)
    
    ##  Calculate joint counts at cut levels:
    bins <- table(height_bins, distance_bins)
    
    ##  Plot as a 3D histogram:
    hist3D(x = seq(min(height), max(height), length.out = n_bins),
           y = seq(min(distance), max(distance), length.out = n_bins),
           z = bins,
           border="black",
           alpha = 0.45,
           ticktype = "detailed",
           xlab = "Height",
           ylab = "Distance",
           zlab = "Frequency")
    
    # Was there short term habitation?
    
    

#### simplified inspire 2 takeoff analysis ####
    
  # degrade data to one entry to column and filter for just inspire 2 takeoff
  data_in2_tkof <- data_aug %>%
    
    # Remove all drones except inspire 2
    filter(drone == 'inspire 2') %>% 
     
    # filter out disturbance manouevre flights
    filter(approach.type == 'takeoff') %>%
    
    # if the birds took flight, remove all the non-flight data from that group so we can do the next step
    filter(case_when(any(eastern.curlew.behaviour == 1) ~ eastern.curlew.behaviour > 0 , TRUE ~ eastern.curlew.behaviour == 0)) %>% 
    
    # take the first entry
    slice(1)
    
  # Initial Visualisation
  ggplot(data = data_in2_tkof, aes(dronedistance, eastern.curlew.behaviour)) +
    geom_point() +
    geom_smooth(method='gam')
    
  # Fit GAM
  in2_tkof_gam <- gam(eastern.curlew.behaviour ~ s(dronedistance, k = 40),
                      data = data_in2_tkof,
                      family = 'gaussian',
                      method = 'REML')
  summary(in2_tkof_gam)
  gam.check(in2_tkof_gam, rep = 500)
  
  # Visualising Results
  visreg(in2_tkof_gam, partial = TRUE)
  
  # What was the drone doing when the birds took flight?
  data_in2_tkof_dist = filter(data_in2_tkof, eastern.curlew.behaviour == 1 && flightcode != 695)
  height = data_in2_tkof_dist$height_above_takeoff.meters.
  distance = data_in2_tkof_dist$dronedistance
  n_bins = 5
  
  hist(height)
  
  ##  Create cuts:
  height_bins <- cut(height, n_bins)
  distance_bins <- cut(distance, n_bins)
  
  ##  Calculate joint counts at cut levels:
  bins <- table(height_bins, distance_bins)
  
  ##  Plot as a 3D histogram:
  hist3D(x = seq(min(height), max(height), length.out = n_bins),
         y = seq(min(distance), max(distance), length.out = n_bins),
         z = bins,
         border="black",
         alpha = 0.45,
         ticktype = "detailed",
         xlab = "Height",
         ylab = "Distance",
         zlab = "Frequency")
    
#### simplified mavic mini disturbance manoeuvre analysis ####
  
  # degrade data to one entry to column and filter for just mavic mini disturbance
  data_min_dima <- data_aug %>%
    
    # Remove all drones except inspire 2
    filter(drone == 'mavic mini') %>% 
    
    # filter out disturbance manouevre flights
    filter(approach.type == 'disturbance manoeuvre') %>% 
    
    # take only closest distance between birds and drone because approaches stopped when birds took flight
    slice(which.min(dronedistance)) %>% 
    
    # remove flight with eagles as we never began approach
    filter(flightcode != 941)
  
  
  # Initial Visualisation
  ggplot(data = data_min_dima, aes(height_above_takeoff.meters., dronedistance)) +
    geom_point() +
    geom_smooth(method='gam')
  
  # Fit GAM
  min_dima_gam <- gam(dronedistance ~ s(height_above_takeoff.meters.),
                      data = data_min_dima,
                      family = 'gaussian',
                      method = 'REML')
  summary(min_dima_gam)
  gam.check(min_dima_gam, rep = 500)
  
  # Visualising Results
  visreg(min_dima_gam, partial = TRUE)

#### simplified mavic 2 pro holistic analysis didn't work ####

  data_m2p <- data_aug %>%
    
    # Add column to tell us if flightcode group involves a bird takeoff
    mutate(birdflight = case_when(any(eastern.curlew.behaviour == 1) ~ 1 , TRUE ~ 0)) %>%
    
    # slice off all data after first disturbance
    filter(eastern.curlew.behaviour < 1 & drone != 'mavic 2 pro landed' & drone != 'inspire 2 landed' & drone != 'mavic mini landed') %>% 
    
    # if the flight contained a disturbance, set the last 3 seconds to disturbance
    mutate(eastern.curlew.behaviour.mod = case_when(row_number() >= (n() - 30) & birdflight == 1 ~ 1, TRUE ~ 0)) %>%
    
    # degrade date so we only keep every 10th row
    
    slice(which(row_number() %% 10 == 1)) %>% 
    
    # filter out controls and other drones for now
    filter(drone == 'mavic 2 pro' & approach.type == 'disturbance manoeuvre')
  
  # Fit GAM
  gam <- gam(eastern.curlew.behaviour.mod ~
               # te(height_above_takeoff.meters., horizontalspeed.m.s.) +
               te(height_above_takeoff.meters., dronedistance),
               # te(dronedistance, horizontalspeed.m.s.),
                 data = data_m2p,
                 family = 'gaussian',
                 method = 'REML')
  summary(gam)
  gam.check(gam, rep = 500)
  
  # Visualising Results
  par(mfrow=c(2, 2))
  visreg(gam, partial = TRUE)
  
  # prediction
  
  pdata <- with(data_m2p,
                expand.grid(horizontalspeed.m.s. = 5,
                            verticalspeed.m.s. = 2,
                            height_above_takeoff.meters. = seq(min(0), max(120), length = 100)
                            )
                )
  fit <- predict(m2p_gam, pdata)
  
  # ind <- exclude.too.far(pdata$height_above_takeoff.meters., data_m2p$height_above_takeoff.meters., dist = 10)
  # fit[ind] <- NA
  
  pred <- cbind(pdata, Fitted = fit)
  
  plt <- ggplot(pred, aes(x = height_above_takeoff.meters., y = Fitted)) +
    geom_point()
  plt
  
#### simplified disturbance manoeuvre analysis ####
  # degrade data to one entry to column and filter for just mavic 2 pro disturbance
  data_dima <- data_aug %>%
    
    # Remove all drones except inspire 2
    filter(drone == 'mavic 2 pro' | drone == 'mavic mini') %>% 
    
    # filter out disturbance manouevre flights
    filter(approach.type == 'disturbance manoeuvre') %>%
    
    mutate(drone = as.factor(drone)) %>% 
    
    # if the birds took flight, remove all the non-flight data from that group so we can do the next step
    filter(case_when(any(eastern.curlew.behaviour == 1) ~ eastern.curlew.behaviour > 0 , TRUE ~ eastern.curlew.behaviour == 0)) %>%
    
    # take only closest distance between birds and drone because for disturbance manoeuvres approaches stopped when birds took flight
    slice(which.min(dronedistance))
  
  head(data_dima$drone)
  
  # Fit GAM
  dima_gam <- gam(dronedistance ~ drone + s(height_above_takeoff.meters., by = drone),
                      data = data_dima,
                      family = 'gaussian',
                      method = 'REML')
  summary(dima_gam)
  gam.check(dima_gam, rep = 500)
  
  # Visualising Results
  visreg(dima_gam, partial = TRUE)
  
  

#### PLAYING AROUND ####
  
  data_play <- data_aug %>%
    
    # Remove all drones except inspire 2
    filter(flightcode == 1361)
    
  
    