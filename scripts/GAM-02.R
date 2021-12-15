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

  # Import Data
    data <- read_csv("data/shorebird-disturbance-06.csv", guess_max = 500000)

#### general data augmentation ####

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
    
      # specify approach type based on drone parameters
      mutate(`approach type` = case_when((is.na(`verticalspeed(m/s)`) & is.na(`horizontalspeed(m/s)`) ~ 'control'),
                                         (`verticalspeed(m/s)` <= 0 ~ 'ascending'),
                                         (flycStateRaw == 41 | flycStateRaw == 10 | flycStateRaw == 14 ~ 'ascending'),
                                         (TRUE ~ 'advancing'))) %>%
      
      # add column for eastern curlew presence
      mutate(`eastern.curlew.presence` = as.factor(case_when((is.na(`eastern curlew behaviour`)) ~ FALSE,
      (TRUE ~ TRUE)))) %>%
      
      # pivot long so that each species is on a different row
      pivot_longer(cols = ends_with("behaviour") | ends_with("count"),
                   names_to = c("species", ".value"),
                   names_pattern = "(.+) (.+)",
                   names_transform = list(species = as.factor),
                   values_drop_na = TRUE) %>%
      
      # degrade data into first instance where behaviour state changes for a species
      filter(`approach type` != "control") %>% 
      group_by(flightcode, species, `approach type`, behaviour) %>%
      slice(1) %>%
      
      # make column names correct format for gam
      rename_all(make.names)
    
#### FIA analysis ####
    data_FIA <- data_aug %>%
      
      # keep only flights where the drone was advancing
      filter(approach.type == 'advancing') %>%
      
      # convert to binary
      mutate(behaviour = case_when(behaviour == "nominal" ~ 0,
                                   TRUE ~ 1)) %>% 
    
      # keep only the maximum behaviour state
      mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered=TRUE)) %>%
      group_by(flightcode, species) %>% 
      filter(behaviour == max(behaviour))
      

    
    # Fit GAM
    
    # behaviour ~ drone + species + altitude
    # 
    # Main Effects
    # Question: at what altitude does flight not occur?
    # drone (does the behaviour vary with drone type?)
    # altitude (does the behaviour vary with drone altitude?)
    # species (does the behaviour vary with species)
    
    gam_FIA <- gam(behaviour ~ s(height_above_takeoff.meters.),
                   data = data_FIA,
                   family = 'binomial',
                   method = 'REML')
    
    glm_FIA <- glm(behaviour ~ height_above_takeoff.meters.,
                   data = data_FIA,
                   family = 'binomial')
    
    hat <- data.frame(height_above_takeoff.meters. = (-200:600))
    
    pred <- predict.gam(gam_FIA, hat, type = "response")
    plot(pred)
    
    
    summary(gam_FIA)
    gam.check(gam_FIA, rep = 500)
    
    # Visualising Results
    visreg(gam_FIA,
           gg = TRUE)
    
    FID_gam_vis <- visreg(gam_FID,
                          "height_above_takeoff.meters.",
                          by = "behaviour",
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
            group = behaviour,
            colour = behaviour,
            fill = behaviour),
        alpha=0.1) +
      geom_line(
        data = FID_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            visregFit,
            group = behaviour,
            colour = behaviour),
        size = 1.2) +
      geom_point(
        data = FID_gam_vis$res,
        aes(height_above_takeoff.meters.,
            visregRes,
            group = behaviour,
            colour = behaviour)) +
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
      
      
      
    
#### FID analysis ####
    
    # degrade data to one entry to column and filter for just mavic 2 pro disturbance
    data_FID <- data_aug %>%
      
      # only retain disturbance manoeuvre flights
      filter(approach.type == 'advancing') %>%
      
      # remove species without enough data 
      filter(species == "eastern curlew") %>% 
      
      # remove all except alarm and flight
      filter(behaviour == "alarm" | behaviour == "flight")
    
    
    # Fit GAM
    
    # FID ~ drone + altitude + behaviour
    # 
    # Main Effects
    # Question: What is the flight initiation distance for eastern curlew?
    # drone (does the FID vary with drone type?)
    # altitude (does the FID vary with drone altitude?)
    # behaviour (at behaviour predicts the FID?)

    gam_FID <- gam(dronedistance ~
                          # drone +
                          behaviour +
                          s(height_above_takeoff.meters.),
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
           by = "behaviour",
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
            group = behaviour,
            colour = behaviour,
            fill = behaviour),
        alpha=0.1) +
      geom_line(
        data = FID_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            visregFit,
            group = behaviour,
            colour = behaviour),
        size = 1.2) +
      geom_point(
        data = FID_gam_vis$res,
        aes(height_above_takeoff.meters.,
            visregRes,
            group = behaviour,
            colour = behaviour)) +
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
  summary(data)
  data_play <- data %>%
    
    # Remove all drones except inspire 2
    mutate(flightcode = test) %>% 
    mutate(tim = `datetime(utc)`)
    # select(c(`count`, `test`)) %>% 
    # drop_na()
    
  
    