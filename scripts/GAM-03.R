# Shorebird Disturbance Analysis
  # Josh Wilson
    # 08-08-2021

#### Setup ####
  # Install Packages
    # install.packages("tidyverse")
    # install.packages("mgcv")
    # install.packages("visreg")

  # Import Packages
    library(tidyverse)
    library(mgcv)
    library(visreg)

  # Clear Environment
    rm(list=ls())

  # Import Data
    data <- read_csv("data/shorebird-disturbance-06.csv", guess_max = 1000000)

#### general data augmentation ####

    data_aug <- data %>%
      
      # add flightcode to group by
      mutate(flightcode = paste0(as.character(test), as.character(flight))) %>%
  
      # group by flight code
      group_by(flightcode) %>%
      
      # add a column with distance between drone and birds
      mutate(dronebirddistance = 6371009 * sqrt(((pi/180)*(`bird lat` - latitude))^2 + ((cos((pi/180)*(`bird lat` + latitude)/2)*(pi/180)*(`bird long` - longitude)))^2)) %>%
      
      # add column for eastern curlew presence
      mutate(`eastern.curlew.presence` = as.factor(case_when((is.na(`eastern curlew behaviour`)) ~ FALSE,
      (TRUE ~ TRUE)))) %>%

      # pivot long so that each species is on a different row
      pivot_longer(cols = ends_with("behaviour") | ends_with("count"),
                   names_to = c("species", ".value"),
                   names_pattern = "(.+) (.+)",
                   names_transform = list(species = as.factor),
                   values_drop_na = TRUE) %>%

      # convert to binary
      mutate(behaviour = case_when(behaviour == "nominal" ~ 0,
                                   behaviour == "flight" ~ 1,
                                   TRUE ~ 2)) %>%
      
      # degrade data into first instance where behaviour state changes
      mutate(behaviour = factor(behaviour, levels = c(0, 1, 2), ordered=TRUE)) %>%
      group_by(flightcode, species, behaviour, `approach type`) %>%
      filter(behaviour == max(behaviour)) %>% 
      slice(1) %>% 
      
      # make column names correct format for gam
      rename_all(make.names)
    
#### FIA analysis ####
    data_FIA <- data_aug %>%
      
      # keep only flights where the drone was advancing
      filter(approach.type == 'advancing') %>%

      # drop the landed behaviour state
      filter(behaviour != 2) %>% 
    
      # keep only the maximum behaviour state
      mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered=TRUE)) %>%
      group_by(flightcode, species) %>%
      filter(behaviour == max(behaviour)) %>% 
      
      # filter out species without enough data
      group_by(species) %>%
      filter(n() > 25) %>% 
      filter(drone != "inspire 2", drone != "phatom 4 pro")
    
      # filter out species when eastern curlew was present
      # filter(case_when(species=="eastern curlew" ~  eastern.curlew.presence == TRUE,
      #                  TRUE ~ eastern.curlew.presence == FALSE))
      

    
    # Fit GAM
    
    # behaviour ~ drone + species + altitude
    # 
    # Main Effects
    # Question: at what altitude does flight not occur?
    
    # Main Effects
    # altitude (does the behaviour vary with drone altitude?)
    # species (does the behaviour vary with species)
    # drone (does the behaviour vary with drone type?)
    # eastern curlew presence (does the behaviour vary with the presence of eastern curlew)
    
    # Interactions
    # altitude - species (does the relationship between altitude and behaviour vary between species)
    # altitude - drone (does the relationship between altitude and behaviour depend on the drone)
    
    
    
    gam_FIA <- gam(behaviour 
                   ~ species
                   + drone
                   + s(height_above_takeoff.meters.)
                   + eastern.curlew.presence,
                   # + s(wind.speed..m.s.)
                   # + s(tide.height..m., k = 9)
                   # + s(temperature..degrees.celcius.),
                   data = data_FIA,
                   family = 'binomial',
                   method = 'REML',
                   select = T)
    
    summary(gam_FIA)
    AIC(gam_FIA)
    gam.check(gam_FIA, rep = 500)
    
    # Visualising Results
    
    height_above_takeoff.meters. = seq(0, 120, by=1)
    species = c("black winged stilt", "bar tailed godwit", "aus")
    drone = c("mavic 2 pro", "phantom 4 pro")
    eastern.curlew.presence = c(TRUE, FALSE)
    
    new_data = expand.grid(height_above_takeoff.meters. = height_above_takeoff.meters., species = species, drone = drone, eastern.curlew.presence = eastern.curlew.presence)
    pred <- predict.gam(gam_FIA, new_data, trans=binomial()$linkinv, type = "response")
    
    FIA_results <- new_data %>% 
      mutate(prediction = pred)
    
    ggplot() +
      theme_set(theme_bw()) +
      geom_point(
        data = FIA_results,
        aes(height_above_takeoff.meters.,
            prediction,
            group = species,
            colour = species)) +
      ylim(0, 1)
    
    FIA_gam_vis <- visreg(gam_FIA,
                          "height_above_takeoff.meters.",
                          trans=binomial()$linkinv,
                          gg = TRUE,
                          xlab = 'Flight Altitude Above Birds (m)',
                          ylab = 'Horizontal Distance Between Birds & Drone (m)',
                          plot = TRUE)
#### FID analysis ####
    
    # degrade data to one entry to column and filter for just mavic 2 pro disturbance
    data_FID <- data_aug %>%
      
      # only retain disturbance manoeuvre flights
      filter(approach.type == 'advancing') %>%
      
      # remove species without enough data 
      filter(species == "eastern curlew") %>% 
      
      # remove all except alarm and flight
      filter(behaviour == 1) #%>% 
      
      #remove all drones except mavic 2 pro
      # filter(drone == "mavic 2 pro")
    
    
    # Fit GAM
    
    # FID ~ drone + altitude + behaviour
    # 
    # Main Effects
    # Question: What is the flight initiation distance for eastern curlew?
    # drone (does the FID vary with drone type?)
    # altitude (does the FID vary with drone altitude?)

    gam_FID <- gam(dronebirddistance ~
                          drone +
                          s(height_above_takeoff.meters.),
                        data = data_FID,
                        family = 'gaussian',
                        method = 'REML')

    summary(gam_FID)
    # gam.check(gam_FID, rep = 500)
    
    # Visualising Results
    visreg(gam_FID,
           gg = TRUE,
           xlab = 'Flight Altitude Above Birds (m)',
           ylab = 'Horizontal Distance Between Birds & Drone (m)')
    
    FID_gam_vis <- visreg(gam_FID,
           "height_above_takeoff.meters.",
           by = "drone",
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
            group = drone,
            colour = drone,
            fill = drone),
        alpha=0.1) +
      geom_line(
        data = FID_gam_vis$fit,
        aes(height_above_takeoff.meters.,
            visregFit,
            group = drone,
            colour = drone),
        size = 1.2) +
      geom_point(
        data = FID_gam_vis$res,
        aes(height_above_takeoff.meters.,
            visregRes,
            group = drone,
            colour = drone)) +
      coord_flip(ylim=c(0, 350),
                 xlim=c(0, 120)) +
      labs(x = "Drone Altitude Above Birds (m)",
           y = "Horizontal Flight Initiation Distance (m)",
           fill = "Drone",
           colour = "Drone") +
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
    data_FIT <- data_aug %>%
      
      # keep only flights where the drone was advancing
      filter(approach.type == 'ascending') %>%
      
      # drop the landed behaviour state
      filter(behaviour != 2) %>% 
      
      # keep only the maximum behaviour state
      mutate(behaviour = factor(behaviour, levels = c(0, 1), ordered=TRUE)) %>%
      group_by(flightcode, species) %>%
      filter(behaviour == max(behaviour)) %>% 
      
      # filter out species when eastern curlew was present
      # filter(case_when(species=="eastern curlew" ~  eastern.curlew.presence == TRUE,
      #                  TRUE ~ eastern.curlew.presence == FALSE)) %>%
      
      # filter out species without enough data
      group_by(species) %>%
      filter(n() > 40) %>% 
      
      # to fix
      # drop_na(drone) %>% 
      # filter(drone != "mavic pro 2") %>%
      filter(flightcode != "311") %>% 
      filter(flightcode != "411")
    
    
    
    # Fit GAM
    
    # behaviour ~ drone + species + altitude
    # 
    # Main Effects
    # Question: at what altitude does flight not occur?
    # drone (does the behaviour vary with drone type?)
    # altitude (does the behaviour vary with drone altitude?)
    # species (does the behaviour vary with species)
    
    gam_FIT <- gam(behaviour 
                   ~ s(dronebirddistance)
                   + species
                   + drone
                   + eastern.curlew.presence,
                   data = data_FIT,
                   family = 'binomial',
                   method = 'REML')
    
    # hat <- data.frame(height_above_takeoff.meters. = (-200:600))
    # 
    # pred <- predict.gam(gam_FIA, hat, type = "response")
    # plot(pred)
    
    summary(gam_FIT)
    # gam.check(gam_FIT, rep = 500)
    
    # Visualising Results
    
    visreg(gam_FIT, trans=binomial()$linkinv, ylab="Probability")
    
    

#### PLAYING AROUND ####
  summary(data)
  data_play <- data_FIA %>%
    
    # Remove all drones except inspire 2
    filter(drone == "inspire 2") %>% 
    filter(behaviour == 0)
    # select(c(`count`, `test`)) %>% 
    # drop_na()
    
  
    