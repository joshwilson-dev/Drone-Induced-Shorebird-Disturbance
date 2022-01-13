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
    library(lubridate)

  # Clear Environment
    rm(list=ls())

  # Import Data
    data <- read_csv("../data/shorebird-disturbance.csv", guess_max = 1000000)

#### general data augmentation ####

    data_aug <- data %>%
      
      # add datetime in aest
      mutate(`datetime(aest)` = as_datetime(`datetime(utc)`*60*60*24, origin="1899/12/30 0:00:00", tz = "australia/queensland"))%>% 
      
      # add month factor
      
      mutate(month = month(`datetime(aest)`)) %>% 
      
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
    
    # Fit GAM
    
    # behaviour ~ species + altitude
    # 
    # Main Effects
    # Question: at what altitude does flight not occur?
    
    # Main Effects
    # altitude (does the behaviour vary with drone altitude?)
    # species (does the behaviour vary with species)
    # eastern curlew presence (does the behaviour vary with the presence of eastern curlew)
    
    # Interactions
    # altitude - species (does the relationship between altitude and behaviour vary between species)
    # altitude - drone (does the relationship between altitude and behaviour depend on the drone)
    
    
    
    gam_FIA <- gam(behaviour 
                   ~ species
                   + s(height_above_takeoff.meters.)
                   # + s(height_above_takeoff.meters., by = species)
                   + eastern.curlew.presence,
                   data = data_FIA,
                   family = 'binomial',
                   method = 'REML',
                   select = T)
    
    summary(gam_FIA)
    
    # Creating new data

    height_above_takeoff.meters.FIA = seq(0, 120, by=1)
    species_FIA = unique(data_FIA$species)
    eastern.curlew.presence.FIA = unique(data_FIA$eastern.curlew.presence)
    
    new_data_FIA = expand.grid(height_above_takeoff.meters. = height_above_takeoff.meters.FIA,
                           species = species_FIA,
                           eastern.curlew.presence = eastern.curlew.presence.FIA)
    
    pred_FIA <- predict.gam(gam_FIA,
                        new_data_FIA,
                        trans=binomial()$linkinv,
                        type = "response",
                        se.fit = TRUE)
    
    FIA_results <- new_data_FIA %>% 
      mutate(prediction = pred_FIA$fit) %>% 
      mutate(upper = pred_FIA$fit + (2 * pred_FIA$se.fit)) %>% 
      mutate(lower = pred_FIA$fit - (2 * pred_FIA$se.fit))
    
    # species sensitivity vs height above takeoff
    
    ggplot() +
      theme_set(theme_bw()) +
      geom_line(
        data = filter(FIA_results, eastern.curlew.presence == FALSE),
        aes(height_above_takeoff.meters.,
            prediction,
            group = species,
            colour = species),
        size = 1.2) +
      geom_ribbon(
        data = filter(FIA_results, eastern.curlew.presence == FALSE),
        aes(height_above_takeoff.meters.,
            ymin=lower,
            ymax=upper,
            group = species,
            colour = species,
            fill = species),
        alpha=0.05) +
      facet_wrap(~species) +
      geom_rug(aes(height_above_takeoff.meters.,
                   group = species,
                   colour = species),
               inherit.aes=FALSE, transform(filter(data_FIA, behaviour == 0), expl.name=height_above_takeoff.meters.),
               sides = "b") +
      geom_rug(aes(height_above_takeoff.meters.,
                   group = species,
                   colour = species),
               inherit.aes=FALSE, transform(filter(data_FIA, behaviour == 1), expl.name=height_above_takeoff.meters.),
               sides = "t") +
      labs(x = "Drone Altitude Above Birds (m)",
           y = "Probability of Inducing Bird Flight",
           fill = "Eastern Curlew Precense",
           colour = "Eastern Curlew Precense") +
      # ggtitle("Sub 2kg Drone Induced Shorebird Disturbance") +
      theme(axis.text = element_text(face="bold",
                                 color="black"),
        axis.title=element_text(size=14,
                                face="bold"),
        plot.title = element_text(size=16,
                                  face="bold",
                                  hjust = 0.5),
        legend.position = "none",
        strip.text.x = element_text(
          size = 10,
          face = "bold"))
    
    # species sensitivity vs eastern curlew presence
    ggplot() +
      theme_set(theme_bw()) +
      geom_rug(data = filter(data_FIA, behaviour == 0 & species != "eastern curlew" & height_above_takeoff.meters. > 40),
               aes(eastern.curlew.presence,
                   as.numeric(behaviour),
                   colour = eastern.curlew.presence),
               sides = "b",
               position = "jitter",
               size = 1) +
      geom_rug(data = filter(data_FIA, behaviour == 1 & species != "eastern curlew" & height_above_takeoff.meters. > 40),
               aes(eastern.curlew.presence,
                   as.numeric(behaviour),
                   colour = eastern.curlew.presence),
               sides = "t",
               position = "jitter",
               size = 1) +
    geom_boxplot(
      data = filter(FIA_results, species != "eastern curlew" & height_above_takeoff.meters. > 40),
      aes(eastern.curlew.presence,
          as.numeric(prediction),
          fill = eastern.curlew.presence),
      alpha=0.4) + 
      scale_y_continuous(limits = c(0, 1),
                         expand = c(0,0)) +
      labs(x = "Eastern Curlew Presence",
           y = "Probability of Inducing Bird Flight") +
      # ggtitle("Sub 2kg Drone Induced Shorebird Disturbance") +
      theme(axis.text = element_text(face="bold",
                                     color="black"),
            axis.title=element_text(size=14,
                                    face="bold"),
            plot.title = element_text(size=16,
                                      face="bold",
                                      hjust = 0.5),
            legend.position = "none")

#### FID analysis ####
    
    # degrade data to one entry to column and filter for just mavic 2 pro disturbance
    data_FID <- data_aug %>%
      
      # only retain disturbance manoeuvre flights
      filter(approach.type == 'advancing') %>%
      
      # remove species without enough data 
      filter(species == "eastern curlew") %>% 
      
      # remove all except alarm and flight
      filter(behaviour == 1) %>% 
      
      #remove all drones except mavic 2 pro
      filter(drone == "mavic 2 pro")
    
    
    # Fit GAM
    
    # FID ~ drone + altitude + behaviour
    # 
    # Main Effects
    # Question: What is the flight initiation distance for eastern curlew?
    # drone (does the FID vary with drone type?)
    # altitude (does the FID vary with drone altitude?)

    gam_FID <- gam(dronebirddistance ~
                          + s(height_above_takeoff.meters.),
                        data = data_FID,
                        family = 'gaussian',
                        method = 'REML')

    summary(gam_FID)
    # gam.check(gam_FID, rep = 500)
    
    # Creating new data
    
    height_above_takeoff.meters.FID = seq(30, 110, by=1)
    
    new_data_FID = expand.grid(height_above_takeoff.meters. = height_above_takeoff.meters.FID)
    
    pred_FID <- predict.gam(gam_FID,
                        new_data_FID,
                        trans=binomial()$linkinv,
                        type = "response",
                        se.fit = TRUE)
    
    FID_results <- new_data_FID %>% 
      mutate(prediction = pred_FID$fit) %>% 
      mutate(upper = pred_FID$fit + (2 * pred_FID$se.fit)) %>% 
      mutate(lower = pred_FID$fit - (2 * pred_FID$se.fit))
    
    # Visualising Results
    
    # effect of altitude for mavic 2 data
    ggplot() +
      theme_set(theme_bw()) +
      geom_ribbon(
        data = FID_results,
        aes(height_above_takeoff.meters.,
            ymin=lower,
            ymax=upper,
            colour = "orange",
            fill = "orange"),
        alpha=0.1) +
      geom_line(
        data = FID_results,
        aes(height_above_takeoff.meters.,
            prediction,
            colour = "orange"),
        size = 1.2) +
      geom_point(
        data = filter(data_FID, drone == "mavic 2 pro"),
        aes(height_above_takeoff.meters.,
            dronebirddistance,
            colour = "orange")) +
      coord_flip(ylim=c(0, 260),
                 xlim=c(0, 120)) +
      labs(x = "Drone Altitude (m)",
           y = "Flight Initiation Distance (m)") +
      # ggtitle("Mavic 2 Pro Induced Eastern Curlew Disturbance") +
      theme(
        axis.text = element_text(face="bold",
                                 color="black"),
        axis.title=element_text(size=14,
                                face="bold"),
        # panel.border = element_blank(),
        # axis.ticks = element_blank(),
        plot.title = element_text(size=16,
                                  face="bold",
                                  hjust = 0.5),
        legend.position = "none",
        aspect.ratio = 0.45) +
      scale_x_continuous(minor_breaks = round(seq(0, 140, 20)),
                         breaks = round(seq(0, 140, by = 20),1),
                         expand = c(0, 0)) +
      scale_y_continuous(minor_breaks = round(seq(-400, 400, 20)),
                         breaks = round(seq(-400, 400, by = 20),1),
                         expand = c(0, 0))
    
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
      
      # filter out species without enough data
      # group_by(species) %>%
      # filter(n() > 25) %>% 
      filter(species == "eastern curlew") %>% 
      
      # filter out drones without much data
      filter(drone != "mavic mini", drone != "phantom 4 pro", drone != "phantom pro 4")
      
      # to fix
      # drop_na(drone) %>% 
      # filter(drone != "mavic pro 2") %>%
      # filter(flightcode != "311") %>% 
      # filter(flightcode != "411")
    
    
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
                   + drone,
                   data = data_FIT,
                   family = 'binomial',
                   method = 'REML')
    
    summary(gam_FIT)
    
    # Creating new data
    
    dronebirddistance_FIT = seq(75,750, by=1)
    # species_FIT = unique(data_FIT$species)
    drone_FIT = unique(data_FIT$drone)
    
    new_data_FIT = expand.grid(dronebirddistance = dronebirddistance_FIT,
                               # species = species_FIT,
                               drone = drone_FIT)
    
    pred_FIT <- predict.gam(gam_FIT,
                            new_data_FIT,
                            trans=binomial()$linkinv,
                            type = "response",
                            se.fit = TRUE)
    
    FIT_results <- new_data_FIT %>% 
      mutate(prediction = pred_FIT$fit) %>% 
      mutate(upper = pred_FIT$fit + (2 * pred_FIT$se.fit)) %>% 
      mutate(lower = pred_FIT$fit - (2 * pred_FIT$se.fit))
    
    # Visualising Results
    ggplot() +
      theme_set(theme_bw()) +
      geom_line(
        data = FIT_results,
        aes(dronebirddistance,
            prediction,
            group = drone,
            colour = drone),
        size = 1.2) +
      geom_ribbon(
        data = FIT_results,
        aes(dronebirddistance,
            ymin=lower,
            ymax=upper,
            group = drone,
            fill = drone,
            colour = drone),
        alpha=0.1) +
      geom_rug(aes(dronebirddistance,
                   group = drone,
                   colour = drone),
               inherit.aes=FALSE, transform(filter(data_FIT, behaviour == 0), expl.name=dronebirddistance),
               sides = "b",
               size = 1) +
      geom_rug(aes(dronebirddistance,
                   group = drone,
                   colour = drone),
               inherit.aes=FALSE, transform(filter(data_FIT, behaviour == 1), expl.name=dronebirddistance),
               sides = "t",
               size = 1) +
      labs(x = "Drone Takeoff Distance (m)",
           y = "Probability of Inducing Bird Flight",
           fill = "Drone",
           colour = "Drone") +
      # ggtitle("Drone Induced Eastern Curlew Disturbance") +
      coord_cartesian(xlim = c(75,750)) +
      theme(axis.text = element_text(face="bold",
                                     color="black"),
            axis.title=element_text(size=14,
                                    face="bold"),
            # panel.border = element_blank(),
            # axis.ticks = element_blank(),
            plot.title = element_text(size=16,
                                      face="bold",
                                      hjust = 0.5),
            legend.title=element_text(size=14,
                                      face="bold"),
            legend.text = element_text(size=14,
                                       face="bold"),
            legend.position = c(0.75,0.75),
            legend.background = element_rect(fill=alpha("blue", 0))) +
      scale_x_continuous(expand = c(0, 0),
                         breaks = round(seq(0, 800, by = 100),1)) +
      scale_y_continuous(expand = c(0, 0))

#### PLAYING AROUND ####
  summary(data)
  data_play <- data_FIA %>%
    
    # Remove all drones except inspire 2
    filter(drone == "inspire 2") %>% 
    filter(behaviour == 0)
    # select(c(`count`, `test`)) %>% 
    # drop_na()
    
  
    