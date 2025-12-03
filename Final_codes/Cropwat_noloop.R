library(dplyr)
library(readr)
library(CropWat)

 #make sure you downloaded the soil data (Zone A, B and C) and climate scenario file into airGCCIA working directory

 # replace X by the file of climate scenario
 climate <- read_csv("X.csv") %>%
    rename(
            Date = time,
            ETo  = evapo,
            P    = pr
           )

  # set date as the whole timeseries
  meteo <- climate

 #load soil data
  soil_params <- read.csv("Soil data (Zone A, B and C).csv")

 # replace X by target zone
  target_zone <- "zoneX"   # <- change to zoneB or zoneC later
  
  soil <- soil_params %>% 
    filter(zone == target_zone)

# replace X by crop code
  cw_input_test2 <- CW_create_input("X",
                                              DatesR = meteo$Date,
                                              ETo    = meteo$ETo,
                                              P      = meteo$P,
                                              soil_depth = soil$w_depth,   # 30cm soil depth
                                              AWC        = soil$awc  )  
