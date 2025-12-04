library(dplyr)
library(readr)
library(CropWat)

#make sure you downloaded the soil data (already in air-grccia) and the right climate data file into airGCCIA working directory

climate <- read_csv("X.csv") %>%    # replace X by the name of the climate data file
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
target_zone <- "zoneX"    # replace X by the letter of the zone wanted

soil <- soil_params %>% 
  filter(zone == target_zone)

#CREATE A SOWING DATE TABLE, change only perinial crops. 
sowing_date=data.frame("FAO-W-WHEAT", "FAO-SORGHUM",
                       "FAO-VEGETABL", "FAO-ALFALFA1",
                       "FAO-PULSES", "FAO-BARLEY"),Date=c('10-25','05-15','02-15','03-01','03-01','02-20'))

#irrigation time from 15/03 to 16/10
irrig_date<-c()
for (year in 2023:2075) {
  start_date <- as.Date(paste(year, "03", "15", sep = "-"))
  end_date <- as.Date(paste(year, "10", "16", sep = "-"))
  date_sequence =c(seq.Date(from = start_date, to = end_date, by = "day"))
  irrig_date <- c(as.Date(irrig_date), date_sequence)}
  
# replace X by crop code
cw_input_test2 <- CW_create_input("X",
                                  DatesR = meteo$Date,
                                  ETo    = meteo$ETo,
                                  P      = meteo$P,
                                  soil_depth = soil$w_depth,   # 30cm soil depth
                                  AWC        = soil$awc  ) 

# Set up the initial state of the model
X0_initial_state <- CW_create_state(cw_input = cw_input)

  
  # irrigation management strategy
  irrig_1pc_RAW <- CW_irrig_fun_factory(RAW_ratio = 1, apply_Dr = TRUE)    #plant is never stressed
  
  # Simulate the water balance with the chosen irrigation management
  cw_output <- CW_run_simulation(X0_initial_state,cw_input,
                                 FUN_IRRIG = irrig_1pc_RAW)
  


