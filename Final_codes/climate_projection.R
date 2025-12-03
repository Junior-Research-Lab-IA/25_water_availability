library(sf)
library(dplyr)
library(RADIS)

#open the shapefile
zone <- st_read("zoneX.shp")    #replace X by the letter of the zone your want and check that your working directory is airgrccia-main

zone_27572 <- st_transform(zone, 27572)

bbox_27572 <- st_bbox(zone_27572)

scenarios_all <- get_drias_scenario()

#filter the desired RCP scenarios
sc_rcp <- scenarios_all %>% 
  filter(RCP == "rcpXX")    #replace XX by the number of the rcp you want (cf. organisation sheet)

#Group the scenarios chain
mods <- sc_rcp %>%
  group_by(GCM, RCM, BC, RCP) %>%
  summarise(vars = list(variable), .groups = "drop")

#Open the scenarios combination
sel_mod <- mods %>%
  filter(all(c("evspsblpot", "prtot") %in% vars[[1]]))

#Pick the desired number
sel_mod <- mods %>%
  filter(all(c("evspsblpot", "prtot") %in% vars[[1]])) %>% 
  slice(1)

#Check the one you choose
sel_mod

#Retrieve the climate data 
climate_bbox <- get_drias_daily(
  LAMBX__greater = bbox_27572["xmin"],
  LAMBX__less    = bbox_27572["xmax"],
  LAMBY__greater = bbox_27572["ymin"],
  LAMBY__less    = bbox_27572["ymax"],
  
  DATE__greater  = "2023-01-01",
  DATE__less     = "2075-12-31",
  
  GCM  = sel_mod$GCM,
  RCM  = sel_mod$RCM,
  BC   = sel_mod$BC,
  RCP  = sel_mod$RCP,
  
  fields = c("evspsblpot", "prtot"),
  ETP    = "FAO"
)

library(stars)
library(dplyr)

#open as data frame
clim_df <- as.data.frame(climate_bbox)  
names(clim_df)

#summarize the data as one value per day
clim_daily <- clim_df %>%
  group_by(time) %>%
  summarise(
    evapo = mean(evspsblpot, na.rm = TRUE),
    pr    = mean(prtot, na.rm = TRUE),
    .groups = "drop"
  )

#save the data in csv
write.csv(clim_daily, "namefromsheet.csv", row.names = FALSE)    #put the name of the output file written in the oragnisation sheet
