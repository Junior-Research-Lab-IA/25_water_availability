library(RADIS)
library(sf)

# Read your shapefile 
> communes_sf <- st_read("zoneA.shp")    #change the letter according to the zone and check that you have the 4 shapefiles for thsis zone in the working directory

# Retrieve soil depth
soil_depth_zone <- get_soil_depth(
  communes_sf,
  source = "BDGSF",
  overlay_mode = "aggregate",
  cache_dir = file.path(Sys.getenv("RADIS_CACHE_DIR", file.path(dirname(tempdir()),
  "RADIS")), "data-raw/soil_depth/BDGSF"),
  force_download = FALSE,
)

#plot the area
plot(soil_depth_zone)

# Save .csv
write.csv(soil_depth_zone, "soildepth_zoneA.csv", row.names = FALSE)    #change the letter according to the zone
