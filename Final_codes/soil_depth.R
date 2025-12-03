library(RADIS)
library(sf)

# Read your shapefile 
> communes_sf <- st_read("zoneA.shp")

# Retrieve soil depth
soil_depth_zonea <- get_soil_depth(
  communes_sf,
  source = "BDGSF",
  overlay_mode = "aggregate",
  cache_dir = file.path(Sys.getenv("RADIS_CACHE_DIR", file.path(dirname(tempdir()),
  "RADIS")), "data-raw/soil_depth/BDGSF"),
  force_download = FALSE,
)

#plot the area
plot(soil_depth_zonea)

# Save .csv
write.csv(soil_depth_zonea, "soildepth_zoneA.csv", row.names = FALSE)
