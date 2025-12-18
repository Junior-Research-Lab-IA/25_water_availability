library(whitebox)
library(tmap)
library(tmaptools)
library(terra)
library(remotes)
library(RADIS)
library(purrr)
library(happign)
bbox <- sf::st_bbox(
  c(
    xmin = 716000,
    ymin = 6238000,
    xmax = 774000,
    ymax = 6355000
  ),
  crs = 2154
)
bbox_sf <- sf::st_as_sfc(bbox) %>% st_as_sf()
tmap_mode("view")
tm_basemap("OpenTopoMap") +
  tm_shape(bbox_sf) +
  tm_polygons(lwd = 3, col = "red", fill = NA, fill_alpha = 0)
dem_file <- getCachePath(bbox, "dem", "tif")
dem <- get_ign_dem(bbox, filename = dem_file)
plot(dem)
 file_streams <- getCachePath(bbox, "stream", "shp")
 streams <- get_ign_streams(bbox, filename = file_streams)
 original_mode <- tmap_mode()

 tmap_mode("plot")

 tm_basemap("OpenStreetMap.France") +
  tm_shape(streams) +
  tm_lines(col = "nature")
 tmap_mode(original_mode)

 stations <- hubeau::get_hydrometrie_sites(
  code_site = c(
    "Y2140010", "Y2140020", "Y2230010",
    "Y2210010", "Y2300020", "Y2100020"
  )
)
stations
file_sb <- getCachePath("V01", "sb", "RDS")
outlets <- stations[, c("code_site", "coordonnee_x_site", "coordonnee_y_site", "surface_bv")]
colnames(outlets) <- c("id", "x", "y", "area")
outlets$area <- outlets$area * 1E6  # Convert from km² to m²
sub_basins <- extract_sub_basins(
  outlets = outlets, 
  dem = dem, 
  streams = streams, 
  filename = file_sb, 
  area_tolerance = 0.2
)
 sub_selection <- sub_basins %>% 
  dplyr::filter(id %in% c("Y2140010"))
 tm_basemap("OpenStreetMap.France") +
  tm_shape(sub_selection) +
  tm_borders(col = "black", lwd = 1)
 basin_union <- st_union(sub_selection)

tm_basemap("OpenStreetMap.France") +
  tm_shape(basin_union) +
  tm_borders(col = "black", lwd = 1)
 basin_communes <- happign::get_wfs(
  x = basin_union,
  layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
) 
 tm_basemap("OpenStreetMap.France") +
  tm_shape(basin_communes) +
  tm_borders(col = "black", lwd = 1)   
 
 soil_depth_basincommunes <- get_soil_depth(
  sf             = basin_communes,
  force_download = TRUE
)
tmap_mode("view")

tm_basemap("OpenStreetMap.France") +
  tm_shape(soil_depth_basincommunes) +
  tm_fill("soil_depth", title = "Soil depth") +
  tm_borders()
library(sf)
library(dplyr)
awc_basincommunes <- get_soil_awc(
    sf             = basin_communes,
    force_download = TRUE
  ) 
library(tmap)
tmap_mode("view")  

tm_basemap("OpenStreetMap.France") +
  tm_shape(awc_basincommunes) +
  tm_fill(
    "soil_awc",
    title  = "AWC"
  ) +
  tm_borders()
library(dplyr)
library(sf)

#Create Map for homogeneous zones 

# Soil depth
soil_depth_basincommunes <- soil_depth_basincommunes %>% 
  mutate(
    depth_class = cut(
      soil_depth,
      breaks = c(0.2, 0.6, 1, 1.4),
      labels = c("shallow","medium", "deep"),
      include.lowest = TRUE
    )
  ) %>% 
  select(depth_class, geometry)   


# AWC
awc_basincommunes <- awc_basincommunes %>% 
  mutate(
    awc_class = cut(
      soil_awc,
      breaks = c(40, 100, 160),
      labels = c("low", "high"),
      include.lowest = TRUE
    )
  ) %>% 
  select(awc_class, geometry)
zones_raw <- st_intersection(soil_depth_basincommunes, awc_basincommunes)
zones_hom <- zones_raw %>% 
  mutate(
    zone_id = paste0(depth_class, "_", awc_class)
  ) %>% 
  group_by(zone_id, depth_class, awc_class) %>% 
  summarise(.groups = "drop")
library(tmap)
tmap_mode("view")

tm_basemap("OpenStreetMap.France") +
  tm_shape(zones_hom) +
  tm_polygons("zone_id", alpha = 0.6) +
  tm_borders()

climate_basincommunes <- soil_depth_basincommunes %>% 
  mutate(
    depth_class = cut(
      soil_depth,
      breaks = c(0.2, 0.6, 1, 1.4),
      labels = c("shallow","medium", "deep"),
      include.lowest = TRUE
    )
  ) %>% 
  select(depth_class, geometry)
