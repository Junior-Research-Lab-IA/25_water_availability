install.packages("remotes")
remotes::install_deps(dep=TRUE)
remotes::install_git("https://forge.inrae.fr/umr-g-eau/RADIS.git", dependencies = TRUE)
pkgload::load_all()
devtools::install()
library(airGRccia)
library(RADIS)
library(sf)
library(dplyr)
library(purrr)
library(happign)
library(ggplot2)
library(tmap)
library(tidyverse)
library(terra)
# Define the bounding box coordinates in RGF93
bbox <- sf::st_bbox(
  c(
    xmin = 716000,
    ymin = 6238000,
    xmax = 774000,
    ymax = 6355000
  ),
  crs = 2154
)

# Create an sf object from the bounding box
bbox_sf <- sf::st_as_sfc(bbox) %>% st_as_sf()

tmap_mode("view")
#> ℹ tmap modes "plot" - "view"
#> ℹ toggle with `tmap::ttm()`
tm_basemap("OpenTopoMap") +
  tm_shape(bbox_sf) +
  tm_polygons(lwd = 3, col = "red", fill = NA, fill_alpha = 0)


# Define cache path for the DEM (to avoid multiple downloads)
dem_file <- getCachePath(bbox, "dem", "tif")
dem <- get_ign_dem(bbox, filename = dem_file)
#> Using cached file: /builds/airgriwrm/airgrccia/ci/airGRccia_cache/airGRccia_cache/dem_7a4806f542eb436ff087627208734161.tif
plot(dem)


# Define cache path for the stream network
file_streams <- getCachePath(bbox, "stream", "shp")
streams <- get_ign_streams(bbox, filename = file_streams)
#> Using cached file: /builds/airgriwrm/airgrccia/ci/airGRccia_cache/airGRccia_cache/stream_7a4806f542eb436ff087627208734161.shp


# Plot (not in "view" mode because it is too large for a vignette)
original_mode <- tmap_mode()
#> ℹ tmap modes "plot" - "view"
tmap_mode("plot")
#> ℹ tmap modes "plot" - "view"
tm_basemap("OpenStreetMap.France") +
  tm_shape(streams) +
  tm_lines(col = "nature")

stations <- hubeau::get_hydrometrie_sites(
  code_site = c(
    "Y2140010", "Y2140020", "Y2230010",
    "Y2210010", "Y2300020", "Y2100020"
  )
)  

# Define cache path for Sub-basin polygons
file_sb <- getCachePath("V01", "sb", "RDS")

# Format the POIs location for `extract_sub_basins`
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
  dplyr::filter(id %in% c("Y2140010", "Y2300020"))

tm_basemap("OpenStreetMap.France") +
  tm_shape(sub_selection) +
  tm_borders(col = "black", lwd = 1)

basin_union <- st_union(sub_selection)

tm_basemap("OpenStreetMap.France") +
  tm_shape(basin_union) +
  tm_borders(col = "black", lwd = 1)



# Extract Lambert-93 bounding box
bbox <- st_bbox(basin_union)

# Convert bbox into numeric vector for DRIAS
bbox_numeric <- as.numeric(bbox)

data <- get_drias_daily(
  
  # Lambert-93 bounding box
  LAMBX__greater = bbox_numeric[1],
  LAMBX__less = bbox_numeric[3],
  LAMBY__greater = bbox_numeric[2],
  LAMBY__less = bbox_numeric[4],
  
  # Time period
  DATE__greater = "2021-01-01",
  DATE__less = "2050-12-31",
  
  # Climate model choices
  GCM = "CNRM-CERFACS-CNRM-CM5",
  RCM = "CNRM-ALADIN63",
  BC  = "ADAMONT",
  RCP = "rcp45",
  
  ETP = "FAO",
  # Variables
  fields = c("prtot", "evspsblpot")
)

