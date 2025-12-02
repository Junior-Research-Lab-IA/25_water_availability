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

tm_basemap("OpenStreetMap.France") +
  tm_shape(sub_basins) +
  tm_borders(col = "black", lwd = 1)

sub_selection <- sub_basins %>% 
  dplyr::filter(id %in% c("Y2140010"))    

tm_basemap("OpenStreetMap.France") +
  tm_shape(sub_selection) +
  tm_borders(col = "black", lwd = 1)

#basin_union <- st_union(sub_selection)

#tm_basemap("OpenStreetMap.France") +
#  tm_shape(basin_union) +
#  tm_borders(col = "black", lwd = 1)


basin_communes <- happign::get_wfs(
  x = sub_selection,
  layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
)

write.csv(basin_communes, "communes.csv", row.names = FALSE)

tm_basemap("OpenStreetMap.France") +
  tm_shape(basin_communes) +
  tm_borders(col = "black", lwd = 1)

communes_zoneA <- basin_communes %>% dplyr::filter(code_insee == "34195" | code_insee == "34277" | code_insee == "34261" | code_insee == "34283" | code_insee == "34286"| code_insee == "34317")
communes_zoneB <- basin_communes %>% dplyr::filter(code_insee == "34010" | code_insee == "34012" | code_insee == "34035" | code_insee == "	34060" | code_insee == "34114"| code_insee == "34163" | code_insee == "34208" | code_insee == "34221" | code_insee == "34241" | code_insee == "34274"| code_insee == "34282" | code_insee == "34343")
communes_zoneC <- basin_communes %>% dplyr::filter(code_insee == "34011" | code_insee == "34041"| code_insee == "34076" | code_insee == "34079" | code_insee == "34122" | code_insee == "34124"| code_insee == "34125"| code_insee == "34173"| code_insee == "34215" | code_insee == "34239" | code_insee == "34254" | code_insee == "34262" | code_insee == "34267" | code_insee == "34268"| code_insee == "34287")


tm_basemap("OpenStreetMap.France") +
  tm_shape(communes_zoneA) +
  tm_borders(col = "black", lwd = 1)
tm_basemap("OpenStreetMap.France") +
  tm_shape(communes_zoneB) +
  tm_borders(col = "black", lwd = 1)
tm_basemap("OpenStreetMap.France") +
  tm_shape(communes_zoneC) +
  tm_borders(col = "black", lwd = 1)

zoneA <- st_union(communes_zoneA)
zoneB <- st_union(communes_zoneB)
zoneC <- st_union(communes_zoneC)

tm_basemap("OpenStreetMap.France") +
  tm_shape(zoneA) +
  tm_borders(col = "black", lwd = 1)
tm_basemap("OpenStreetMap.France") +
  tm_shape(zoneB) +
  tm_borders(col = "black", lwd = 1)
tm_basemap("OpenStreetMap.France") +
  tm_shape(zoneC) +
  tm_borders(col = "black", lwd = 1)


st_write(zoneA, "zoneA.shp")
st_write(zoneB, "zoneB.shp")
st_write(zoneC, "zoneC.shp")
