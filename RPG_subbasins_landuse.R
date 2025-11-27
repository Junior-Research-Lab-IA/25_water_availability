install.packages("remotes") # If not already installed
remotes::install_deps(dep=TRUE)
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


basin_communes <- happign::get_wfs(
  x = basin_union,
  layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
)

tm_basemap("OpenStreetMap.France") +
  tm_shape(basin_communes) +
  tm_borders(col = "black", lwd = 1)

# Years to process
years <- 2018:2022   # change selon tes besoins

# Function that process a year
processing_ayear <- function(ayear) {
  
  message("Treatment of the year : ", ayear)
  
    # 1. Retrieval of rpg data

    rpgbis <- get_rpg_data(basin_communes, year = 2020, id = "id")

    # 2. Removal of the geometry (optional, this one that we can save as csv at one moment if needed)
    rpgbis_no_geom <- rpgbis %>% st_drop_geometry()
    
    # 3. Cleaning of surf_parc
    rpg_clean <- rpgbis %>%
      mutate(
        surf_parc = surf_parc %>%
          as.character() %>%
          trimws() %>%
          gsub(",", ".", .) %>%          # virgules → points
          gsub("[^0-9.]", "", .) %>%     # removal of unwanted caracteres
          as.numeric()
      )
    
    # 4. Computing of the crops proportions
    proportions <- rpg_clean %>%
      group_by(code_cultu) %>%
      summarise(total_surface = sum(surf_parc, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        prop_surface = total_surface / sum(total_surface)
      ) %>%
      arrange(desc(prop_surface))
    
    # 5. Removal of the geometry + as the year in a column
    proportions_df <- proportions %>%
      st_drop_geometry() %>%       # enlever la géométrie
      mutate(year = ayear)        # ajouter l’année
    
    return(proportions_df)
}

# Loop to process all the years we consider
df_total <- map_df(years, processing_ayear)
write.csv(df_total, "proportions_surfaces_crops_tot.csv", row.names = FALSE)

# Filter to remove the rows of crop with a null surface (to simplify the legends)
df_final <- filter(df_total, total_surface!=0, .by = NULL, .preserve = FALSE)   
write.csv(df_final, "proportions_surfaces_crops_final.csv", row.names = FALSE)

#boxplot to visualise the evolution of crop surfaces
surfaces_plot <- ggplot(df_final, aes(x = year, y = total_surface))+
  geom_col(aes(fill = code_cultu), width = 0.7)
show(surfaces_plot)

#boxplot to visualise the evolution of crop proportions
proportions_plot <- ggplot(df_final, aes(x = year, y = prop_surface))+
  geom_col(aes(fill = code_cultu), width = 0.7)
show(proportions_plot)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#basin_cult <- rpg_basin[,"code_cultu"]

#tm_basemap("OpenStreetMap.France") +
#  tm_shape(basin_cult) +
#  tm_borders(col = "black", lwd = 1)




#sf_basin <- read_sf("BassinVersantTopographique_FXX.shp")


#tm_basemap("OpenStreetMap.France") +
# tm_shape(sf_basin) +
# tm_borders(col = "black", lwd = 1)

#plot(sf_basin)

#sf_heraultbasin <- sf_basin %>% dplyr::filter(grepl('Hérault',TopoOH))

#tm_basemap("OpenStreetMap.France") +
# tm_shape(sf_heraultbasin) +
# tm_borders(col = "black", lwd = 1)

#plot(sf_heraultbasin)

#sf_heraultbasin <- st_transform(sf_heraultbasin, 2154)

#basin_union <- st_union(sf_heraultbasin)
