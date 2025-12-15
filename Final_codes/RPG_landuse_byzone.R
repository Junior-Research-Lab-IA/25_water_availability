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

zone <- "zoneC"

zone_shp <- st_read(paste0(zone, ".shp"))

tm_basemap("OpenStreetMap.France") +
  tm_shape(zone_shp) +
  tm_borders(col = "black", lwd = 1)

zone_communes <- happign::get_wfs(
  x = zone_shp,
  layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
)

tm_basemap("OpenStreetMap.France") +
  tm_shape(zone_communes) +
  tm_borders(col = "black", lwd = 1)

inzone_communes <- st_intersection(zone_communes, zone_shp)

tm_basemap("OpenStreetMap.France") +
  tm_shape(inzone_communes) +
  tm_borders(col = "black", lwd = 1)

# Retrieving the grouping of crop codes by type
crop_type <- read.csv("crop_type.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

# Years to process
years <- 2018:2022   # change selon tes besoins

# Function that process a year
processing_ayear <- function(ayear) {
  
  message("Treatment of the year : ", ayear)
  
  # 1. Retrieval of rpg data
  
  rpgbis <- get_rpg_data(inzone_communes, year = ayear, id = "id", source = "IGN")
  
  # 2. Removal of the geometry (optional, this one that we can save as csv at one moment if needed)
  rpgbis_no_geom <- rpgbis %>% st_drop_geometry()
  
  # 3. Cleaning of surf_parc
  rpg_clean <- rpgbis_no_geom %>%
    mutate(
      surf_parc = surf_parc %>%
        as.character() %>%
        trimws() %>%
        gsub(",", ".", .) %>%          # virgules → points
        gsub("[^0-9.]", "", .) %>%     # removal of unwanted caracteres
        as.numeric()
    )
  
  # 3.bis Adding the column of corresponding crop type
  rpg_clean_type <- rpg_clean %>%
    left_join(
      crop_type %>% select("cropcode","type_simplified","irrigation") %>%
        rename("code_cultu" = "cropcode"),
      by = "code_cultu"
    )
  
  
  # 3.bisbis Removal NA
  rpg_clean_type <- na.omit(rpg_clean_type)
  
  # 4. Computing of the crop's type proportions
  proportions <- rpg_clean_type %>%
    group_by(type_simplified,irrigation) %>%
    summarise(total_surface = sum(surf_parc, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      prop_surface = total_surface / sum(total_surface)
    ) %>%
    arrange(desc(prop_surface))
  
  # 5. Add the year in a column
  proportions_df <- proportions %>%
    mutate(year = ayear)        # ajouter l’année
  
  return(proportions_df)
}

# Loop to process all the years we consider
df_total <- map_df(years, processing_ayear)

# Filter to remove the rows of crop with a null surface (to simplify the legends)
df_final <- filter(df_total, total_surface!=0, .by = NULL, .preserve = FALSE)   
write.csv(df_final, paste0(zone,"_past_land_use_area_and_prop.csv"), row.names = FALSE)

df_final$type_simplified <- recode(
  df_final$type_simplified,
  "vineyard" = "Vineyards",
  "orchard" = "Orchards",
  "vegetable" = "Vegetables",
  "perennial" = "Other perennials",
  "semiperennial" = "Semi-perennials",
  "pasture" = "Pastures",
  "nonprod" = "Non productive area",
  "annual" = "Other annuals"
)

#boxplot to visualise the evolution of crop surfaces
areas_plot <- ggplot(df_final, aes(x = year, y = total_surface)) +
  geom_col(aes(fill = factor(
    type_simplified,
    levels = c(
      "Non productive area",
      "Pastures",
      "Other annuals",
      "Semi-perennials",
      "Other perennials",
      "Vegetables",
      "Orchards",
      "Vineyards"
    )
  )
  ), width = 0.7) +
  scale_fill_manual(
    values = c(
      "Vineyards"= "#580023",
      "Orchards"= "#058816",
      "Vegetables"= "#ff4c0b",    
      "Other perennials"= "#e8e8e8", 
      "Semi-perennials"= "#cfcfcf",      
      "Other annuals"= "#959494",
      "Pastures"= "#787878",
      "Non productive area"= "#5B5B5B"
    )
  )+
  labs(
    title = paste0("Past land use - ",zone),
    x = "Year",
    y = "Area (ha)",
    fill = "Crops"
  )+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(
      hjust = 0.5,            # centre le titre
      margin = margin(b = 10)
    )
  )
show(areas_plot)

#boxplot to visualise the evolution of crop proportions
proportions_plot <- ggplot(df_final, aes(x = year, y = prop_surface)) +
  geom_col(aes(fill = factor(
    type_simplified,
    levels = c(
      "Non productive area",
      "Pastures",
      "Other annuals",
      "Semi-perennials",
      "Other perennials",
      "Vegetables",
      "Orchards",
      "Vineyards"
    )
  )
  ), width = 0.7) +
  scale_fill_manual(
    values = c(
      "Vineyards"= "#580023",
      "Orchards"= "#058816",
      "Vegetables"= "#ff4c0b",    
      "Other perennials"= "#e8e8e8", 
      "Semi-perennials"= "#cfcfcf",      
      "Other annuals"= "#959494",
      "Pastures"= "#787878",
      "Non productive area"= "#5B5B5B"
    )
  )+
  labs(
    title = paste0("Past land use (%) - ",zone),
    x = "Year",
    y = "Percentage of the total cultivated area",
    fill = "Crops"
  )+
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(
      hjust = 0.5,            # centre le titre
      margin = margin(b = 10)
    )
  )
show(proportions_plot)


sau_by_year <- df_final %>%
  group_by(year) %>%
  summarise(SAU = sum(total_surface, na.rm = TRUE))
sau_by_year

sau_mean <- mean(sau_by_year$SAU)
sau_mean

prop_crop <- df_final %>%
  group_by(type_simplified) %>%
  summarise(surface_totale = sum(total_surface, na.rm = TRUE)) %>%
  mutate(proportion = 100 * surface_totale / sum(surface_totale))
prop_crop

prop_crop$SAU <- sau_mean

prop_crop_SAU <- prop_crop %>% 
  select(type_simplified, proportion, SAU)

write.csv(prop_crop_SAU, paste0(zone,"base_prop_crop_and_SAU_for_python.csv"), row.names = FALSE)


