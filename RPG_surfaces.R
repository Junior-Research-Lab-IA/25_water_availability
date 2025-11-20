library(sf)
library(dplyr)
library(purrr)
library(happign)
library(RADIS)
library(ggplot2)
library(sf)

bbox_L93 <- sf::st_bbox(
  c(
    xmin = 676121,
    xmax = 809573,
    ymin = 6241941,
    ymax = 6335868
  ),
  crs = sf::st_crs(2154) # RGF93 / Lambert-93
)

rect <- sf::st_as_sfc(bbox_L93)
sf_herault <- happign::get_wfs(
  x = rect,
  layer = "LIMITES_ADMINISTRATIVES_EXPRESS.LATEST:commune"
)

my_commune <- sf_herault %>% dplyr::filter(code_insee == "11012")

# Years to process
years <- 2018:2022   # change selon tes besoins

# Function that process a year
processing_ayear <- function(ayear) {
  
  message("Treatment of the year : ", ayear)
  
  # 1. Retrieval of rpg data
  rpgbis <- get_rpg_data(my_commune, year = ayear, id = "id")
  
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
df_total <- map_df(years, treatment_ayear)
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


