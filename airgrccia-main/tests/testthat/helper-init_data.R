# Define bounding box coordinates
bbox <- c(xmin = 325522.8, ymin = 6250949.2, xmax = 347137.1, ymax = 6270285.1)

# Download DEM
file_dem <- getCachePath(bbox, "test-extract_sub_basins-dem", "tiff")
dem <- suppressWarnings(get_ign_dem(
  sf::st_bbox(bbox, crs = 2154),
  filename = file_dem #, overwrite = TRUE
))
# Download Stream paths
file_streams <- getCachePath(bbox, "test-extract_sub_basins-streams", "shp")
streams <- get_ign_streams(
  sf::st_bbox(bbox, crs = 2154),
  filename = file_streams #, overwrite = TRUE
)

# Define sub-basin outlets
outlets <- data.frame(
  id = c("Latsa", "NiveLaxia", "Antzarako", "NiveAntzarako"),
  x = c(339530, 343038, 338408, 337856),
  y = c(6264578, 6256016, 6269769, 6270153)
)

# Compute (or load from cache) sub-basins polygon
sb <- extract_sub_basins(
  outlets = outlets,
  dem = dem,
  streams = streams
)
