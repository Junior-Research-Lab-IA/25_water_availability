pkgload::load_all()
cfg <- loadConfig()
library(tidyverse)
library(stars)

stopifnot(cfg$data$write_results)

path_SAFRAN <- "E:/Archives GHOSTE/Partage/SAFRAN/safran_2023-11-17"
dates <- seq.Date(as.Date("2012-08-01"), as.Date("2023-07-31"), by = "1 days")

################################################################################
# SAFRAN / DRIAS coordinates reconciliation
################################################################################
file <- system.file("extdata", "mailles_safran_drias.tsv", package = "airGRccia")
if (!file.exists(file)) {
  # Download correspondence SAFRAN id / DRIAS id in TSV format from https://doi.org/10.57745/1PDFNL
  url_mailles_safran_drias <-
    "https://entrepot.recherche.data.gouv.fr/api/access/datafile/142676?format=tab&gbrecs=true"
  df_mailles_safran_drias <- readr::read_tsv(url_mailles_safran_drias)

  # Download description mailles DRIAS avec coordonnées lambert
  url_grille_drias <- "https://drias-prod.meteo.fr/serveur/simulations_climatiques/grilles/safran/grilleSafran_complete_drias2021.csv"
  s <- readLines(url_grille_drias, n = 1)
  coln_gd <- strsplit(s, ";")[[1]]
  coln_gd <- sub("#", "", coln_gd) %>% trimws()
  df_grille_drias <-
    read.table(url_grille_drias, sep = ";", col.names = coln_gd)

  # Merge
  df_safran_lambert <- df_mailles_safran_drias %>%
    left_join(df_grille_drias, join_by(maille_drias == idPointDrias))

  # Save result
  comments <- c(
    "SAFRAN/DRIAS identifier correspondences downloaded from https://doi.org/10.57745/1PDFNL",
    "DRIAS grid description with Lambert coords from ",
    url_grille_drias,
    "Warning: Lambert coords correspond to the center of the cells"
  )
  comments <- paste("#", comments)

  writeLines(comments, file)
  readr::write_tsv(df_safran_lambert,
                   file,
                   append = TRUE,
                   col_names = TRUE)
} else {
  df_safran_lambert <- readr::read_tsv(file, comment = "#")
}

################################################################################
# Create empty stars for SAFRAN data
################################################################################

v_herault <- terra::vect(getDataPath("sub-basins.geojson"))
terra::crs(v_herault) <- "epsg:2154"
v_herault <- terra::project(v_herault, "epsg:27572")
ext <- terra::ext(v_herault)
# Selection by cell centers
df_crop <- df_safran_lambert %>%
  filter(
    E.lambert2et.m. > ext$xmin - 4001,
    N.lambert2et.m. > ext$ymin - 4001,
    E.lambert2et.m. < ext$xmax + 4001,
    N.lambert2et.m. < ext$ymax + 4001
  )
# x and y should be defined for the lower-left corner of each cell
x <- unique(df_crop$E.lambert2et.m.) %>% sort - 4000
y <- unique(df_crop$N.lambert2et.m.) %>% sort - 4000

X0 <- min(df_crop$X) - 1
Y0 <- min(df_crop$Y) - 1

safran_template <- read_safran_csv(list.files(path_SAFRAN, full.names = TRUE)[1], dates = dates)
variables <- names(safran_template)[-1]
variables <- setNames(nm = variables)
l_safran <- lapply(variables, function(v) {
  array(as.numeric(NA), dim = c(
    x = length(x),
    y = length(y),
    time = length(dates)
  ))
})

for (i in seq.int(nrow(df_crop))) {
  r <- df_crop[i, , drop = FALSE]
  file <- file.path(path_SAFRAN, sprintf("%04i_SAFRAN.txt", r$maille_safran))
  message("Processing file ", file, "...")
  df_safran <- read_safran_csv(file, dates)
  for (v in variables) {
    l_safran[[v]][r$X - X0, r$Y - Y0, ] <- df_safran[[v]]
  }
}

# Convert huss from g/kg to kg/kg
l_safran$huss <- l_safran$huss / 1000

v_units <- get_safran_var_units()

s_safran <- do.call(c, lapply(variables, function(v) {
  s <- stars::st_as_stars(l_safran[[v]], dimensions = stars::st_dimensions(x = x, y = y, time = dates))
  sf::st_crs(s) <- "EPSG:27572"
  names(s) <- v
  units(s[[v]]) <- v_units[v]
  s
}))

file_safran <- getDataPath("meteo_safran_historical.nc")
stars::write_mdim(s_safran, file_safran)
