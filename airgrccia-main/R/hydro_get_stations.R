#' Get info on hydrometric station
#'
#' @param x code site
#'
#' @return A [data.frame] or a SpatVector. See [hubeau::get_hydrometrie_sites].
#' @export
#'
#' @examples
#' hydro_get_station("Y2300020")
#' hydro_station_as_sf("Y2300020")
hydro_get_station <- function(x) {
  hubeau::get_hydrometrie_sites(code_site = x)
}

#' @export
#' @rdname hydro_get_station
hydro_station_as_sf <- function(x) {
  station <- hydro_get_station(x)
  station_point <- sf::st_point(c(station$coordonnee_x_site, station$coordonnee_y_site))
  v <- terra::vect(station_point)
  terra::crs(v) <- "epsg:2154"
  v
}
