#' Title
#'
#' @param sb Output of [extract_sub_basins].
#'
#' @return A simple feature with points representing sub-basins outlets.
#' @export
#'
sb_to_outlets <- function(sb) {
  df <- as.data.frame(sb)
  sf::st_as_sf(df, coords = c("mvd_outlet_x", "mvd_outlet_y"), crs = terra::crs(sb))
}
