#' Extract and Aggregate SIM2 Daily Climate Data from RADIS
#'
#' This function retrieves selected daily climate variables from the SIM2 dataset
#' via the `RADIS::get_sim2_daily()` function, renames the fields for compatibility
#' with GR models (e.g., `airGR`), and optionally aggregates multiple related
#' source fields (e.g., solid and liquid precipitation).
#'
#' @param x An `sf` object or geometry supported by `RADIS::get_sim2_daily()`,
#'        typically representing a spatial feature (e.g., watershed or point).
#' @param fields A named list where each element corresponds to a GR-compatible
#'        variable name (e.g., `Precip`, `PotEvap`, `TempMean`) and contains one or
#'        more SIM2 field names to be retrieved and optionally summed.
#' @param date_start A `Date` object specifying the start of the extraction period.
#' @param date_end A `Date` object specifying the end of the extraction period.
#' @param overlay_mode A character string passed to `RADIS::get_sim2_daily()`,
#'        typically `"aggregate"` for spatial mean over polygons or `"raw"` for full raster.
#' @param ... Additional arguments passed to `RADIS::get_sim2_daily()`.
#' @param filename If not `NULL`, path of cached data (See details).
#' @inheritParams get_ign_streams
#'
#' @details
#' Default value for `filename` depends on the parameter `overlay_mode` which
#' cache the data as a netCDF file for `overlay_mode = "intersect_geometry"` and
#' in RDS format for `overlay_mode = "intersect_geometry"`.
#'
#'
#'
#' @return A named list where each element is a 3D array (or similar structure) containing
#'         the extracted and aggregated climate variables, named using the keys of `fields`
#'         (e.g., `"Precip"`, `"PotEvap"`, etc.).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Example with a watershed polygon
#' basin <- sf::st_read("my_watershed.gpkg")
#' data <- get_climate_safran(
#'   x = basin,
#'   date_start = as.Date("2000-01-01"),
#'   date_end = as.Date("2005-12-31")
#' )
#' }
#'
#' @seealso [RADIS::get_sim2_daily()]
#' @export
get_climate_safran <- function(
  x,
  fields = list(
    Precip = c("PRENEI_Q", "PRELIQ_Q"),
    # Precipitation components
    PotEvap = "ETP_Q",
    # Potential Evapotranspiration
    TempMean = "T_Q" # Mean Temperature
  ),
  date_start,
  date_end,
  overlay_mode = "aggregate",
  filename = getCachePath(
    list(x, fields, date_start, date_end, overlay_mode),
    "climate_safran",
    ifelse(overlay_mode %in% c("intersect_geometry", "aggregate"), "nc", "RDS")
  ),
  overwrite = FALSE,
  ...
) {
  if (!overwrite && !is.null(filename) && file.exists(filename)) {
    message("Using cached file: ", filename)
    if (toupper(tools::file_ext(filename)) == "RDS") {
      s <- readRDS(filename)
    } else {
      s <- stars::read_mdim(filename)
    }
  } else {
    # Call RADIS to extract the selected fields over a geometry (x) and date range
    s <- RADIS::get_sim2_daily(
      x,
      DATE__greater = date_start,
      DATE__less = date_end,
      fields = unlist(fields),
      overlay_mode = overlay_mode,
      ...
    )

    # Loop through the output variable groups (e.g., Precip, PotEvap, etc.)
    for (GR_field in names(fields)) {
      nb_fields <- length(fields[[GR_field]])

      # Rename the first source variable to match the GR-compatible field name
      names(s)[names(s) == fields[[GR_field]][1]] <- GR_field

      # If multiple source fields are mapped to the same GR field, sum them
      if (nb_fields > 1) {
        for (i in seq(2, nb_fields)) {
          s[[GR_field]][] <- s[[GR_field]][] + s[[fields[[GR_field]][i]]][]
          s[[fields[[GR_field]][i]]] <- NULL
        }
      }
    }
    if (!is.null(filename)) {
      if (toupper(tools::file_ext(filename)) == "RDS") {
        saveRDS(s, filename)
      } else {
        stars::write_mdim(s, filename)
      }
    }
  }

  # Return the final list with renamed and aggregated variables
  return(s)
}
