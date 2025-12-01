#' Extract DEM from IGN API
#'
#' This function download DEM from IGN API and perform correction on outliers.
#'
#' @param x A bounding box, a simple feature collection or a simple feature geometry.
#' @param ... Further parameters passed to methods and to [happign::get_wms_raster]
#'
#' @returns A [`SpatRaster`][terra::SpatRaster] object containing the DEM data.
#'
#' @details
#' Default value of `layer` correspond to the BD Alti with 25 m of resolution.
#' Available DEM on IGN API can be explored with the instruction:
#' `happign::get_layers_metadata("wms-r", "altimetrie")`.
#'
#' @rdname get_ign_dem
#' @export
#'
#' @examples
#' # Basic usage with default DEM
#' bbox <- c(xmin = 325522.8, ymin = 6250949.2, xmax = 347137.1, ymax = 6270285.1)
#' dem <- get_ign_dem(sf::st_bbox(bbox, crs = 2154))
#' dem
#' terra::plot(dem)
get_ign_dem <- function(x, ...) {
  UseMethod("get_ign_dem")
}

#' @rdname get_ign_dem
#' @export
get_ign_dem.sf <- function(x, ...) {
  get_ign_dem(sf::st_as_sfc(x), ...)
}

#' @rdname get_ign_dem
#' @export
get_ign_dem.bbox <- function(x, ...) {
  get_ign_dem(sf::st_as_sfc(x), ...)
}

#' @inheritParams happign::get_wms_raster
#' @param neg_outlier_threshold Threshold under which data is rejected as outlier
#' and replaced by the mean of neighbour cells
#' @rdname get_ign_dem
#' @export
get_ign_dem.sfc <- function(x,
                            layer = "ELEVATION.ELEVATIONGRIDCOVERAGE",
                            res = 25,
                            crs = sf::st_crs(x),
                            neg_outlier_threshold = 0,
                            ...) {
  if (! is_projected_crs(x)) {
    stop("Error: The input object has a geographic CRS. ",
         "This function requires an object with a projected CRS. ",
         "Please transform the CRS before using this function.")
  }
  dem <- happign::get_wms_raster(
    x = x,
    layer = layer,
    res = 25,
    crs = crs,
    rgb = FALSE,
    ...
  )
  # Remove negative outliers in the middle of the map
  dem[dem < neg_outlier_threshold] <- NA
  while (any(is.na(terra::values(dem)))) {
    warning("Found ",
            length(which(is.na(
              terra::values(dem)
            ))),
            " negative outliers: data gap have been interpolated")
    dem <- terra::focal(
      dem,
      w = 3,
      fun = mean,
      na.policy = "only",
      na.rm = TRUE
    )
  }
  names(dem) <- "z"
  return(dem)
}


#' Check if the CRS is projected
#'
#' @param x [`sf`][sf::st_sf], [`sf`][sf::st_sfc] geometry, or [`bbox`][sf::st_bbox].
#'
#' @returns `TRUE` if the CRS is projected, `FALSE` otherwise.
#' @export
#'
#' @examples
#' # Define the bounding box coordinates in WGS84
#' bounding_box <- c(xmin = 3.211086, ymin = 43.24153, xmax = 3.904615, ymax = 44.28818)
#'
#' # Create the bounding box using st_bbox
#' bbox <- sf::st_bbox(bounding_box, crs = 4326)
#'
#' # Is it in projected CRS?
#' is_projected_crs(bbox)
#'
#' # Convert to projected CRS
#' bbox_projected <- sf::st_transform(bbox, 2154)
#' is_projected_crs(bbox_projected)
is_projected_crs <- function(x) {
  crs_wkt <- sf::st_crs(x)$wkt

  return(
    grepl("PROJCRS", crs_wkt, ignore.case = TRUE) ||
      grepl("PROJECTEDCRS", crs_wkt, ignore.case = TRUE)
  )
}
