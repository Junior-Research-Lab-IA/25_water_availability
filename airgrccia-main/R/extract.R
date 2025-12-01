#' Extract values from a raster `stars` object to a vector `sf` object
#'
#' Compute mean area-weighted values of intersections between raster and vector.
#'
#' @inheritParams terra::extract
#'
#' @return A vector `stars` object with extracted values from raster with an
#' attribute "sf_data" which is a copy of the data contained in `y`.
#' @export
#' @import stars sf
#' @importFrom methods as
#' @importFrom stats setNames
#' @importMethodsFrom terra extract
#' @exportMethod extract
#'
setMethod("extract", signature(x="stars", y="sf"),
function(x, y) {
  # Conversion to SpatialPolygons*
  g <- sf::st_geometry(sf::st_as_sf(y))
  sv_y <- terra::vect(y)
  # Compute areas of destination polygons
  y$destination_area <- terra::expanse(sv_y, unit="m", transform=TRUE)
  # Intersection between the 2 layers, area proportion of destination polygons
  r_mesh <- as(x[1,,,1], "SpatRaster")
  terra::values(r_mesh) <- seq(terra::ncell(r_mesh))
  names(r_mesh) <- "ncell"
  r_mesh$x <- matrix(rep(seq(dim(r_mesh)[2]), each = dim(r_mesh)[1]), ncol = dim(r_mesh)[2])
  r_mesh$y <- matrix(rep(rev(seq(dim(r_mesh)[1])), dim(r_mesh)[2]), ncol = dim(r_mesh)[2])
  logger::log_info("extract from SpatRaster to SpatVector")
  df_inter <- raster::extract(r_mesh, sv_y, exact = TRUE)
  logger::log_info("found ", nrow(df_inter), " intersections")

  l <- lapply(setNames(nm = names(x)), function(v) {
    logger::log_info("populate stars object with attribute: ", v)
    a <- x[[v]][,,]
    l <- lapply(seq(dim(y)[[1]]), function(i) {
      df <- df_inter[df_inter$ID == i, ]
      f <- a[1, 1, ]
      f[] <- 0
      valid_rows <- which(sapply(seq(nrow(df)), function(j) all(!is.na(a[df$x[j], df$y[j], ]))))
      df$fraction[valid_rows] <- df$fraction[valid_rows] / sum(df$fraction[valid_rows]) # Scale weights to 1
      for (j in valid_rows) {
        f <- f + a[df$x[j], df$y[j], ] * df$fraction[j]
      }
      return(f)
    })
    m <- do.call(rbind, l)
  })

  # Create stars object from description https://r-spatial.github.io/stars/articles/stars4.html
  # and from https://github.com/r-spatial/stars/blob/1ee07b698e79a5df85790d85c1a15002c71ef937/R/cubble.R#L30
  d = stars:::create_dimensions(list(
    geometry = stars:::create_dimension(values = st_geometry(y)),
    time = st_dimensions(x)$time))
  s <- stars::st_as_stars(l, dimensions = d)
  attr(s, "sf_data") <- sf::st_drop_geometry(y)
  attr(s, "raster_extract") <- df_inter
  return(s)
})
