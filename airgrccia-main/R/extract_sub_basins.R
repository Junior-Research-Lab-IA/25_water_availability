#' Extract sub-basins from DEM
#'
#' @describeIn extract_sub_basins extracts sub-basins from a DEM using whitebox tools.
#'
#' @details
#' `extract_sub_basins` delineate the watershed from the DEM for each outlet,
#' then it computes the sub-basins contour by remove overlapping basin areas and
#' compute which sub-basin is downstream of which other sub-basin.
#'
#' `watershed_get_from_outlet` delineate one basin given outlet coordinates.
#' One can provide an a priori area in `outlets$area` which is used to control
#' the area of the resulting polygon. If the resulting area is outside the tolerance
#' (`area_tolerance`), the algorithm search for a closer point (at a maximum distance
#' of `max_radius` cells of the DEM raster) on the stream network and delineate
#' again the basin.
#' This is repeated until the area is within the tolerance or `max_tries` is reached.
#'
#' @param outlets [data.frame] with columns "id", "x", and "y" and optionnaly "area"
#' containing respectively the outlet ID, X and Y coordinates (in the CRS of the DEM)
#' and the a priori area of the sub-basin (in square meters) (See Details)
#' @param dem [character] containing the filename of the DEM raster, or
#' a [`SpatRaster`][terra::SpatRaster] object
#' @param streams A [`sf`][sf::st_sf] object containing the streams.
#' @param crs The CRS used for outlets coordinates. If `NULL`, the CRS of `dem`
#' is used
#' @param threshold Minimum threshold of contributing area to extract streams
#' (unit: square meters)
#' @param snap_dist Maximum distance between outlet and a stream vector for
#' relocating outlet in the stream (in meters)
#' @param max_radius Maximum search radius (in number of cells) to find a closer point
#' @param area_tolerance Maximum relative error between the a priori area
#' and the computed area of the sub-basin
#' @param filename [character] Optional. If provided, the sub-basins geometry will
#' be saved to this file and can be used as cache if `overwrite = FALSE`.
#' If `TRUE` (default), a filename will be automatically generated with
#' [getCachePath] based on the input parameters.
#' @inheritParams get_ign_streams
#' @inheritParams whitebox::wbt_fill_burn
#'
#' @return `extract_sub_basins` returns a [`sf`][sf::st_sf] object with the
#' polygons representing the sub-basins and various attributes detailling the
#' sub-basin up-down order, total basin and sub-basin areas, coordinates of
#' outlets after move.
#'
#' @export
#' @import sf
#'
extract_sub_basins <- function(
  outlets,
  dem = NULL,
  streams,
  crs = NULL,
  threshold = 1000,
  snap_dist = 100,
  area_tolerance = 0.05,
  max_radius = 50,
  filename = TRUE,
  overwrite = FALSE,
  verbose_mode = NULL
) {
  # Check whitebox tools installation
  if (!whitebox::check_whitebox_binary()) {
    whitebox::install_whitebox()
  }

  # Checks on DEM
  if (is.null(dem)) {
    stop("dem == NULL is not handled. Please provide a dem")
  } else if (is.character(dem)) {
    f <- c(dem = dem)
    elev <- terra::rast(dem)
  } else {
    stopifnot(inherits(dem, "SpatRaster"))
    elev <- dem
    f <- c(dem = getCachePath(outlets, "0-dem", "tif"))
    if (file.exists(f["dem"]) && !overwrite) {
      logger::log_info("Using cached DEM file: ", f["dem"])
    } else {
      terra::writeRaster(elev, filename = f["dem"], overwrite = TRUE)
    }
  }
  if (is.null(crs)) {
    if (is.null(terra::crs(elev))) {
      stop("The CRS of the DEM is not defined. Please provide a CRS.")
    }
    crs <- terra::crs(elev)
  }
  if (is.null(st_crs(crs)$units)) {
    stop(
      "Parameter `dem`: The raster is using a geographic coordinate system.\n",
      "Only projected coordinate system are handled by the function."
    )
  }
  ext <- as.vector(terra::ext(elev))

  # Check outlets
  stopifnot(
    is.data.frame(outlets),
    "id" %in% names(outlets),
    "x" %in% names(outlets),
    "y" %in% names(outlets),
    is.character(outlets$id),
    is.numeric(outlets$x),
    is.numeric(outlets$y)
  )
  if ("area" %in% names(outlets)) {
    stopifnot(is.numeric(outlets$area))
  }

  # Define cache file names
  cache_hash_input <- list(
    ext,
    crs,
    threshold,
    snap_dist,
    terra::res(elev),
    streams
  )
  if (!is.null(filename) && is.logical(filename) && filename) {
    filename <- getCachePath(cache_hash_input, "sub_basins", "rds")
  }
  if (!is.null(filename) && tolower(tools::file_ext(filename)) != "rds") {
    stop("Parameter `filename`: Cache file only supports 'rds' file extension")
  }

  if (!overwrite && !is.null(filename) && file.exists(filename)) {
    logger::log_info("Using cached file: ", filename)
    return(readRDS(filename))
  }
  # Generate paths for all rasters and shape files
  for (raster_step in c(
    "burn",
    "out_dem",
    "d8",
    "flow_accum",
    "flow_accum_ca",
    "flow_path"
  )) {
    f[[raster_step]] <- getCachePath(cache_hash_input, raster_step, "tif")
  }
  for (sf_step in c("streams", "outlets", "moved_outlets")) {
    f[[sf_step]] <- getCachePath(cache_hash_input, sf_step, "shp")
  }

  streams <- sf::st_transform(sf::st_as_sf(streams), crs)
  streams <- sf::st_as_sf(sf::st_as_sfc(streams))
  streams$num <- seq(nrow(streams))
  sf::st_write(streams, f["streams"], append = FALSE)

  if (
    !overwrite &&
      all(file.exists(f[c(
        "burn",
        "out_dem",
        "d8",
        "flow_accum",
        "flow_accum_ca",
        "flow_path",
        "streams",
        "outlets",
        "moved_outlets"
      )]))
  ) {
    logger::log_info(
      "Using cached whitebox files: \n- ",
      paste(
        f[c(
          "burn",
          "out_dem",
          "d8",
          "flow_accum",
          "flow_accum_ca",
          "flow_path",
          "streams",
          "outlets",
          "moved_outlets"
        )],
        collapse = "\n- "
      )
    )
  } else {
    logger::log_info("Starting whitebox tools wbt_fill_burn...")
    whitebox::wbt_fill_burn(
      dem = f["dem"],
      streams = f["streams"],
      output = f["burn"],
      verbose_mode = verbose_mode
    )

    # Extract streams using specific contributed area
    logger::log_info(
      "Starting whitebox tools wbt_flow_accumulation_full_workflow 1/2..."
    )
    whitebox::wbt_flow_accumulation_full_workflow(
      dem = f["burn"],
      out_dem = f["out_dem"],
      out_pntr = f["d8"],
      out_accum = f["flow_accum"],
      verbose_mode = verbose_mode
    )
    logger::log_info("Starting whitebox tools wbt_extract_streams...")
    whitebox::wbt_extract_streams(
      flow_accum = f["flow_accum"],
      output = f["flow_path"],
      threshold = threshold,
      verbose_mode = verbose_mode
    )
    # srStreams <- terra::rast(f["flow_path"])

    p.sf <- sf::st_as_sf(outlets, coords = c("x", "y"), crs = crs)
    p.sf$area <- NULL
    sf::write_sf(p.sf, f["outlets"])
    logger::log_info("Starting whitebox tools wbt_jenson_snap_pour_points...")
    whitebox::wbt_jenson_snap_pour_points(
      pour_pts = f["outlets"],
      streams = f["flow_path"],
      output = f["moved_outlets"],
      snap_dist = snap_dist,
      verbose_mode = verbose_mode
    )
    logger::log_info(
      "Starting whitebox tools wbt_flow_accumulation_full_workflow 2/2..."
    )
    whitebox::wbt_flow_accumulation_full_workflow(
      dem = f["burn"],
      out_dem = f["out_dem"],
      out_pntr = f["d8"],
      out_accum = f["flow_accum_ca"],
      out_type = "ca", # contributing area
      verbose_mode = verbose_mode
    )
  }
  sf_mvd_outlets <- sf::read_sf(f["moved_outlets"])
  sf_mvd_outlets <- sf_mvd_outlets |>
    dplyr::mutate(
      x = sf::st_coordinates(geometry)[, 1],
      y = sf::st_coordinates(geometry)[, 2],
    )
  if (!is.null(outlets$area)) {
    sf_mvd_outlets <- sf_mvd_outlets |>
      dplyr::left_join(
        outlets[, c("id", "area")],
        by = "id"
      )
  }

  # Delineate basin extents
  r_flow_accum <- terra::rast(f["flow_accum_ca"])

  list_sf_basins <- lapply(
    seq(nrow(sf_mvd_outlets)),
    watershed_get_from_outlet,
    outlets = sf_mvd_outlets,
    file_d8 = f["d8"],
    r_flow_accum = r_flow_accum,
    crs = crs,
    area_tolerance = area_tolerance,
    max_radius = max_radius,
    verbose_mode = verbose_mode,
    overwrite = overwrite
  )
  sf_b <- do.call(rbind, list_sf_basins)
  suppressWarnings(st_crs(sf_b) <- crs)
  if (any(names(sf_b) == "geom")) {
    names(sf_b)[names(sf_b) == "geom"] <- "geometry"
    sf_b <- sf::st_set_geometry(sf_b, "geometry")
  }

  sf_b <- multipolygons_to_polygons(sf_b)
  sf_b$tot_area <- sf::st_area(sf_b)

  logger::log_info(
    "Delineate sub-basin extents by removing overlapping basin areas..."
  )
  area_order <- order(sf_b$tot_area)
  sb <- sf_b[area_order[1], ]
  for (i in area_order[-1]) {
    intersection <- sf::st_intersection(st_geometry(sb), st_geometry(sf_b[i, ]))
    if (length(intersection) != 0) {
      # If the intersection is not empty, we need to remove it from the sub-basin
      intersection <- st_combine(intersection)
      sfc_sub <- st_difference(st_geometry(sf_b[i, ]), intersection)
      sf_sub <- st_sf(st_drop_geometry(sf_b[i, ]), geometry = sfc_sub)
      sb <- rbind(sb, sf_sub)
    } else {
      # If the intersection is empty, we can add the sub-basin
      sb <- rbind(sb, sf_b[i, ])
    }
  }
  sb <- multipolygons_to_polygons(sb)
  sb$area <- sf::st_area(sb)
  sb$outlet_x <- outlets$x[area_order]
  sb$outlet_y <- outlets$y[area_order]
  sb$mvd_dist <- sqrt(
    (sb$outlet_x - sb$mvd_outlet_x)^2 + (sb$outlet_y - sb$mvd_outlet_y)^2
  )

  logger::log_info("Compute downstream sub-basins of each sub-basin...")
  sf_mvd_outlets <- sf::st_as_sf(
    sf::st_drop_geometry(sb)[, c("id", "mvd_outlet_x", "mvd_outlet_y")],
    coords = c("mvd_outlet_x", "mvd_outlet_y"),
    crs = crs
  )
  buf_mvd_outlets <- sf::st_buffer(
    sf_mvd_outlets,
    dist = 1.5 * terra::res(elev)[1]
  )
  sb_intersects <- st_intersects(buf_mvd_outlets, sb)
  sb$down <- sb$id[sapply(sb_intersects, "[", 2)]
  sb$down[sb$down == sb$id] <- NA_character_
  class(sb) <- append(class(sb), "SubBasins", 2)
  attr(sb, "files") <- f

  if (!is.null(filename)) {
    saveRDS(sb, filename)
  }

  return(sb)
}


#' @describeIn extract_sub_basins Extract one watershed for one outlet
#'
#' @param i [integer] row number to process in `outlets`
#' @param file_d8 Path of raster D8
#' @param r_flow_accum Raster of flow accumulation
#'
#' @export
watershed_get_from_outlet <- function(
  i,
  outlets,
  file_d8,
  r_flow_accum,
  crs,
  area_tolerance = 0.05,
  max_radius = 50,
  verbose_mode = NULL,
  overwrite = FALSE
) {
  logger::log_info(
    "Starting delineation of watershed: ",
    outlets$id[i],
    " - ",
    i,
    " / ",
    nrow(outlets)
  )
  cache_hash_input <- list(file_d8, outlets[i, ])
  file_sb <- getCachePath(
    cache_hash_input,
    paste("watershed", outlets$id[i], sep = "_"),
    "tif"
  )

  if ("area" %in% names(outlets)) {
    area_apriori <- outlets$area[i]
  } else {
    area_apriori <- NA
  }

  file_outlet <- getCachePath(
    cache_hash_input,
    paste("outlet", outlets$id[i], sep = "_"),
    "shp"
  )
  pt_rc <- c(outlets$x[i], outlets$y[i])

  if (overwrite || !file.exists(file_sb)) {
    cache_used <- FALSE
    if (
      !is.na(area_apriori) &&
        area_apriori > 0
    ) {
      logger::log_info(
        "Searching for a closer point on the stream network with target area ",
        round(area_apriori / 1E6, 2),
        " km²..."
      )
      # Get row/col of outlet
      cell_id <- terra::cellFromXY(r_flow_accum, st_coordinates(outlets[i, ]))
      rc <- terra::rowColFromCell(r_flow_accum, cell_id)
      result <- find_near_target(
        r_flow_accum = r_flow_accum,
        row0 = rc[1],
        col0 = rc[2],
        target = area_apriori,
        area_tolerance = area_tolerance,
        max_radius = max_radius
      )
      # Convert row/col to spatial coordinates
      pt_rc <- terra::xyFromCell(
        r_flow_accum,
        terra::cellFromRowCol(r_flow_accum, result$row, result$col)
      )
      logger::log_info(
        "Found a closer point at row/col: ",
        result$row,
        "/",
        result$col,
        " (Radius: ",
        result$radius,
        " cells)"
      )
    }
    logger::log_info(
      "Use coordinates (",
      paste(round(pt_rc), collapse = ", "),
      ") as outlet"
    )
    if (overwrite || !file.exists(file_outlet)) {
      pt_sf <- sf::st_as_sf(
        data.frame(x = pt_rc[1], y = pt_rc[2]),
        coords = c("x", "y"),
        crs = crs
      )
      sf::write_sf(pt_sf, file_outlet)
    }

    whitebox::wbt_watershed(
      d8_pntr = file_d8,
      pour_pts = file_outlet,
      output = file_sb,
      verbose_mode = verbose_mode
    )
  } else {
    cache_used <- TRUE
    logger::log_info("Read cached file: ", file_sb)
  }
  wtrshd <- terra::rast(file_sb)
  file_sfb <- getCachePath(
    cache_hash_input,
    paste("watershed", outlets$id[i], sep = "_"),
    "gpkg"
  )
  if (!cache_used || !file.exists(file_sfb)) {
    # Convert to sf
    sfB <- sf::st_as_sf(terra::as.polygons(wtrshd))
    sfB[[1]] <- NULL
    sfB$id <- outlets$id[i]
    sfB$area <- sf::st_area(sfB) |> units::drop_units()
    sfB$mvd_outlet_x <- pt_rc[1]
    sfB$mvd_outlet_y <- pt_rc[2]
    sfB$apriori_area <- area_apriori
    sf::st_write(sfB, file_sfb, append = FALSE, quiet = TRUE)
  } else {
    sfB <- sf::st_read(file_sfb, quiet = TRUE)
  }
  logger::log_info(
    "Computed area / A priori area / Relative error: ",
    round(sfB$area / 1E6, 2),
    " km² / ",
    ifelse(
      is.na(area_apriori),
      "NA",
      paste0(
        round(
          area_apriori / 1E6,
          2
        ),
        " km²"
      )
    ),
    " / ",
    ifelse(
      is.na(area_apriori),
      "NA",
      paste0(
        round(
          100 * abs(1 - sfB$area / area_apriori),
          2
        ),
        " %"
      )
    )
  )
  return(sfB)
}

multipolygons_to_polygons <- function(sf_b) {
  if (any(sf::st_geometry_type(sf_b) == "MULTIPOLYGON")) {
    logger::log_info("Multipolygons detected. Converting to polygons...")
    sf_b <- sf_b %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_cast("POLYGON", warn = FALSE) %>%
      # Add area column
      mutate(mp_area_m2 = sf::st_area(.)) %>%
      # Keep only the largest polygon per original feature
      group_by(id) %>%
      filter(mp_area_m2 == max(mp_area_m2)) %>%
      ungroup() %>%
      mutate(
        new_area = sf::st_area(.),
        area_lost = (mp_area_m2 - new_area) / mp_area_m2
      )
    logger::log_info(
      "Area lost in the polygon conversion: ",
      round(sum(sf_b$area_lost) * 100, 2),
      "%"
    )
    sf_b <- sf_b %>% select(-mp_area_m2, -area_lost, -new_area)
  }
  return(sf_b)
}

#' @describeIn extract_sub_basins Find a cell near (row0, col0) in the flow
#' accumulation raster cells with a value close to a target area.
#' @param row0,col0 respectively row and column number of the current coordinates
#' of the outlet in the raster
#' @param target Target area to reach (in square meters)
#' @export
find_near_target <- function(
  r_flow_accum,
  row0,
  col0,
  target,
  area_tolerance,
  max_radius = 5
) {
  l_closest_area <- NULL
  for (radius in 0:max_radius) {
    # Define bounding box of the search square
    rows <- (row0 - radius):(row0 + radius)
    cols <- (col0 - radius):(col0 + radius)

    # Get all row/col combinations on the border of this square
    border_rc <- expand.grid(row = rows, col = cols)
    border_rc <- border_rc[
      border_rc$row %in%
        c(min(rows), max(rows)) |
        border_rc$col %in% c(min(cols), max(cols)),
    ]

    # Convert row/col to cell IDs
    cell_ids <- terra::cellFromRowCol(
      r_flow_accum,
      border_rc$row,
      border_rc$col
    )

    # Get values
    vals <- terra::values(r_flow_accum)[cell_ids]

    # Compute relative error
    rel_err <- abs(1 - vals / target)
    if (
      is.null(l_closest_area) ||
        l_closest_area$rel_err > min(rel_err)
    ) {
      idx <- which.min(rel_err) # pick best match
      l_closest_area <- list(
        row = border_rc$row[idx],
        col = border_rc$col[idx],
        value = vals[idx],
        rel_err = rel_err[idx],
        radius = radius
      )
      logger::log_debug(
        "  Radius: ",
        radius,
        " - Closest area (",
        l_closest_area$row,
        ", ",
        l_closest_area$col,
        "): ",
        round(l_closest_area$value / 1E6, 2),
        " km²",
        " - Relative error: ",
        round(100 * l_closest_area$rel_err, 2),
        " %"
      )
    }

    # Check area_tolerance
    if (any(rel_err < area_tolerance, na.rm = TRUE)) {
      return(l_closest_area)
    }
  }
  logger::log_warn(
    "No cell found within the area tolerance after ",
    max_radius,
    " iterations. Returning the closest cell found."
  )
  return(l_closest_area)
}

plot_outlet <- function(rc, r_flow_accum, sfB, buffer_cells = 1000) {
  # Convert row/col to spatial coordinates
  pt_rc <- terra::xyFromCell(
    r_flow_accum,
    terra::cellFromRowCol(r_flow_accum, rc[1, 1], rc[1, 2])
  )
  pt_sf <- sf::st_as_sf(
    data.frame(x = pt_rc[1], y = pt_rc[2]),
    coords = c("x", "y"),
    crs = terra::crs(r_flow_accum)
  )

  # Define buffer size in cells -> convert to map units (25 m resolution)
  buffer_m <- buffer_cells * terra::res(r_flow_accum)[1] # 200 * 25 = 5000 m

  # Make a square window around rc
  ext_rc <- terra::ext(
    pt_rc[1] - buffer_m,
    pt_rc[1] + buffer_m,
    pt_rc[2] - buffer_m,
    pt_rc[2] + buffer_m
  )

  # Crop raster
  r_flow_accum_crop <- terra::crop(r_flow_accum, ext_rc)

  # calcul des quantiles sur le raster
  # quantiles sur une seule couche raster
  q <- unique(as.numeric(
    terra::global(
      r_flow_accum_crop,
      fun = quantile,
      probs = seq(0, 1, length.out = 11),
      na.rm = TRUE
    )[1, ]
  ))

  # carte
  otmap_mode <- tmap::tmap_mode("view")
  tm <- tmap::tm_basemap("OpenStreetMap.France") +
    tmap::tm_shape(r_flow_accum_crop) +
    tmap::tm_raster(
      col.scale = tmap::tm_scale(
        values = "brewer.rd_yl_gn",
        # palette
        breaks = q # coupures selon les quantiles
      ),
      col.legend = tmap::tm_legend(title = "Flow accumulation"),
      col_alpha = 0.5
    ) +
    tmap::tm_shape(sfB) +
    tmap::tm_borders(col = "red", lwd = 2) +
    tmap::tm_shape(pt_sf) +
    tmap::tm_symbols(col = "black", size = 0.5, shape = 19)
  tmap::tmap_mode(otmap_mode)
  return(tm)
}
