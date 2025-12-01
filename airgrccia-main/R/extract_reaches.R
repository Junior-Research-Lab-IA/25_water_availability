#' Extract River Reaches by Sub-basin from a DEM
#'
#' This function extracts individual river reaches by tracing flow paths from
#' each sub-basin outlet to the next downstream outlet using a D8 flow direction
#' raster and a stream network.
#'
#' @param sb An [`sf`][sf::st_sf] object representing the perimeter of sub-basins.
#' Typically the output of [extract_sub_basins()].
#' @param d8fd A [`SpatRaster`][terra::SpatRaster] or [character] path to the file containing D8
#' flow directions.
#' Can also be obtained from [extract_sub_basins()].
#' @param streams An [`sf`][sf::st_sf] object or [character] path to the file containing
#' the stream network
#' (e.g., output of [get_ign_streams()]).
#' @param max_tries [integer]. Maximum cells tested for retrieving a reach.
#' @param eps [numeric]. Minimum acceptable distance between the downstream reach
#' and the downstream outlet in cells.
#' @param max_step_back [integer] Maximum contiguous back steps in stream cell research
#' @param max_azimut [integer] Maximum change of orientation in cell research
#' (See details)
#' @param max_buffer [integer] Maximum number of cells matching around stream cells
#' @param dTolerance [numeric]. The tolerance for simplifying the reach geometry (
#' Used in [sf::st_simplify]).
#' @param future_plan [function] One of available future plan: [future::sequential],
#' [future::multisession], [future::multicore], [future::cluster]
#' @param future_workers [integer] Number of workers to use for parallel processing.
#' @inheritParams get_ign_streams
#'
#' @details
#' Type `logger::log_level(logger::DEBUG)` or `logger::log_level(logger::TRACE)`
#' before running the function for getting details on stream cells search.
#' In this mode, maps zooming on issue zones are recorded in the working
#' directory (names "`debug_extract_reaches_bvi*_step*_row*_col*.png`").
#'
#' `max_azimut` correspond to the maximum change of orientation when searching
#' the next stream cell (0, for no change; 2 for 45°; 4 for 90°, 6 for 135° and
#' 7 for 90°).
#'
#' @return An [`sf`][sf::st_sf] object containing one river reach per sub-basin,
#' from the sub-basin outlet to the next downstream outlet.
#'
#' @export
#'
extract_reaches <- function(
  sb,
  d8fd = attr(sb, "files")["d8"],
  streams = attr(sb, "files")["streams"],
  max_tries = 100000,
  eps = 1,
  max_step_back = 10,
  max_azimut = 4,
  max_buffer = 3,
  dTolerance = 2 * terra::res(d8fd),
  future_plan = future::multisession,
  future_workers = get_future_workers(),
  filename = getCachePath(sb, "reaches", "gpkg"),
  overwrite = FALSE
) {
  if (!overwrite && !is.null(filename) && file.exists(filename)) {
    message("Using cached file: ", filename)
    return(sf::read_sf(filename))
  }
  if (logger::log_threshold() >= logger::DEBUG) {
    logger::log_messages()
    logger::log_warnings()
    unlink("debug_extract_reaches_*.png")
  }
  if (!is.character(d8fd)) {
    file_d8 <- whitebox::wbt_file_path(d8fd, shell_quote = FALSE)
  } else {
    file_d8 <- d8fd
    d8fd <- terra::rast(file_d8)
  }
  if (!is.character(streams)) {
    file_streams <- whitebox::wbt_file_path(streams, shell_quote = FALSE)
  } else {
    file_streams <- streams
    streams <- sf::st_read(file_streams)
  }
  stopifnot(terra::crs(d8fd) == terra::crs(streams))

  a_d8 <- as.matrix(d8fd, wide = TRUE)

  # Get row, col from XY coordinates
  i_ups <- which(!is.na(sb$down))
  i_downs <- sapply(i_ups, function(i_up) which(sb$id == sb$down[i_up]))

  x_up <- sb$mvd_outlet_x[i_ups]
  y_up <- sb$mvd_outlet_y[i_ups]
  x_down <- sb$mvd_outlet_x[i_downs]
  y_down <- sb$mvd_outlet_y[i_downs]

  c_up <- terra::colFromX(d8fd, x_up)
  r_up <- terra::rowFromY(d8fd, y_up)
  c_dn <- terra::colFromX(d8fd, x_down)
  r_dn <- terra::rowFromY(d8fd, y_down)

  reaches <- init_reaches_raster(d8fd, streams, max_buffer)

  # Parallel processing setup
  if (!identical(future_plan, future::sequential)) {
    message(sprintf(
      "Running extract_reach on %d cores... This can take a while.",
      future_workers
    ))
    future::plan(future_plan, workers = future_workers)
    FUN_APPLY <- future.apply::future_lapply
  } else {
    message(
      "Running extract_reach sequentially, it's time for a long cofee break..."
    )
    FUN_APPLY <- lapply
  }
  l_reaches <- FUN_APPLY(seq_along(i_ups), function(i) {
    i_up <- i_ups[i]
    i_down <- i_downs[i]
    message(sprintf("Search From %s to %s", sb$id[i_up], sb$id[i_down]))
    reach <- extract_reach(
      i_up,
      a_d8 = a_d8,
      reaches = reaches,
      c_up = c_up[i],
      r_up = r_up[i],
      c_dn = c_dn[i],
      r_dn = r_dn[i],
      max_tries = max_tries,
      eps = eps,
      max_step_back = max_step_back,
      max_azimut = max_azimut,
      max_buffer = max_buffer,
      dTolerance = dTolerance
    )
    if (!is.null(reach)) {
      reach$id <- sb$id[i_up]
      reach$x_up <- sb$mvd_outlet_x[i_up]
      reach$y_up <- sb$mvd_outlet_y[i_up]
      reach$x_down <- sb$mvd_outlet_x[i_down]
      reach$y_down <- sb$mvd_outlet_y[i_down]
    }
    return(reach)
  })
  reaches <- do.call(rbind, l_reaches)
  sf::st_crs(reaches) <- sf::st_crs(streams)

  if (!is.null(filename)) {
    sf::st_write(reaches, filename)
  }

  return(reaches)
}

extract_reach <- function(
  i_up,
  a_d8,
  reaches,
  c_up,
  r_up,
  c_dn,
  r_dn,
  eps,
  max_tries,
  max_step_back,
  max_azimut,
  max_buffer,
  dTolerance
) {
  dist_max <- sqrt((c_up - c_dn)^2 + (r_up - r_dn)^2)
  message(
    sprintf(
      "Reach Upstream (%i, %i) - Downstream (%i, %i). Fly distance = %3.0f cells",
      r_up,
      c_up,
      r_dn,
      c_dn,
      dist_max
    )
  )

  cc <- c_up
  rc <- r_up
  coord_path <- matrix(c(rc, cc), ncol = 2)
  colnames(coord_path) <- c("row", "col")
  tries <- 0
  nb_step_back <- 0
  dist <- min_dist <- 1E9

  while (dist > 0) {
    tries <- tries + 1
    dist <- sqrt((cc - c_dn)^2 + (rc - r_dn)^2)
    min_dist <- min(min_dist, dist)
    if (dist == 0 || (dist > min_dist && dist < eps)) {
      message(
        "Found downstream outlet with dist = ",
        dist,
        " | min dist = ",
        min_dist,
        " | reach length = ",
        nrow(coord_path),
        " (unit: cells)"
      )
      break
    }

    d8 <- a_d8[rc, cc]
    reaches[rc, cc] <- i_up
    rccc <- search_next_stream_cell(
      rc,
      cc,
      a_d8,
      reaches,
      max_azimut,
      max_buffer
    )
    if (all(is.na(rccc))) {
      # Downstream stream cell not found: step back!
      logger::log_debug(
        "Step ",
        tries,
        ": No downstream stream cell found from (",
        rc,
        ", ",
        cc,
        "). Step back."
      )
      if (logger::log_threshold() >= logger::DEBUG) {
        p <- plot_local_stream(a_d8, reaches, rc, cc)
        ggsave(
          sprintf(
            "debug_extract_reaches_bvi%i_step%05d_row%i_col%i.png",
            i_up,
            tries,
            rc,
            cc
          ),
          p,
          width = 8,
          height = 8,
          units = "in",
          dpi = 100
        )
      }
      reaches[rc, cc] <- NA
      coord_path <- coord_path[-nrow(coord_path), , drop = FALSE]
      nb_step_back <- nb_step_back + 1
      if (nb_step_back == max_step_back || nrow(coord_path) == 0) {
        warning(
          "Unable to draw the stream path to downstream outlet. Stopping trace. ",
          "Type `logger::log_threshold(logger::DEBUG)` and rerun the function for details."
        )
        return(NULL)
      }
      rc <- coord_path[nrow(coord_path), 1]
      cc <- coord_path[nrow(coord_path), 2]
    } else {
      # Downstream stream cell found: check stop or continue
      nb_step_back <- 0
      coord_path <- rbind(coord_path, rccc)
      rc <- rccc[1]
      cc <- rccc[2]

      if (tries > max_tries) {
        warning(
          "Max tries reached (",
          max_tries,
          "). Stopping trace. ",
          "Type `logger::log_threshold(logger::DEBUG)` and rerun the function for details."
        )
        break
      }
      if (
        cc <= 1 ||
          rc <= 1 ||
          cc >= ncol(reaches) ||
          rc >= nrow(reaches)
      ) {
        warning(
          "\nStopped at boundary (",
          rc,
          ", ",
          cc,
          ") | dist = ",
          dist,
          " | min dist = ",
          min_dist
        )
        break
      }
    }
  }
  cat("\n")
  r <- terra::rast(reaches, extent = terra::ext(attr(reaches, "ext")))
  xy_path <- terra::xyFromCell(
    r,
    terra::cellFromRowCol(r, row = coord_path[, 1], col = coord_path[, 2])
  )
  reach_geom <- sf::st_linestring(xy_path)
  reach_sfc <- sf::st_sfc(reach_geom)
  reach_sf <- sf::st_sf(geometry = reach_sfc)
  reach_sf <- sf::st_simplify(
    reach_sf,
    preserveTopology = TRUE,
    dTolerance = 2 * mean(attr(reaches, "res"))
  )
  return(reach_sf)
}

search_next_stream_cell <- function(
  rc,
  cc,
  a_d8,
  reaches,
  max_azimut,
  max_buffer
) {
  # Offsets by D8 value (WhiteboxTools style)
  d8_offsets <- list(
    `1` = c(0, 1), # E
    `2` = c(1, 1), # SE
    `4` = c(1, 0), # S
    `8` = c(1, -1), # SW
    `16` = c(0, -1), # W
    `32` = c(-1, -1), # NW
    `64` = c(-1, 0), # N
    `128` = c(-1, 1) # NE
  )

  d8 <- a_d8[rc, cc]
  for (stride in seq(2)) {
    for (buf in seq(max_buffer) * -1) {
      for (i in seq(0, max_azimut)) {
        l2d8 <- mod_wrap(log2(d8) + (-1)^i * floor((i + 1) / 2), 8)
        offset <- d8_offsets[[l2d8 + 1]]
        next_rc <- rc + offset[1] * stride
        next_cc <- cc + offset[2] * stride
        azimut_change <- mod_wrap(
          l2d8 - log2(a_d8[next_rc, next_cc]),
          max_azimut
        )
        if (!is.na(reaches[next_rc, next_cc])) {
          if (reaches[next_rc, next_cc] > 0) {
            # We touch a cell already visited: we should go back now!
            return(NA)
          } else if (
            reaches[next_rc, next_cc] == buf &&
              azimut_change <= max_azimut
          ) {
            # We found a stream cell in the good direction
            logger::log_trace(
              "(",
              rc,
              ", ",
              cc,
              ") stride=",
              stride,
              ": Next cell found rc=",
              next_rc,
              " cc=",
              next_cc,
              " direction=",
              i,
              " buffer=",
              buf,
              " asimut_change=",
              azimut_change
            )
            return(c(next_rc, next_cc))
          }
        }
      }
    }
  }
  return(NA)
}

search_next_stream_cell_v2 <- function(
  rc,
  cc,
  a_d8,
  reaches,
  max_azimut,
  max_buffer,
  azimut_change_threshold = 1
) {
  # Offsets by D8 value (WhiteboxTools style)
  d8_offsets <- list(
    `1` = c(0, 1),
    # E
    `2` = c(1, 1),
    # SE
    `4` = c(1, 0),
    # S
    `8` = c(1, -1),
    # SW
    `16` = c(0, -1),
    # W
    `32` = c(-1, -1),
    # NW
    `64` = c(-1, 0),
    # N
    `128` = c(-1, 1) # NE
  )

  d8 <- a_d8[rc, cc]
  best_azimut_change <- max_azimut + 1
  for (buf in seq(max_buffer) * -1) {
    for (stride in seq(2, 1, -1)) {
      for (i in seq(0, ifelse(stride > 1, 2, max_azimut))) {
        l2d8 <- mod_wrap(log2(d8) + (-1)^i * floor((i + 1) / 2), 8)
        offset <- d8_offsets[[as.character(2^l2d8)]]
        next_rc <- rc + offset[1] * stride
        next_cc <- cc + offset[2] * stride
        azimut_change <- 0.5 *
          ceiling(i / 2) +
          circ_diff(l2d8, log2(a_d8[next_rc, next_cc]), 8) -
          (buf + 1)
        if (!is.na(reaches[next_rc, next_cc])) {
          if (reaches[next_rc, next_cc] > 0) {
            # There is a cell already visited slightly on the front : we should go back now!
            return(NA)
          } else if (
            reaches[next_rc, next_cc] == buf &&
              azimut_change < best_azimut_change
          ) {
            # We found a stream cell in the good direction
            best_azimut_change <- azimut_change
            attr(best_azimut_change, "coords") <- c(next_rc, next_cc)
            if (logger::log_threshold() == logger::TRACE) {
              attr(best_azimut_change, "extras") <-
                c(
                  BAC = best_azimut_change,
                  stride = stride,
                  i = i,
                  buf = buf,
                  l2d8 = l2d8,
                  d8 = log2(d8),
                  nd8 = log2(a_d8[next_rc, next_cc])
                )
            }
            if (best_azimut_change < azimut_change_threshold) {
              break
            }
          }
        }
      }
      if (best_azimut_change < azimut_change_threshold) {
        break
      }
    }
    if (best_azimut_change < azimut_change_threshold) {
      break
    }
  }
  if (is.null(attr(best_azimut_change, "coords"))) {
    return(NA)
  } else {
    extras <- attr(best_azimut_change, "extras")
    extras_str <- paste(names(extras), extras, sep = "=", collapse = ", ")
    logger::log_trace(
      "(",
      rc,
      ", ",
      cc,
      "): ",
      extras_str
    )
    return(attr(best_azimut_change, "coords"))
  }
}

mod_wrap <- function(x, n) {
  ((x %% n) + n) %% n
}

circ_diff <- function(a, b, n) {
  d <- abs(a - b)
  pmin(d, n - d)
}

d8_to_arrow <- function(mat) {
  # Define the lookup table
  lookup <- c(
    "1" = "\u2192",
    # E
    "2" = "\u2198",
    # SE
    "4" = "\u2193",
    # S
    "8" = "\u2199",
    # SW
    "16" = "\u2190",
    # W
    "32" = "\u2196",
    # NW
    "64" = "\u2191",
    # N
    "128" = "\u2197" # NE
  )

  # Apply the lookup
  arrow_mat <- matrix("", nrow = nrow(mat), ncol = ncol(mat))

  for (code in names(lookup)) {
    arrow_mat[mat == as.numeric(code)] <- lookup[[code]]
  }

  # Mark NA or invalid values as blank
  arrow_mat[!mat %in% as.numeric(names(lookup))] <- "\u00B7"

  return(arrow_mat)
}

plot_local_stream <- function(
  a_d8,
  reaches,
  rc,
  cc,
  cell_zoom = 30,
  arrow_size = 0.4 * attr(reaches, "res")[1]
) {
  # Define flow direction vectors
  d8_codes <- c(1, 2, 4, 8, 16, 32, 64, 128)
  dx <- c(1, 1, 0, -1, -1, -1, 0, 1)
  dy <- c(0, 1, 1, 1, 0, -1, -1, -1)
  dir_df <- data.frame(code = d8_codes, dx = dx, dy = dy)
  #Zoom
  r_range <- max(0, rc - cell_zoom):min(dim(a_d8)[1], rc + cell_zoom)
  c_range <- max(0, cc - cell_zoom):min(dim(a_d8)[2], cc + cell_zoom)
  m <- reaches[r_range, c_range]
  d8 <- a_d8[r_range, c_range]
  df_xy <- data.frame(
    x = attr(reaches, "ext")["xmin"] +
      (cc - 1 + rep(seq(ncol(d8)) - 1, each = nrow(d8))) *
        attr(reaches, "res")[1],
    y = attr(reaches, "ext")["ymin"] +
      attr(reaches, "res")[2] *
        (rc - 1 + nrow(d8) - rep(seq(nrow(d8)), ncol(d8)))
  )
  df_streams <- cbind(df_xy, value = as.vector(m))
  df_d8 <- cbind(df_xy, dir = as.vector(d8))
  df_d8 <- merge(df_d8, dir_df, by.x = "dir", by.y = "code")
  ggplot(df_d8, aes(x = x - dx * arrow_size, y = y + dy * arrow_size)) +
    geom_tile(
      data = df_streams,
      aes(
        x = x,
        y = y,
        fill = factor(value)
      ),
      alpha = 0.6
    ) +
    geom_segment(
      aes(xend = x + dx * arrow_size, yend = y - dy * arrow_size),
      arrow = arrow(length = unit(4, "pt")),
      linewidth = 0.3
    ) +
    coord_equal() +
    theme_minimal()
}

init_reaches_raster <- function(d8fd, streams, max_buffer) {
  reaches <- terra::rast(
    terra::ext(d8fd),
    resolution = terra::res(d8fd),
    crs = terra::crs(d8fd)
  )
  stopifnot(is.numeric(max_buffer), max_buffer > 0)
  message("Rasterization of streams...")
  reaches <- terra::rasterize(streams, reaches, field = -1, touches = TRUE)
  if (max_buffer == 1) {
    return(reaches)
  }
  for (buf in seq(max_buffer - 1) * -1) {
    message("Buffering around streams step ", -buf, "/", max_buffer - 1, "...")
    is_neg <- reaches == buf
    neighbors_of_neg <- terra::focal(
      is_neg,
      w = 3,
      fun = sum,
      na.rm = TRUE,
      fillvalue = 0
    )
    to_fill <- is.na(reaches) & (neighbors_of_neg > 0)
    reaches[to_fill] <- buf - 1
  }
  # Convert to matrix for future.apply (SpatRaster is a non exportable C++ objects)
  m <- as.matrix(reaches, wide = TRUE)
  attr(m, "ext") <- as.vector(terra::ext(reaches))
  attr(m, "res") <- terra::res(reaches)
  return(m)
}
