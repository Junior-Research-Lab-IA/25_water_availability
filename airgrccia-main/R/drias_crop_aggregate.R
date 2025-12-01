#' Create a stars object for one DRIAS projection
#'
#' DRIAS data are downloaded for each requested variable, the study area is
#' cropped and all variables are merged into one stars object.
#'
#' @details
#' URLs for downloading NetCDF files are picked from the `drias_GCM_RCM_scenario_variables.tsv`
#' file stored on Nextcloud and created by [drias_list_remote].
#'
#' @param GCM [character] Selected GCM
#' @param RCM [character] Selected RCM
#' @param BC [character] Selected Bias Correction
#' @param scenario [character] Selected scenario ("historical", "rcp2.6", "rcp4.5", or "rcp8.5")
#' @param bbox [vector] of bounding box coordinates in the form
#' `c(xmin, ymin, xmax, ymax)` in the Lambert II CRS ("EPSG:27572").
#' @param drias [data.frame] containing the DRIAS URLs and metadata.
#' @inheritParams drias_list
#' @inheritParams read_drias
#' @param variables [character] [vector] of variables to merge
#'
#' @return A stars object cropped to the study area with all selected variables for the
#' selected GCM/RCM/scenario.
#'
#' @export
#' @rdname drias_crop_aggregate
#' @import dplyr
#' @import stars
#'
#' @examples
#' \dontrun{
#' nc <- drias_crop_aggregate("CNRM-CERFACS-CNRM-CM5",
#'                            "CNRM-ALADIN63",
#'                            "ADAMONT",
#'                            "historical",
#'                            c("prtot", "evspsblpot"),
#'                            bbox)
#' nc
#' # Dimensions of the object are [variable, x, y, date]
#' # Let's plot the first time steps for the wind speed
#' plot(nc["sfcWindAdjust",,,1:66])
#' }
drias_crop_aggregate <- function(
  GCM,
  RCM,
  BC,
  scenario,
  variables = cfg$climato$drias$variables,
  bbox,
  drias = drias_list(cfg = cfg, with_variables = TRUE),
  path = tempdir(),
  cfg = loadConfig()
) {
  stopifnot(
    length(GCM) == 1,
    length(RCM) == 1,
    length(BC) == 1,
    length(scenario) == 1,
    length(variables) > 0,
    length(bbox) == 4
  )

  drias <- drias %>%
    filter(
      GCM == .env$GCM,
      RCM == .env$RCM,
      BC == .env$BC,
      scenario == .env$scenario,
      variable %in% variables
    )

  l <- lapply(seq(nrow(drias)), function(i) {
    url <- drias$url[i]
    s <- read_drias(url, path = path)
    s <- drias_crop(s, bbox)
    names(s) <- sub("Adjust", "", names(s))
    if (names(s) == "evspsblpot") {
      names(s) <- paste(names(s), drias$ETP[i], sep = "_")
    }
    if (grepl("(prtot|evspsblpot)", names(s))) {
      # Convert kg/m2/s to mm/d
      s <- s * 86400
    }
  })
  s <- do.call(c, l)

  var_units <- c(
    prtot = "mm/d",
    evspsblpot_FAO = "mm/d",
    evspsblpot_Hg0175 = "mm/d",
    tas = "\u00B0C", # Convert K to \u00B0C
    tasmin = "\u00B0C",
    tasmax = "\u00B0C",
    rlds = "W/m2",
    rsds = "W/m2",
    sfcWind = "m/s"
  )
  # Correction wrong units for radiation and wind speed
  var_drop_units <- c("rlds", "rsds", "sfcWind")

  for (i in grep(paste(variables, collapse = "|"), names(var_units))) {
    v <- names(var_units[i])
    if (v %in% var_drop_units) {
      s[[v]] <- units::drop_units(s[[v]])
    }
    s[[v]] <- units::set_units(s[[v]], var_units[v], mode = "standard")
  }
  return(s)
}

drias_crop <- function(s, bbox, eps = 0.1) {
  x <- stars::st_get_dimension_values(s, 'x', where = "center")
  y <- stars::st_get_dimension_values(s, 'y', where = "center")
  i_x <- which(x > bbox$xmin - eps & x < bbox$xmax + eps)
  i_y <- which(y > bbox$ymin - eps & y < bbox$ymax + eps)
  s[, i_x, i_y, ]
}

#' Read a NetCDF file from DRIAS2020 database
#'
#' This function downloads a netCDF file, open it with [stars::read_mdim],
#' correct the CRS by setting it to Lambert II , and returns
#' the modified stars object.
#'
#' @details
#' The undefined CRS that is corrected here is due to a wrong format of the netCDF
#' or a bug in GDAL, see https://github.com/r-spatial/stars/issues/628#issuecomment-1537946005
#' for details.
#'
#' @param url [character] URL of the file for downloading
#' @param filename [character] Name of the file to download
#' @param path [character] Local path of the downloaded file
#' @param crs [character] for replacing the `crs` attributes of the `stars` object
#'
#' @return A `stars` object (See stars::read_mdim)
#' @export
#' @importFrom utils tail
#'
#' @examples
#' \dontrun{
#' s <- read_drias("https://climatedata.umr-cnrm.fr/public/dcsc/projects/DRIAS/DRIAS2020_NEW/CNRM-CERFACS-CNRM-CM5/CNRM-ALADIN63/historical/day/prtotAdjust/prtotAdjust_France_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CNRM-ALADIN63_v2_MF-ADAMONT-SAFRAN-1980-2011_day_19510101-20051231.nc")
#' s
#' }
read_drias <- function(
  url = NULL,
  filename = basename(url),
  path = tempdir(),
  crs = "EPSG:27572"
) {
  file_tmp <- tempfile(fileext = ".tmp.nc")
  file_path <- file.path(path, filename)
  on.exit(unlink(file_tmp, force = TRUE))
  if (!file.exists(file_path)) {
    logger::log_info("Downloading ", filename)
    utils::download.file(url, destfile = file_tmp, mode = "wb")
    file.rename(file_tmp, file_path)
    logger::log_info(filename, " successfully downloaded.")
  }
  # We use read_mdim instead of read_stars or read_ncdf because they are not ready for multi time steps
  s <- stars::read_mdim(file_path)
  # Clear uncorrect CRS in order to be able to redefine it
  crs_NA <- list(input = as.character(NA), wkt = as.character(NA))
  attr(s, "dimensions")$x$refsys <- crs_NA
  attr(s, "dimensions")$y$refsys <- crs_NA
  sf::st_crs(s) <- crs
  return(s)
}

#' @rdname drias_crop_aggregate
#' @export
drias_crop_agg_save <- function(
  GCM,
  RCM,
  BC,
  scenario,
  variables,
  bbox,
  path = getwd(),
  cfg = loadConfig()
) {
  logger::log_appender(logger::appender_file("drias_crop_agg_save.log"))
  logger::log_info("Starting job: ", paste(GCM, RCM, BC, scenario, sep = ", "))
  filename <- paste0(
    paste(GCM, RCM, BC, scenario, sep = "_"),
    ".nc"
  )
  filepath <- file.path(path, filename)
  if (file.exists(filepath)) {
    invisible()
  }
  n_try <- 100
  nc <- FALSE
  while (!inherits(nc, "stars") & n_try > 0) {
    nc <- tryCatch(
      {
        logger::log_info(
          "Aggregating ",
          paste(GCM, RCM, BC, scenario, sep = ", "),
          " (remaining ",
          n_try,
          " tries)"
        )
        drias_crop_aggregate(GCM, RCM, BC, scenario, variables, bbox, cfg = cfg)
      },
      error = function(e) {
        logger::log_error(e$message)
        return(FALSE)
      }
    )
    n_try <- n_try - 1
  }
  if (inherits(nc, "stars")) {
    logger::log_info("write_mdim: ", filepath)
    stars::write_mdim(nc, filepath)
    # Clean disk space
    unlink(list.files(tempdir(), "*.nc", full.names = TRUE))
  }
  invisible()
}
