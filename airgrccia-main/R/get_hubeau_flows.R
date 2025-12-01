#' Retrieve river flow observation from the Hydrometrie Hub'Eau API
#'
#' Flow observations are retrieved with [hubeau::get_hydrometrie_obs_elab] and
#' formatted for **airGRiwrm** usage.
#' Station identifiers are used as `code_entite` filter parameter on the
#' Hydrometrie Hub"Eau API and can be either a site (8 characters-length) or a
#' measuring station (10 characters-length) code.
#'
#' @details
#' Hub'Eau APIs are currently limited to 20000 rows so this function cut the time
#' period in `max_days` chunks for the API calls.
#'
#' @param x Either a [character] vector of station codes or a [`SubBasins`][extract_sub_basins] object.
#' @param ... Parameters passed to methods or to [hubeau::get_hydrometrie_obs_elab]
#'
#' @returns A [data.frame] with a first column `DatesR` containing the dates and
#' one column by station containing observed flows in m3/s.
#' The returned object contains an attribute "codes_site_station" listing the
#' correspondance between `code_entite` and station codes.
#' @export
#' @rdname get_hubeau_flows
#'
get_hubeau_flows <- function(x, ...) {
  UseMethod("get_hubeau_flows")
}

#' @export
#' @rdname get_hubeau_flows
get_hubeau_flows.default <- function(x, ...) {
  stop("get_hubeau_flows is not implemented for this class.")
}

#' @export
#' @rdname get_hubeau_flows
get_hubeau_flows.SubBasins <- function(x, ...) {
  stations <- hubeau::get_hydrometrie_sites(code_site = x$id)
  message("Found stations:", paste(stations$code_site, collapse = ", "))
  get_hubeau_flows(stations$code_site)
}

#' @param date_start Date of starting period
#' @param date_end Date of ending period
#' @param max_days Maximum number of days in one API call
#' @param path_cache If not `NULL`, folder path of cached data
#' @param grandeur_hydro_elab Processed hydrometric variable type
#' ("HIXM", "HIXnJ", "QINM", "QINnJ", "QixM", "QIXnJ", "QmM", or "QmnJ")
#' @inheritParams get_ign_streams
#'
#' @export
#' @rdname get_hubeau_flows
get_hubeau_flows.character <- function(
  x,
  date_start,
  date_end,
  grandeur_hydro_elab = "QmnJ",
  max_days = 10000,
  path_cache = getCachePath(
    list(date_start, date_end, grandeur_hydro_elab),
    "hubeau_flows"
  ),
  overwrite = FALSE,
  ...
) {
  # List stations from sites
  x <- setNames(nm = x)
  pb <- cli::cli_progress_bar(
    name = "Listing station codes",
    total = length(x),
    clear = FALSE
  )
  codes_entite_station <- lapply(x, function(xi) {
    cli::cli_progress_update(id = pb)
    codes_station <- get_hubeau_flows_code_station(xi)
    if (is.null(codes_station)) {
      return(data.frame(code_entite = xi, code_station = NA_character_))
    } else {
      return(data.frame(code_entite = xi, code_station = codes_station))
    }
  }) %>% bind_rows()

  cli::cli_progress_done()

  # --- check for missing ones ---
  missing_codes <- codes_entite_station$code_entite[is.na(codes_entite_station$code_station)]

  if (length(missing_codes) > 0) {
    warning(
      "No station found for code_entite: ",
      paste(missing_codes, collapse = ", "),
      " (will try to download flows with these `code_entite` anyway)"
    )
  }

  codes_station <- codes_entite_station$code_station
  codes_station[is.na(codes_station)] <-
    codes_entite_station$code_entite[is.na(codes_station)]
  codes_station <- unique(codes_station)

  # Query API for each station
  dfQ <- lapply(
    codes_station,
    function(code_entite) {
      if (!is.null(path_cache)) {
        dir.create(path_cache, showWarnings = FALSE, recursive = TRUE)
      }
      file_cache <- file.path(path_cache, paste0(code_entite, ".RDS"))
      if (!is.null(path_cache) && file.exists(file_cache) && !overwrite) {
        message("Processing ", code_entite, " from cache: ", file_cache)
        df <- readRDS(file_cache)
      } else {
        message(
          "Downloading station ",
          code_entite,
          " from ",
          date_start,
          " to ",
          date_end
        )
        df <- get_hubeau_flows_time_loop(
          code_entite = code_entite,
          date_start = date_start,
          date_end = date_end,
          grandeur_hydro_elab = grandeur_hydro_elab,
          max_days = max_days,
          ...
        )
        if (nrow(df) > 0) {
          df$code_station[is.na(df$code_station)] <- df$code_site[is.na(df$code_station)]
          if (!is.null(path_cache)) {
            saveRDS(df, file_cache)
          }
        } else {
          warning("No observation found for code_entite ", code_entite)
        }
      }
      return(df)
    }
  ) |>
    dplyr::bind_rows()

  dfQ <- dfQ %>%
    dplyr::mutate(
      DatesR = as.POSIXlt(date_obs_elab),
      id = code_station,
      Q = resultat_obs_elab / 1000 # Conversion from L/s to m3/s
    ) %>%
    dplyr::select(DatesR, id, Q) %>%
    dplyr::arrange(DatesR) %>%
    tidyr::pivot_wider(names_from = id, values_from = Q)
  attr(dfQ, "codes_entite_station") <- codes_entite_station
  return(dfQ)
}

get_hubeau_flows_time_loop <- function(
  code_entite,
  date_start,
  date_end,
  grandeur_hydro_elab = "QmnJ",
  max_days,
  ...
) {
  # ensure dates are Date class
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)

  # create sequence of chunk boundaries
  chunk_starts <- seq(date_start, date_end, by = max_days)
  chunk_ends <- pmin(chunk_starts + (max_days - 1), date_end)

  # loop over chunks
  df_all <- purrr::map2_dfr(
    chunk_starts,
    chunk_ends,
    function(start, end) {
      hubeau::get_hydrometrie_obs_elab(
        code_entite = code_entite,
        date_debut_obs_elab = format(start),
        date_fin_obs_elab = format(end),
        grandeur_hydro_elab = grandeur_hydro_elab,
        fields = c("code_site", "code_station", "date_obs_elab", "resultat_obs_elab"),
        ...
      )
    }
  )

  return(df_all)
}

get_hubeau_flows_code_station <- function(code_entite) {
  if (nchar(code_entite) == 10) {
    return(code_entite)
  } else {
    df <- hubeau::get_hydrometrie_stations(
      code_site = code_entite
    )
    if (nrow(df) > 0) {
      return(df$code_station)
    } else {
      return(NULL)
    }
  }
}
