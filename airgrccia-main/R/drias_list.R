#' List of netCDF file from DRIAS2020/Explore2 database
#'
#' `drias_list` returns the list of the netCDF files processed for the Aude basin.
#' `drias_list_remote` returns the list of netCDF files available on the CNRM server
#' for the whole metropolitan France territory.
#'
#' @details
#' This function is used by the script `data-raw/drias_GCM_RCM_scenario_variables.R`
#' for creating the file `drias_GCM_RCM_scenario_variables.tsv` stored on Nextcloud.
#'
#' @param url [character] URLs where to search NetCDF files
#' @param nc_pattern Pattern
#' @param method [character] the bias correction method ("ADAMONT", "CDFt", or "(ADAMONT|CDFt)" for both)
#' @param ignore_pattern [character] regex pattern for files or path to ignore
#' @param cfg a config object. Configuration to use. See [loadConfig] for details
#'
#' @return A [data.frame] with the following columns:
#' - for `drias_list_remote`: GCM, RCM, scenario, time_step, parameter, file, url
#' - for `drias_list`: if called with argument `with_variables=FALSE`, one row by scenario,
#' and columns GCM, RCM, bias\_correction, scenario, gcm\_rcm, and file. If called
#' with argument `with_variables=FALSE`, same output as `drias_list_remote`
#' except that rows containing "_OLD" are filtered and with extra columns:
#' ETP ("FAO" or "Hg0175"), and bias\_correction ("ADAMONT" or "CDFt")
#'
#' @export
#' @rdname drias_list
#'
#' @examples
#' # The function fetches the file list from the CNRM server...
#' \dontrun{
#' df <- drias_list_remote()
#' str(df)
#' }
#'
drias_list_remote <- function(
    url = file.path(cfg$climato$drias$url,
                    cfg$climato$drias$folders),
    nc_pattern = ".*\\.nc",
    method = "(ADAMONT|CDFt)",
    ignore_pattern = sprintf("(%s)",
                             paste(cfg$climato$drias$ignore_pattern,
                                   collapse = "|")),
    cfg = loadConfig()) {

  urls <- drias_list_remote_loop(
    url,
    nc_pattern = nc_pattern,
    method = method,
    ignore_pattern = ignore_pattern
  )

  n_url <- length(strsplit(url[1], "/")[[1]])
  s <- lapply(strsplit(urls, "/"), function(x) {
    as.data.frame(matrix(x[(n_url+1):length(x)], nrow = 1))
  })
  df <- do.call(rbind, s)
  df <- cbind(df, urls)
  names(df) <- c("GCM", "RCM", "scenario", "time_step", "variable", "file", "url")
  df$variable <- sub("Adjust", "", df$variable)
  return(df)
}

drias_list_remote_loop <- function(url, nc_pattern, method, ignore_pattern) {
  lapply(
    url,
    drias_list_remote_one,
    nc_pattern = nc_pattern,
    method = method,
    ignore_pattern = ignore_pattern
  ) %>% unlist(use.names = FALSE)
}

drias_list_remote_one <- function(url, nc_pattern, method, ignore_pattern) {
  message("fetching ", url, "...")
  items <- rvest::read_html(url) %>%
    rvest::html_elements(css = "li") %>%
    rvest::html_text()
  items <- trimws(items)
  items <- items[items != "Parent Directory"]
  items <- items[!grepl(ignore_pattern, items)]
  items_to_fetch <- items[!grepl(nc_pattern, items)]
  if (length(items_to_fetch) > 0) {
    items <- drias_list_remote_loop(file.path(url, items_to_fetch),
                                   nc_pattern = nc_pattern,
                                   method = method,
                                   ignore_pattern = ignore_pattern)
    return(items)
  }
  items <- items[grepl(method, items)]
  return(file.path(url, unlist(items)))
}

#' @param with_variables [logical], if `TRUE`, return the same output as [drias_list_remote]
#' @param safran [logical], if `TRUE`, add a row for SAFRAN database
#' @rdname drias_list
#' @export
drias_list <- function(cfg = loadConfig(), with_variables = FALSE, safran = TRUE) {
  df <- readr::read_tsv(file.path(system.file("extdata", package = "airGRccia"),
                                  "drias_scenarios_variables.tsv"),
                        show_col_types = FALSE)
  df <- df |> filter(!grepl("_OLD", file))
  df$ETP <- grep_categorize(df$file, c(FAO = "_FAO", Hg0175 = "_Hg0175"))
  df$BC <- grep_categorize(df$file, c(ADAMONT = "ADAMONT", CDFt = "CDFt"))
  if (with_variables) {
    return(df)
  }
  df <- df %>% group_by(GCM, RCM, scenario, BC) %>%
    summarise() %>%
    mutate(gcm_rcm = sprintf("%s_%s", GCM, RCM),
           file = sprintf("%s_%s_%s_%s.nc", GCM, RCM, BC, scenario))
  if (!is.null(cfg$climato$drias$excluded_files)) {
    df <- df %>% filter(!file %in% cfg$climato$drias$excluded_files)
  }
  if (safran) {
    df <- rbind(df,
                data.frame(GCM = "safran",
                           RCM = "safran",
                           scenario = "historical",
                           gcm_rcm = "safran",
                           file = "safran_historical.nc"))
  }
  return(df)
}

grep_categorize <- function(x, patterns, none = as.character(NA)) {
  out <- rep(none, length(as.vector(x)))
  for(p in names(patterns)) {
    out[grepl(patterns[p], x)] <- p
  }
  return(out)
}
