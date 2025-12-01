#' Read climate data from NetCDF files
#'
#' Read and concatenate historical and RCP climate netCDF files provided by
#' `drias_crop_agg_save`.
#'
#' @inheritParams drias_crop_agg_save
#' @param RCP The chosen RCP (either "rcp26", "rcp45", or "rcp85")
#'
#' @return A stars object
#' @export
#'
climate_read <- function(GCM, RCM, BC, RCP, path = getwd()) {
  stopifnot(RCP %in% paste0("rcp", c(26, 45, 85)))
  base_filename <- file.path(path, paste(GCM, RCM, BC, sep = "_"))
  s_historical <- stars::read_mdim(sprintf(
    "%s_%s.nc",
    base_filename,
    "historical"
  ))
  s_rcp <- stars::read_mdim(sprintf("%s_%s.nc", base_filename, RCP))
  return(c(s_historical, s_rcp))
}


#' Check that climate scenario netCDF files exist
#'
#' @inheritParams climate_read
#'
#' @return A [logical] with an attribute "details" specifying which files are missing
#' @export
#'
climate_is_available <- function(GCM, RCM, BC, RCP, path = getwd()) {
  base_filename <- file.path(path, paste(GCM, RCM, BC, sep = "_"))
  l <- lapply(setNames(nm = c("historical", RCP)), function(x) {
    filepath <- sprintf("%s_%s.nc", base_filename, x)
    list(file = filepath, exists = file.exists(filepath))
  })
  res <- all(sapply(l, "[[", "exists"))
  attr(res, "details") <- l
  return(res)
}
