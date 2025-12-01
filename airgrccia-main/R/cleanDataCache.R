#' Clean the local cache of cloud data used with [getDataPath]
#'
#' @details
#' Raises an error if the deleting failed.
#'
#' @inheritParams getDataPath
#'
#' @return This function is only used for side effect.
#' @export
cleanDataCache <- function(cache = Sys.getenv("PKG_DATA_CACHE",
                                              file.path(dirname(tempdir()),
                                                        utils::packageName()))) {
  r <- unlink(cache, recursive = TRUE, force = TRUE)
  if (r != 0) stop("Clean data cache failed in folder: ", cache)
  invisible()
}
