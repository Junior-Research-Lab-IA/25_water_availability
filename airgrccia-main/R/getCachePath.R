#' Get cache path
#'
#' Build a path in a cache directory and a file name build from MD5 hash.
#'
#' @return The cache path.
#'
#' @name getCachePath
#' @rdname getCachePath
#' @examples
#' getCacheDir()
#' path_cars <- getCachePath(cars, fileext = "rds")
#' path_cars
#' \dontrun{
#' saveRDS(cars, path_cars)
#' }
NULL


#' @describeIn getCachePath Returns cache path root and create the folder if it
#' does not exist.
#'
#' @param pkg Package name (default current package)
#' @param basepath Folder in which to add cache directory (default temp)
#' @param pattern Pattern applied on package name with [sprintf]
#'
#' @export
getCacheDir <- function(
  pkg = utils::packageName(),
  basepath = Sys.getenv("CACHE_DIR_AIRGRCCIA", dirname(tempdir())),
  pattern = "%s_cache"
) {
  p <- file.path(basepath, sprintf(pattern, pkg))
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
  return(p)
}

#' @describeIn getCachePath Returns cache path for a specific file.
#' @param x Object to save (or property of object that can be used as a signature)
#' @param prefix Optional prefix to add to the file name (default variable name)
#' @param fileext Optional file extension to use for the file (e.g., "rds", "csv", etc.)
#' @param cache_dir Path to the cache directory (default from [getCacheDir()])
#' @export
getCachePath <- function(
  x,
  prefix = as.character(substitute(x)),
  fileext = NULL,
  cache_dir = getCacheDir()
) {
  path <- file.path(cache_dir, paste(c(prefix, xfun::md5(x)), collapse = "_"))
  if (!is.null(fileext)) {
    path <- sprintf("%s.%s", path, fileext)
  }
  return(path)
}

#' @describeIn getCachePath Clear the cache directory
#' @export
clearCache <- function(cache_dir = getCacheDir()) {
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("Cache cleared: ", cache_dir)
  } else {
    message("Cache directory does not exist: ", cache_dir)
  }
}
