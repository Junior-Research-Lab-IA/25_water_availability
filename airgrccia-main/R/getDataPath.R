#' Give the complete path of the data and cache data on local disk
#'
#' @description
#' It supports Nextcloud public links in case of using cloud. In this case,
#' this function can download the file in a temporary
#' folder and returns the path of the downloaded file.
#' This functionality is useful for `read_excel` from the package *readxl*
#' (See: https://stackoverflow.com/a/41368947/5300212).
#'
#' @param path [character] representing the path of the data in the database
#' @param ... [character] path elements to add after `path`
#' @param use_cache [logical] download the data and use cache location
#' @param root [character] URL or path of the database root
#' @param cache [character] path where downloaded files from cloud are cached (See details)
#' @param cfg a config object. Configuration to use. See [loadConfig] for details
#'
#' @details
#' The cache directory is by default located in the system temporary folder in a
#' subdirectory named as the package name or the value of the environment variable
#' "PKG_DATA_CACHE" if it is defined.
#'
#' @return [character] with the path of the file to read either on:
#' - the local storage if `cfg$data$mode == "local"`
#' - the cache directory if `cfg$data$mode != "local"` and `use_cache == TRUE`
#' - the cloud URL if `cfg$data$mode != "local"` and `use_cache == FALSE`
#' @export
#'
getDataPath <- function(path,
                        ...,
                        use_cache = TRUE,
                        cfg = loadConfig(),
                        root = cfg$data[[cfg$data$mode]],
                        cache = Sys.getenv("PKG_DATA_CACHE", file.path(dirname(tempdir()), utils::packageName()))) {

  path <- file.path(path, ...)
  stopifnot(is.character(path), length(path) == 1)

  if (cfg$data$mode != "local") {
    file <- basename(path)
    ext <- tools::file_ext(file)

    folder <- dirname(path)
    folder <- ifelse(folder == ".", "/", folder)
    # model of link on Nextcloud https://nextcloud.inrae.fr/s/QbwR7yCo2HSDCb9/download?path=hydrometrie&files=Hydrometrie_site.csv
    query <- file.path(root, "download")
    query <- urltools::param_set(query,
                                 key = "path",
                                 value = urltools::url_encode(folder))
    query <- urltools::param_set(query,
                                 key = "files",
                                 value = urltools::url_encode(file))

    if (use_cache) {
      exdir <- cache
      if (folder != "/") {
        exdir <- file.path(exdir, folder)
      }
      dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
      path <- file.path(exdir, file)
      if (!file.exists(path)) {
        message("Download from URL: ", query)
        resp <- httr::GET(query, httr::write_disk(path))
        if (!identical(httr::status_code(resp), 200L)) {
          stop("Error ", httr::status_code(resp), " on query ", query)
        }
        if (ext == "") {
          # Folder in ZIP format
          zipfile <- paste(path, "zip", sep = ".")
          file.rename(path, zipfile)
          utils::unzip(zipfile, exdir = exdir)
          unlink(zipfile)
        }
      }
      return(path)
    } else {
      return(query)
    }
  } else {
    return(file.path(root, path))
  }
}
