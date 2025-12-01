#' Read package configuration
#'
#' Read default package configuration and complete it with eventual user config.
#'
#' @details
#' User file configuration (by default the `config.yml` file in the working
#' directory) can overwrite items of the default configuration (See examples).
#'
#' @param userFile location of the user config YML file (Use NULL or empty character to ignore)
#' @param pathDefaultCfg The location of the default configuration (located in "inst/config.yml" of the package by default)
#' @param ... Other parameters than `file` sent to [config::get] such as `config` or `value`
#'
#' @return A configuration as it is returned by [config::get]
#' @export
#'
loadConfig <- function(userFile = sprintf("config_%s.yml", utils::packageName()),
                       pathDefaultCfg = system.file("config.yml", package = utils::packageName()),
                       ...) {
  cfg <- config::get(file = pathDefaultCfg, ...)
  if (!is.null(userFile) && userFile != "") {
    if (userFile == basename(userFile)) {
      # Search in package sub folders
      pkg_path <- tryCatch({
        pkgload::pkg_path()
      },
      error = function(cond) {
        warning("Project directory not found up to ", getwd())
        getwd()
      })
      current_path <- getwd()
      repeat {
        userFile <- file.path(current_path, basename(userFile))
        if (file.exists(userFile) || current_path == pkg_path)
          break
        current_path <- dirname(current_path)
      }
    }
    if (file.exists(userFile)) {
      message("Read user configuration from: ", userFile)
      cfg = config::merge(cfg, config::get(file = userFile, ...))
    }
  }
  stopifnot(cfg$data$mode %in% c("local", "remote"),
            is.logical(cfg$data$write_results))
  if (cfg$data$write_results && cfg$data$mode == "remote") {
    warning("cfg$data$write_results is forced to `FALSE` in 'remote' mode")
    cfg$data$write_results <- FALSE
  }
  cfg
}
