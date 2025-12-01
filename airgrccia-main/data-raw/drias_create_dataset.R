# DRIAS / Explore2 climate dataset for the Aude basin
# Creation and storage on Nextcloud
# Already created files are not processed neither overwritten

# For partial operation select lines in "drias_GCM_RCM_scenario_variables.tsv"
selection <- NULL

#Remove messages from readr on column type guessing
options(readr.show_col_types = FALSE)

library(tidyverse)
pkgload::load_all()
cfg <- loadConfig()

dir.create("logs", showWarnings = FALSE)

drias <- readr::read_tsv(getDataPath(cfg$climato$path,
                                     "drias_GCM_RCM_scenario_variables.tsv",
                                     cfg = cfg))
projections <- drias %>% group_by(GCM, RCM, scenario) %>% summarise()

if (!is.null(selection)) projections <- projections[selection, ]

drias_crop_agg_save <- function(GCM, RCM, scenario, cfg) {
  stopifnot(cfg$data$write_results)
  logger::log_appender(logger::appender_file("logs/drias_create_dataset.log"))
  logger::log_info("Starting job: ", paste(GCM, RCM, scenario, sep = ", "))
  filename <- paste0(
    paste(GCM, RCM, scenario, sep = "_"),
    ".nc"
  )
  filepath <- getDataPath(cfg$climato$data$path,
                          filename)
  n_try <- 100
  nc <- FALSE
  while (!inherits(nc, "stars") & n_try > 0) {
    nc <- tryCatch(
      {
        logger::log_info("Aggregating ", paste(GCM, RCM, scenario, sep = ", "), " (remaining ", n_try, " tries)")
        drias_crop_aggregate(GCM, RCM, scenario, cfg = cfg)
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

future::plan(future::multisession, workers = 4)
future.apply::future_mapply(
  drias_crop_agg_save,
  GCM = projections$GCM,
  RCM = projections$RCM,
  scenario = projections$scenario,
  MoreArgs = list(cfg = cfg)
) %>% invisible()
