

soil_awc_c <- get_soil_awc(
    sf = communes_sf,
    source = "BDGSF",
    overlay_mode = "aggregate",
    cache_dir = file.path(
        Sys.getenv("RADIS_CACHE_DIR",
                   file.path(dirname(tempdir()), "RADIS")),
        "data-raw/soil_awc/BDGSF"   # <— fixed path
    ),
    force_download = FALSE
)
