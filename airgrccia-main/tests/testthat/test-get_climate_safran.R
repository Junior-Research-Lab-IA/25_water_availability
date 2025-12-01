test_that("get_climate_safran works", {
  date_start <- "2019-01-01"
  date_end <- "2021-01-01"
  s_extent <- get_climate_safran(
    sb,
    date_start = date_start,
    date_end = date_end,
    overlay_mode = "extent",
    overwrite = TRUE
  )
  expect_s3_class(s_extent, "stars")
  expect_equal(stars::st_get_dimension_values(s_extent, "time"), seq(
    as.Date(date_start),
    as.Date(date_end),
    by = "day"
  ))
  s_extent_cached <- get_climate_safran(
    sb,
    date_start = date_start,
    date_end = date_end,
    overlay_mode = "extent"
  )
  expect_equal(s_extent_cached, s_extent)
  s_aggregate <- suppressWarnings(
    get_climate_safran(
      sb,
      date_start = date_start,
      date_end = date_end,
      overlay_mode = "aggregate",
      overwrite = TRUE
    )
  )
  expect_s3_class(s_aggregate, "stars")
  expect_equal(sort(names(s_aggregate)), c("PotEvap", "Precip", "TempMean"))
  s_aggregate_cached <-
    get_climate_safran(
      sb,
      date_start = date_start,
      date_end = date_end,
      overlay_mode = "aggregate"
    )
  # TODO: Resolve differences between CRS attributes on cached stars
  #expect_equal(s_aggregate_cached, s_aggregate, tolerance=1e-6)
})
