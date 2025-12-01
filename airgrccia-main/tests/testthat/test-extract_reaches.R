# Read D8 flow direction raster and convert it to matrix
d8fd <- terra::rast(attr(sb, "files")["d8"])
a_d8 <- as.matrix(d8fd, wide = TRUE)
max_buffer <- 3

file_reaches_buffer <- getCachePath(
  outlets,
  "test-extract_reaches_buffer",
  "RDS"
)
if (!file.exists(file_reaches_buffer)) {
  raw_reaches <- init_reaches_raster(d8fd, streams, max_buffer)
  saveRDS(raw_reaches, file_reaches_buffer)
} else {
  raw_reaches <- readRDS(file_reaches_buffer)
}

test_that("plot_local_stream works", {
  rc = 12
  cc = 498
  p <- plot_local_stream(a_d8, raw_reaches, rc, cc)
  expect_true(min(p$data$x) >= attr(raw_reaches, "ext")["xmin"])
  expect_true(max(p$data$x) <= attr(raw_reaches, "ext")["xmax"])
  expect_true(min(p$data$y) >= attr(raw_reaches, "ext")["ymin"])
  expect_true(max(p$data$y) <= attr(raw_reaches, "ext")["ymax"])
})

test_that("extract_reaches works", {
  files_sb <- attr(sb, "files")
  reaches <- extract_reaches(
    sb,
    d8fd = files_sb["d8"],
    file_streams
  )

  expect_s3_class(reaches, "sf")
  expect_true(all(sf::st_geometry_type(reaches) == "LINESTRING"))
  expect_equal(nrow(reaches), nrow(outlets) - 1)

  # Check that the reaches are within the bounding box
  bbox_sf <- sf::st_as_sfc(sf::st_bbox(bbox, crs = 2154))
  expect_true(all(sf::st_intersects(reaches, bbox_sf, sparse = FALSE)))
})
