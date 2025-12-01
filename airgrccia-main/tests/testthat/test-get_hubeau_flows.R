test_that("get_hubeau_flows_code_station works", {
  expect_equal(get_hubeau_flows_code_station("H010001001"), "H010001001")
  expect_equal(get_hubeau_flows_code_station("H0100010"), "H010001001")
  expect_equal(
    get_hubeau_flows_code_station("H5071010"),
    c("H507101001", "H507101002", "H507101003")
  )
})

test_that("get_hubeau_flows works for long time series", {
  Q <- get_hubeau_flows(
    x = "H0100010",
    date_start = "1959-08-01",
    date_end = "2024-07-31",
    overwrite = TRUE
  )
  expect_s3_class(Q, "data.frame")
  expect_true(nrow(Q) > 20000)
  expect_equal(names(Q), c("DatesR", "H010001001"))
  expect_equal(
    attr(Q, "codes_entite_station"),
    data.frame(code_entite = "H0100010", code_station = "H010001001")
  )
})

test_that("get_hubeau_flows uses cache", {
  expect_message(
    get_hubeau_flows(
      x = "H0100010",
      date_start = "1959-08-01",
      date_end = "2024-07-31"
    ),
    regexp = "from cache"
  )
})

test_that("get_hubeau_flows should raise warning on non existing entities", {
  warnings <- testthat::capture_warnings(
    get_hubeau_flows(
      x = c("H5071010", "fake_station", "H010001001"),
      date_start = "2024-01-01",
      date_end = "2024-07-31"
    )
  )

  expect_match(
    warnings,
    "No station found for code_entite: fake_station|No observation found for code_entite (H507101001|H507101003|fake_station)"
  )
})

test_that("get_hubeau_flows should handle duplicated stations without warnings", {
  expect_no_warning(get_hubeau_flows(
    x = rep("H010001001", 2),
    date_start = "1980-01-01",
    date_end = "1980-07-31"
  ))
})

test_that("get_hubeau flows should handle virtual stations", {
  Q <- suppressWarnings(
    get_hubeau_flows(
      "H0800010",
      date_start = "2024-01-01",
      date_end = "2024-12-31",
      overwrite = TRUE
    )
  )
  expect_equal(nrow(Q), 366)
  expect_equal(names(Q), c("DatesR", "H0800010"))
  expect_warning(get_hubeau_flows(
    "H0800010",
    date_start = "2024-01-01",
    date_end = "2024-12-31"
  ))
})
