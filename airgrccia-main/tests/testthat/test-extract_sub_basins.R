test_that("extract_sub_basins works", {
  expect_s3_class(sb, "sf")
  expect_true(all(sf::st_geometry_type(sb) == "POLYGON"))
  expect_true(all(sf::st_area(sb) > units::set_units(1E6, "m^2")))
})
