test_that("get_firecci51 works", {
  skip_on_cran()

  # Use the included resource for testing
  x <- read_sf(
    system.file("extdata", "gfw_sample.gpkg",
                package = "mapme.biodiversity"
    )
  )

  # Call the function for a specific month/year to keep the test lightweight
  gfc51 <- get_firecci51(years = 2001, months = 1)
  expect_silent(.check_resource_fun(gfc51))

  # Test if the function creates footprints correctly
  fps <- gfc51(x)
  expect_silent(.check_footprints(fps))

  # Ensure that the filenames are set correctly and check the first file
  expect_true(any(grepl("JD.tif$", fps$filename)))
  expect_equal(basename(fps$filename[1]), "20010101-ESACCI-L3S_FIRE-BA-MODIS-AREA_2-fv5.1-CL.tif")

  # Check if the footprints intersect with the AOI
  intersect <- st_intersects(x, fps, sparse = FALSE)
  expect_true(any(intersect))
})

test_that("get_firecci51 handles non-intersecting AOI correctly", {
  skip_on_cran()

  # Define an AOI that does not intersect with any AREA_*
  non_intersecting_aoi <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -170, ymin = -50, xmax = -160, ymax = -40), crs = "EPSG:4326")))

  # Call the function for a specific month/year
  gfc51 <- get_firecci51(years = 2001, months = 1)
  expect_silent(.check_resource_fun(gfc51))

  # Test if the function handles non-intersecting AOI correctly
  expect_error(gfc51(non_intersecting_aoi), "The extent of the portfolio does not intersect with any FireCCI51 areas.")
})
