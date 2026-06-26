test_that("calc_population_count_ghsl works", {
  .clear_resources()
  outdir <- file.path(tempdir(), "mapme.data")
  dir.create(file.path(outdir, "ghsl"), showWarnings = FALSE, recursive = TRUE)
  mapme_options(outdir = outdir, verbose = FALSE)

  # Create a small dummy raster
  tif_name <- "GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.tif"
  tif_path <- file.path(outdir, "ghsl", tif_name)
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1, crs = "EPSG:4326")
  r[] <- 1:100
  terra::writeRaster(r, tif_path, overwrite = TRUE)

  aoi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(r)))
  
  # Register and get resources
  # get_ghsl already registers the resource
  # We need to manually add it to avail_resources for the test since we are mocking
  fps <- make_footprints(tif_path, what = "raster")
  .add_resource(list(ghsl = fps))

  calc <- calc_population_count_ghsl(stats = "sum")
  # Manually prep resources as calc_indicators would
  ghsl_resource <- prep_resources(aoi, resources = "ghsl")[["ghsl"]]
  
  result <- calc(aoi, ghsl = ghsl_resource)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$variable, "population_sum")
  expect_equal(result$value, sum(1:100))
  expect_equal(format(result$datetime, "%Y"), "2020")
})
